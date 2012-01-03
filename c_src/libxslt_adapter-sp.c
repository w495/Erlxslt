#include <stdio.h>
#include <string.h>
#include <stdlib.h>
//#include <sablot.h>

#define VERSION                     "0.61"
#define CMD_VERSION                   'v'
#define CMD_APPLY_XSL 'a'



#define PACKET_SIZE 4
#define LEN(buf) (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | buf[3]
#define PUT_INT(i,s)   s[0] = (i>>24) & 0xff;  s[1] = (i>>16) & 0xff;  s[2] =  (i>>8) & 0xff;  s[3] = i & 0xff

#include <libxml/tree.h>
#include <libxslt/xsltInternals.h>
#include <libxslt/transform.h>


int read_exact(unsigned char *buf, int len) {
    int i, got = 0;
    do {
        if ((i=read(0, buf+got, len-got)) <= 0)
            return i;
        got += i;
    } while (got < len);

    return len;
}

int write_exact(const unsigned char *buf, int len) {

  int i, wrote = 0;

  do {
    if ((i=write(1, buf+wrote, len-wrote)) <= 0)
      return i;
    wrote += i;
  } while (wrote < len);

  return len;
}

int read_cmd(unsigned char *buf) {
  if (read_exact(buf, PACKET_SIZE) != PACKET_SIZE)
    return -1;


  return read_exact(buf, LEN(buf));
}

/* as_string == 1 ==> reserva 1 byte mas y devuelve ASCIIZ */

unsigned char *read_alloc_cmd(int as_string) {
  unsigned char size[PACKET_SIZE];
  char *buf;
  int len;

  if (read_exact(size, PACKET_SIZE) != PACKET_SIZE)
    return NULL;

  len = LEN(size);

  buf = (unsigned char *) malloc(len+as_string);
  if (read_exact(buf, len) == len) {
    if (as_string) buf[len] = '\0';
    return buf;
  }
  else {
    free(buf);
    return NULL;
  }
}

int write_cmd(const unsigned char *buf, int len) {
  unsigned char str[PACKET_SIZE];

  PUT_INT(len, str);
  if (write_exact(str,PACKET_SIZE) != PACKET_SIZE)
    return -1;

  return write_exact(buf, len);
}

void write_int(int x) {
  unsigned char r[PACKET_SIZE];

  PUT_INT(x,r);
  write_cmd(r, PACKET_SIZE);
}
///////////////////////////////////////////////////////////////////



/////////////////////////////////////////////////////////////////////
// ecode workaround
/////////////////////////////////////////////////////////////////////

static int theEcode;
static char error_msg[1024];

#define GetErrorMsg(p) error_msg

/*static unsigned long makeCodeWAround(void *userData, 
                                     SablotHandle processor_, 
                                     int severity, 
                                     unsigned short facility, 
                                     unsigned short code) {
  if (severity) 
    theEcode = code;
  return code;
}
*/
/*
static unsigned long logWAround(void *userData, 
                                SablotHandle processor_, 
                                MH_ERROR code, 
                                MH_LEVEL level, 
                                char **fields) {}


static unsigned long errorWAround(void *userData, 
                                  SablotHandle processor_,  
                                  MH_ERROR code, 
                                  MH_LEVEL level, 
                                  char **fields) {
  if (code) {
    char **f = fields+3;

    strcpy(error_msg, *f);

    for (f++; *f != NULL; f++)
      { strcat(error_msg, ", "); strcat(error_msg, *f); }
  }
}
*/
//MessageHandler errorHandler = {logWAround, errorWAround };
/*
int CreateProcessor(SablotHandle *processorPtr) {

  SablotCreateProcessor(processorPtr);
  SablotRegHandler(*processorPtr, HLR_MESSAGE, &errorHandler, NULL);  
}*/

//////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
//// Command implementation
//////////////////////////////////////////////////////////////////////
/*
void apply_xsl(SablotHandle *processor) {
    int ecode;
    char *xslfile       = read_alloc_cmd(1);
    char *input_xml_str = read_alloc_cmd(1);

    SablotSituation S;
    SablotHandle proc;
    SDOM_Document xsl, xml;

    SablotCreateSituation(&S);
    SablotParseStylesheet(S, xslfile, &xsl);
    SablotParseBuffer(S, input_xml_str, &xml);

    SablotCreateProcessorForSituation(S, &proc);
    SablotAddArgTree(S, proc, "sheet", xsl);
    SablotAddArgTree(S, proc, "data", xml);

    ecode = SablotRunProcessorGen(S, proc, "arg:/sheet", "arg:/data", "arg:/out");

    char * result;
    SablotGetResultArg(proc, "arg:/out", &result);

    write_int(ecode);
    if (ecode)
        write_cmd(GetErrorMsg(*processor), strlen(GetErrorMsg(*processor)));
    else
        write_cmd(result,strlen(result));

    SablotFree(result);
    SablotDestroyDocument(S, xsl);
    SablotDestroyDocument(S, xml);
    SablotDestroyProcessor(proc);
    SablotDestroySituation(S);
}
*/

//////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
//// Initialization
//////////////////////////////////////////////////////////////////////
void apply_xsl2(){
    int ecode = 0;
    char *xslfile       = read_alloc_cmd(1);
    char *input_xml_str = read_alloc_cmd(1);

    xsltStylesheetPtr xsl = xsltParseStylesheetFile(xslfile);
    xmlDocPtr doc = xmlParseMemory(input_xml_str, strlen(input_xml_str));
    xmlDocPtr result = xsltApplyStylesheet(xsl, doc, NULL);

    int resSize;
    xmlChar *resBuff;
    xmlDocDumpMemory(result, &resBuff, &resSize);

    write_int(ecode);
    if (ecode)
        fprintf(stderr, "unknown error\n");
        //write_cmd(GetErrorMsg(*processor), strlen(GetErrorMsg(*processor)));
    else
        write_cmd(resBuff,strlen(resBuff));
////////////

//    xmlSaveFile("stylesheet_output.xml", result);

    xmlFree(resBuff);
    xmlFreeDoc(doc);
    xmlFreeDoc(result);
    xsltFreeStylesheet(xsl);
//    return 0;
}



int main(int argc, char **argv) {

//    SablotHandle processor;
    unsigned char buffer[PACKET_SIZE];

#ifdef DEBUG
    fprintf(stderr, "sablotron_adapter v%s running...\n", VERSION);
#endif

//    CreateProcessor(&processor);

    while (1) {
        if (read_cmd(buffer) != 1)
            exit(1);

        switch (*buffer) {

            case CMD_VERSION: 
                write_int(0);
                write_cmd(VERSION, strlen(VERSION));
                break;

            case CMD_APPLY_XSL:
                apply_xsl2();
                break;

            default:
                fprintf(stderr, "unknown command %c in sablotron_adapter\n", *buffer);
                //SablotDestroyProcessor(processor);
                exit(1);
        }
    }
}
