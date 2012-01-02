#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <map>
#include <string>

#define VERSION                     "0.61"
#define CMD_VERSION                   'v'
#define CMD_APPLY_XSL 'a'



#define PACKET_SIZE 4
#define LEN(buf) (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | buf[3]
#define PUT_INT(i,s)   s[0] = (i>>24) & 0xff;  s[1] = (i>>16) & 0xff;  s[2] =  (i>>8) & 0xff;  s[3] = i & 0xff

#include <libxml/tree.h>
#include <libxslt/xsltInternals.h>
#include <libxslt/transform.h>


using namespace std;
typedef std::map<std::string, xsltStylesheetPtr> templMap;
typedef std::pair<std::string, xsltStylesheetPtr> templPair;

templMap tMap;

int read_exact(char *buf, int len) {
    int i, got = 0;
    do {
        if ((i=read(0, buf+got, len-got)) <= 0)
            return i;
        got += i;
    } while (got < len);

    return len;
}

int write_exact(const char *buf, int len) {

  int i, wrote = 0;

  do {
    if ((i=write(1, buf+wrote, len-wrote)) <= 0)
      return i;
    wrote += i;
  } while (wrote < len);

  return len;
}

int read_cmd(char *buf) {
  if (read_exact(buf, PACKET_SIZE) != PACKET_SIZE)
    return -1;


  return read_exact(buf, LEN(buf));
}

/* as_string == 1 ==> reserva 1 byte mas y devuelve ASCIIZ */

char *read_alloc_cmd(int as_string) {
  char size[PACKET_SIZE];
  char *buf;
  int len;

  if (read_exact(size, PACKET_SIZE) != PACKET_SIZE)
    return NULL;

  len = LEN(size);

  buf = (char *) malloc(len+as_string);
  if (read_exact(buf, len) == len) {
    if (as_string) buf[len] = '\0';
    return buf;
  }
  else {
    free(buf);
    return NULL;
  }
}

int write_cmd(const char *buf, int len) {
  char str[PACKET_SIZE];

  PUT_INT(len, str);
  if (write_exact(str,PACKET_SIZE) != PACKET_SIZE)
    return -1;

  return write_exact(buf, len);
}

void write_int(int x) {
  char r[PACKET_SIZE];

  PUT_INT(x,r);
  write_cmd(r, PACKET_SIZE);
}


void apply_xsl3(){
    char *xslfile       = read_alloc_cmd(1);
    char *input_xml_str = read_alloc_cmd(1);


    write_int(0);    
    write_cmd("JOPA", strlen("JOPA"));
}


void apply_xsl2(){
    int ecode = 0;
    char *xslfile       = read_alloc_cmd(1);
    char *input_xml_str = read_alloc_cmd(1);

/*    xsltStylesheetPtr xsl = NULL;
    if(tMap.find(xslfile) != tMap.end()) {
        xsl = tMap.find(xslfile)->second;
    }
    else {
        xsl = xsltParseStylesheetFile((const xmlChar*) xslfile);
        tMap.insert(templPair(xslfile, xsl));
    }
*/
    xsltStylesheetPtr xsl = xsltParseStylesheetFile((const xmlChar*) xslfile);

    xmlDocPtr doc = xmlParseMemory(input_xml_str, strlen(input_xml_str));
    xmlDocPtr result = xsltApplyStylesheet(xsl, doc, NULL);

    int resSize;
    xmlChar *resBuff;
    xmlDocDumpMemory(result, &resBuff, &resSize);

    write_int(ecode);
    if (ecode) {
        fprintf(stderr, "unknown error\n");
        write_cmd("JOPA!", strlen("JOPA!"));
    }
    else {
        write_cmd((const char*) resBuff,strlen((const char*) resBuff));
    }
////////////

//    xmlSaveFile("stylesheet_output.xml", result);

    xmlFree(resBuff);
    xmlFreeDoc(result);
    xmlFreeDoc(doc);
    xsltFreeStylesheet(xsl);
}



int main(int argc, char **argv) {

    char buffer[PACKET_SIZE];

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
