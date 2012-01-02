#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
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


typedef std::map<std::string, xsltStylesheetPtr> templMap;
typedef std::pair<std::string, xsltStylesheetPtr> templPair;

templMap tMap;

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
    unsigned char *buf;
    int len;

    if (read_exact(size, PACKET_SIZE) != PACKET_SIZE)
        return NULL;

    len = LEN(size);

    buf = (unsigned char *) malloc(len+as_string);
    if (read_exact(buf, len) == len) {
        if (as_string) 
            buf[len] = '\0';
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


void apply_xsl3() {
    int ecode = 0;
    unsigned char *xslfile       = read_alloc_cmd(1);
    char *input_xml_str = (char *)read_alloc_cmd(1);
                
    xsltStylesheetPtr xsl = NULL;
    if(tMap.find((const char*)xslfile) != tMap.end()) {
        xsl = tMap.find((const char*)xslfile)->second;
    }
    else {
        xsl = xsltParseStylesheetFile((const xmlChar*) xslfile);
        tMap.insert(templPair((const char*)xslfile, xsl));
    }

//    xsltStylesheetPtr xsl = xsltParseStylesheetFile((const xmlChar*) xslfile);
    xmlDocPtr doc = xmlParseMemory((const char *)input_xml_str, strlen(input_xml_str));
    xmlDocPtr result = xsltApplyStylesheet(xsl, doc, NULL);

    int resSize;
    xmlChar *resBuff;
    xmlDocDumpMemory(result, &resBuff, &resSize);

    write_int(ecode);
    if (ecode) {
        fprintf(stderr, "unknown error\n");
        write_cmd((const unsigned char *)"JOPA!", strlen("JOPA!"));
    }
    else {
        write_cmd((const unsigned char*) resBuff,strlen((const char*)resBuff));
    }   

    //    xmlSaveFile("stylesheet_output.xml", result);
    xmlFree(resBuff);
    xmlFreeDoc(result);
    xmlFreeDoc(doc);
//    xsltFreeStylesheet(xsl);
}
/*
void apply_xsl2(){
    int ecode = 0;
    unsigned char *xslfile       = read_alloc_cmd(1);
    unsigned char *input_xml_str = read_alloc_cmd(1);

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

*/

int main(int argc, char **argv) {

    unsigned char buffer[PACKET_SIZE];

    while (1) {
        if (read_cmd(buffer) != 1)
            exit(1);

        switch (*buffer) {

            case CMD_VERSION: 
                write_int(0);
                write_cmd((const unsigned char*)VERSION, strlen(VERSION));
                break;

            case CMD_APPLY_XSL:
                apply_xsl3();
                break;

            default:
                fprintf(stderr, "unknown command %c in sablotron_adapter\n", *buffer);
                exit(1);
        }
    }
}
