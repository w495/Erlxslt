# default


CC=g++
EC=./rebar 
EFLAGS=compile

ESRC_DIR=src
EONJ_DIR=ebin

CSRC_DIR=c_src
CONJ_DIR=ebin

CSRC=$(CSRC_DIR)/libxslt_adapter.c
COBJ=$(CONJ_DIR)/libxslt_adapter
CFLAGS=-lxml2 -lxslt
CINCLUDES=/usr/include/libxml2

all: c_bin
	$(EC) $(EFLAGS)

c_bin:
	$(CC) -o $(COBJ) $(CSRC) -I$(CINCLUDES) $(CFLAGS)

cleanall: clean
	rm -rf $(CONJ_DIR)/*

clean:
	rm -rf $(EONJ_DIR)/*

