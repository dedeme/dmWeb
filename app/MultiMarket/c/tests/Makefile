PRG = MultiMarket_tests
LIB = dmc

LIB_PATH = ../lib/lib$(LIB)
LIB_INCLUDE = ../lib/lib$(LIB)/include

CFLAGS = -Wall -rdynamic
FILES = $(wildcard src/*.c) $(wildcard include/*.h)
bin/$(PRG): $(FILES) ../.mkaux/libMkAux.a $(LIB_PATH)/lib$(LIB).a
	gcc $(CFLAGS) $(FILES) -o bin/$(PRG) \
	  -Iinclude -I../include -I$(LIB_INCLUDE) \
	  -L../.mkaux -lMkAux \
		-L$(LIB_PATH) -l$(LIB) \
		-lgc -lm
