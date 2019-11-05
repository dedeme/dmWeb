LIB_PATH_dmc = lib/libdmc
LIB_INCLUDE_dmc = $(LIB_PATH_dmc)/include

CFLAGS = -Wall -rdynamic
bin/MarketLeague : src/marketleague.c include/marketleague.h lib/liblocalcompilation/liblocalcompilation.a
	gcc $(CFLAGS) src/marketleague.c -o bin/MarketLeague \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc) \
		-Llib/liblocalcompilation -llocalcompilation \
		-L$(LIB_PATH_dmc) -ldmc \
		-lgc -lpthread -lm

lib/liblocalcompilation/liblocalcompilation.a : lib/liblocalcompilation/obj/marketleague.o lib/liblocalcompilation/obj/io.o lib/liblocalcompilation/obj/Match.o lib/liblocalcompilation/obj/Mark.o lib/liblocalcompilation/obj/Data.o lib/liblocalcompilation/obj/DEFS.o
	ar rcs lib/liblocalcompilation/liblocalcompilation.a lib/liblocalcompilation/obj/marketleague.o lib/liblocalcompilation/obj/io.o lib/liblocalcompilation/obj/Match.o lib/liblocalcompilation/obj/Mark.o lib/liblocalcompilation/obj/Data.o lib/liblocalcompilation/obj/DEFS.o

lib/liblocalcompilation/obj/marketleague.o : include/marketleague.h src/marketleague.c include/io.h src/io.c include/Data.h src/Data.c
	gcc $(CFLAGS) -c src/marketleague.c -o lib/liblocalcompilation/obj/marketleague.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/io.o : include/io.h src/io.c include/Match.h src/Match.c include/Mark.h src/Mark.c include/DEFS.h src/DEFS.c include/Data.h src/Data.c
	gcc $(CFLAGS) -c src/io.c -o lib/liblocalcompilation/obj/io.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/Match.o : include/Match.h src/Match.c
	gcc $(CFLAGS) -c src/Match.c -o lib/liblocalcompilation/obj/Match.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/Mark.o : include/Mark.h src/Mark.c include/Match.h src/Match.c include/Data.h src/Data.c
	gcc $(CFLAGS) -c src/Mark.c -o lib/liblocalcompilation/obj/Mark.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/Data.o : include/Data.h src/Data.c
	gcc $(CFLAGS) -c src/Data.c -o lib/liblocalcompilation/obj/Data.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/DEFS.o : include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/DEFS.c -o lib/liblocalcompilation/obj/DEFS.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

