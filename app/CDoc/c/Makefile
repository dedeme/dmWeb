LIB_PATH_dmc = lib/libdmc
LIB_INCLUDE_dmc = $(LIB_PATH_dmc)/include

LIB_PATH_tx = lib/libtx
LIB_INCLUDE_tx = $(LIB_PATH_tx)/include

CFLAGS = -Wall -rdynamic
bin/CDoc : src/CDoc.c include/CDoc.h lib/liblocalcompilation/liblocalcompilation.a
	gcc $(CFLAGS) src/CDoc.c -o bin/CDoc \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc) \
		-I$(LIB_INCLUDE_tx) \
		-Llib/liblocalcompilation -llocalcompilation \
		-L$(LIB_PATH_dmc) -ldmc \
		-L$(LIB_PATH_tx) -ltx \
		-lgc -lpthread -lm

lib/liblocalcompilation/liblocalcompilation.a : lib/liblocalcompilation/obj/CDoc.o lib/liblocalcompilation/obj/pages/main.o lib/liblocalcompilation/obj/io/confdb.o lib/liblocalcompilation/obj/data/Conf.o lib/liblocalcompilation/obj/io/dpathsdb.o lib/liblocalcompilation/obj/data/Dpath.o lib/liblocalcompilation/obj/pages/paths.o lib/liblocalcompilation/obj/pages/chpass.o lib/liblocalcompilation/obj/pages/index.o lib/liblocalcompilation/obj/io/io.o lib/liblocalcompilation/obj/io/indexrd.o lib/liblocalcompilation/obj/io/modulerd.o lib/liblocalcompilation/obj/utx.o lib/liblocalcompilation/obj/data/Doc.o lib/liblocalcompilation/obj/data/IndexTree.o lib/liblocalcompilation/obj/pages/module.o lib/liblocalcompilation/obj/pages/code.o
	ar rcs lib/liblocalcompilation/liblocalcompilation.a lib/liblocalcompilation/obj/CDoc.o lib/liblocalcompilation/obj/pages/main.o lib/liblocalcompilation/obj/io/confdb.o lib/liblocalcompilation/obj/data/Conf.o lib/liblocalcompilation/obj/io/dpathsdb.o lib/liblocalcompilation/obj/data/Dpath.o lib/liblocalcompilation/obj/pages/paths.o lib/liblocalcompilation/obj/pages/chpass.o lib/liblocalcompilation/obj/pages/index.o lib/liblocalcompilation/obj/io/io.o lib/liblocalcompilation/obj/io/indexrd.o lib/liblocalcompilation/obj/io/modulerd.o lib/liblocalcompilation/obj/utx.o lib/liblocalcompilation/obj/data/Doc.o lib/liblocalcompilation/obj/data/IndexTree.o lib/liblocalcompilation/obj/pages/module.o lib/liblocalcompilation/obj/pages/code.o

lib/liblocalcompilation/obj/CDoc.o : include/CDoc.h src/CDoc.c include/pages/main.h src/pages/main.c include/pages/paths.h src/pages/paths.c include/pages/chpass.h src/pages/chpass.c include/pages/index.h src/pages/index.c include/pages/module.h src/pages/module.c include/pages/code.h src/pages/code.c
	gcc $(CFLAGS) -c src/CDoc.c -o lib/liblocalcompilation/obj/CDoc.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc) \
		-I$(LIB_INCLUDE_tx)

lib/liblocalcompilation/obj/pages/main.o : include/pages/main.h src/pages/main.c include/io/confdb.h src/io/confdb.c include/io/dpathsdb.h src/io/dpathsdb.c
	gcc $(CFLAGS) -c src/pages/main.c -o lib/liblocalcompilation/obj/pages/main.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc) \
		-I$(LIB_INCLUDE_tx)

lib/liblocalcompilation/obj/io/confdb.o : include/io/confdb.h src/io/confdb.c include/data/Conf.h src/data/Conf.c
	gcc $(CFLAGS) -c src/io/confdb.c -o lib/liblocalcompilation/obj/io/confdb.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc) \
		-I$(LIB_INCLUDE_tx)

lib/liblocalcompilation/obj/data/Conf.o : include/data/Conf.h src/data/Conf.c
	gcc $(CFLAGS) -c src/data/Conf.c -o lib/liblocalcompilation/obj/data/Conf.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc) \
		-I$(LIB_INCLUDE_tx)

lib/liblocalcompilation/obj/io/dpathsdb.o : include/io/dpathsdb.h src/io/dpathsdb.c include/data/Dpath.h src/data/Dpath.c
	gcc $(CFLAGS) -c src/io/dpathsdb.c -o lib/liblocalcompilation/obj/io/dpathsdb.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc) \
		-I$(LIB_INCLUDE_tx)

lib/liblocalcompilation/obj/data/Dpath.o : include/data/Dpath.h src/data/Dpath.c
	gcc $(CFLAGS) -c src/data/Dpath.c -o lib/liblocalcompilation/obj/data/Dpath.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc) \
		-I$(LIB_INCLUDE_tx)

lib/liblocalcompilation/obj/pages/paths.o : include/pages/paths.h src/pages/paths.c include/io/confdb.h src/io/confdb.c include/io/dpathsdb.h src/io/dpathsdb.c
	gcc $(CFLAGS) -c src/pages/paths.c -o lib/liblocalcompilation/obj/pages/paths.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc) \
		-I$(LIB_INCLUDE_tx)

lib/liblocalcompilation/obj/pages/chpass.o : include/pages/chpass.h src/pages/chpass.c
	gcc $(CFLAGS) -c src/pages/chpass.c -o lib/liblocalcompilation/obj/pages/chpass.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc) \
		-I$(LIB_INCLUDE_tx)

lib/liblocalcompilation/obj/pages/index.o : include/pages/index.h src/pages/index.c include/io/dpathsdb.h src/io/dpathsdb.c include/io/io.h src/io/io.c
	gcc $(CFLAGS) -c src/pages/index.c -o lib/liblocalcompilation/obj/pages/index.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc) \
		-I$(LIB_INCLUDE_tx)

lib/liblocalcompilation/obj/io/io.o : include/io/io.h src/io/io.c include/io/indexrd.h src/io/indexrd.c include/io/modulerd.h src/io/modulerd.c include/data/IndexTree.h src/data/IndexTree.c include/data/Doc.h src/data/Doc.c
	gcc $(CFLAGS) -c src/io/io.c -o lib/liblocalcompilation/obj/io/io.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc) \
		-I$(LIB_INCLUDE_tx)

lib/liblocalcompilation/obj/io/indexrd.o : include/io/indexrd.h src/io/indexrd.c
	gcc $(CFLAGS) -c src/io/indexrd.c -o lib/liblocalcompilation/obj/io/indexrd.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc) \
		-I$(LIB_INCLUDE_tx)

lib/liblocalcompilation/obj/io/modulerd.o : include/io/modulerd.h src/io/modulerd.c include/utx.h src/utx.c include/data/Doc.h src/data/Doc.c
	gcc $(CFLAGS) -c src/io/modulerd.c -o lib/liblocalcompilation/obj/io/modulerd.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc) \
		-I$(LIB_INCLUDE_tx)

lib/liblocalcompilation/obj/utx.o : include/utx.h src/utx.c
	gcc $(CFLAGS) -c src/utx.c -o lib/liblocalcompilation/obj/utx.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc) \
		-I$(LIB_INCLUDE_tx)

lib/liblocalcompilation/obj/data/Doc.o : include/data/Doc.h src/data/Doc.c
	gcc $(CFLAGS) -c src/data/Doc.c -o lib/liblocalcompilation/obj/data/Doc.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc) \
		-I$(LIB_INCLUDE_tx)

lib/liblocalcompilation/obj/data/IndexTree.o : include/data/IndexTree.h src/data/IndexTree.c
	gcc $(CFLAGS) -c src/data/IndexTree.c -o lib/liblocalcompilation/obj/data/IndexTree.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc) \
		-I$(LIB_INCLUDE_tx)

lib/liblocalcompilation/obj/pages/module.o : include/pages/module.h src/pages/module.c include/io/dpathsdb.h src/io/dpathsdb.c include/io/io.h src/io/io.c
	gcc $(CFLAGS) -c src/pages/module.c -o lib/liblocalcompilation/obj/pages/module.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc) \
		-I$(LIB_INCLUDE_tx)

lib/liblocalcompilation/obj/pages/code.o : include/pages/code.h src/pages/code.c include/io/dpathsdb.h src/io/dpathsdb.c include/io/io.h src/io/io.c
	gcc $(CFLAGS) -c src/pages/code.c -o lib/liblocalcompilation/obj/pages/code.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc) \
		-I$(LIB_INCLUDE_tx)

