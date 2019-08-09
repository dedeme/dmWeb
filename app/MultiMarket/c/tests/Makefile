LIB_PATH_local = lib/liblocal
LIB_INCLUDE_local = $(LIB_PATH_local)/include

LIB_PATH_dmc = lib/libdmc
LIB_INCLUDE_dmc = $(LIB_PATH_dmc)/include

CFLAGS = -Wall -rdynamic
bin/tests : src/tests.c include/tests.h lib/liblocalcompilation/liblocalcompilation.a
	gcc $(CFLAGS) src/tests.c -o bin/tests \
		-Iinclude \
		-I$(LIB_INCLUDE_local) \
		-I$(LIB_INCLUDE_dmc) \
		-Llib/liblocalcompilation -llocalcompilation \
		-L$(LIB_PATH_local) -llocal \
		-L$(LIB_PATH_dmc) -ldmc \
		-lgc -lpthread -lm

lib/liblocalcompilation/liblocalcompilation.a : lib/liblocalcompilation/obj/tests.o lib/liblocalcompilation/obj/log_tests.o lib/liblocalcompilation/obj/nick_tests.o lib/liblocalcompilation/obj/quote_tests.o lib/liblocalcompilation/obj/quote2_tests.o lib/liblocalcompilation/obj/gen_tests.o lib/liblocalcompilation/obj/rs_tests.o lib/liblocalcompilation/obj/order_tests.o lib/liblocalcompilation/obj/facc_tests.o lib/liblocalcompilation/obj/nickSets_tests.o lib/liblocalcompilation/obj/rank_tests.o
	ar rcs lib/liblocalcompilation/liblocalcompilation.a lib/liblocalcompilation/obj/tests.o lib/liblocalcompilation/obj/log_tests.o lib/liblocalcompilation/obj/nick_tests.o lib/liblocalcompilation/obj/quote_tests.o lib/liblocalcompilation/obj/quote2_tests.o lib/liblocalcompilation/obj/gen_tests.o lib/liblocalcompilation/obj/rs_tests.o lib/liblocalcompilation/obj/order_tests.o lib/liblocalcompilation/obj/facc_tests.o lib/liblocalcompilation/obj/nickSets_tests.o lib/liblocalcompilation/obj/rank_tests.o

lib/liblocalcompilation/obj/tests.o : include/tests.h src/tests.c include/log_tests.h src/log_tests.c include/nick_tests.h src/nick_tests.c include/quote_tests.h src/quote_tests.c include/quote2_tests.h src/quote2_tests.c include/gen_tests.h src/gen_tests.c include/rs_tests.h src/rs_tests.c include/order_tests.h src/order_tests.c include/facc_tests.h src/facc_tests.c include/nickSets_tests.h src/nickSets_tests.c include/rank_tests.h src/rank_tests.c
	gcc $(CFLAGS) -c src/tests.c -o lib/liblocalcompilation/obj/tests.o \
		-Iinclude \
		-I$(LIB_INCLUDE_local) \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/log_tests.o : include/log_tests.h src/log_tests.c
	gcc $(CFLAGS) -c src/log_tests.c -o lib/liblocalcompilation/obj/log_tests.o \
		-Iinclude \
		-I$(LIB_INCLUDE_local) \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/nick_tests.o : include/nick_tests.h src/nick_tests.c
	gcc $(CFLAGS) -c src/nick_tests.c -o lib/liblocalcompilation/obj/nick_tests.o \
		-Iinclude \
		-I$(LIB_INCLUDE_local) \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/quote_tests.o : include/quote_tests.h src/quote_tests.c
	gcc $(CFLAGS) -c src/quote_tests.c -o lib/liblocalcompilation/obj/quote_tests.o \
		-Iinclude \
		-I$(LIB_INCLUDE_local) \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/quote2_tests.o : include/quote2_tests.h src/quote2_tests.c
	gcc $(CFLAGS) -c src/quote2_tests.c -o lib/liblocalcompilation/obj/quote2_tests.o \
		-Iinclude \
		-I$(LIB_INCLUDE_local) \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/gen_tests.o : include/gen_tests.h src/gen_tests.c
	gcc $(CFLAGS) -c src/gen_tests.c -o lib/liblocalcompilation/obj/gen_tests.o \
		-Iinclude \
		-I$(LIB_INCLUDE_local) \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/rs_tests.o : include/rs_tests.h src/rs_tests.c
	gcc $(CFLAGS) -c src/rs_tests.c -o lib/liblocalcompilation/obj/rs_tests.o \
		-Iinclude \
		-I$(LIB_INCLUDE_local) \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/order_tests.o : include/order_tests.h src/order_tests.c
	gcc $(CFLAGS) -c src/order_tests.c -o lib/liblocalcompilation/obj/order_tests.o \
		-Iinclude \
		-I$(LIB_INCLUDE_local) \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/facc_tests.o : include/facc_tests.h src/facc_tests.c
	gcc $(CFLAGS) -c src/facc_tests.c -o lib/liblocalcompilation/obj/facc_tests.o \
		-Iinclude \
		-I$(LIB_INCLUDE_local) \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/nickSets_tests.o : include/nickSets_tests.h src/nickSets_tests.c
	gcc $(CFLAGS) -c src/nickSets_tests.c -o lib/liblocalcompilation/obj/nickSets_tests.o \
		-Iinclude \
		-I$(LIB_INCLUDE_local) \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/rank_tests.o : include/rank_tests.h src/rank_tests.c
	gcc $(CFLAGS) -c src/rank_tests.c -o lib/liblocalcompilation/obj/rank_tests.o \
		-Iinclude \
		-I$(LIB_INCLUDE_local) \
		-I$(LIB_INCLUDE_dmc)

