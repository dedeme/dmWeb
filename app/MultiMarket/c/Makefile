PRG = MultiMarket

LIB_PATH_dmc = lib/libdmc
LIB_INCLUDE_dmc = $(LIB_PATH_dmc)/include
LIB_NAME_dmc = $(LIB_PATH_dmc)/libdmc.a

CFLAGS = -Wall -rdynamic
FILES = $(wildcard src/*.c) $(wildcard include/*.h)
bin/MultiMarket: $(FILES) \
		.mkdir/lib/server/libserver.a
	gcc $(CFLAGS) $(FILES) -o bin/MultiMarket \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc) \
		-L.mkdir/lib/server -lserver \
		-L.mkdir/lib/server/acc -lacc \
		-L.mkdir/lib/server/fleas -lfleas \
		-L.mkdir/lib/server/fleas/wgs -lwgs \
		-L.mkdir/lib/server/sys -lsys \
		-L.mkdir/lib/server/sys/nicks -lnicks \
		-L.mkdir/lib/server/sys/servers -lservers \
		-L.mkdir/lib/scheduler -lscheduler \
		-L.mkdir/lib/io -lio \
		-L.mkdir/lib/data/dfleas -ldfleas \
		-L.mkdir/lib/data -ldata \
		-L$(LIB_PATH_dmc) -ldmc \
		-lgc -lpthread -lm
