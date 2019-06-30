PRG = MultiMarket
LIB = dmc

C_FILES = $(wildcard src/*.c) \
					$(wildcard src/data/*.c) \
					$(wildcard src/io/*.c) \
					$(wildcard src/server/*.c) \
					$(wildcard src/server/sys/*.c) \
					$(wildcard src/server/sys/nicks/*.c) \
					$(wildcard src/server/sys/servers/*.c) \
					$(wildcard src/server/fleas/*.c) \
					$(wildcard src/server/fleas/wgs/*.c) \
					$(wildcard src/server/acc/*.c) \
					$(wildcard src/scheduler/*.c) \
					$(wildcard src/scheduler/fleas/*.c)
H_FILES = $(wildcard include/*.h) \
					$(wildcard include/data/*.h) \
					$(wildcard include/io/*.h) \
					$(wildcard include/server/*.h) \
					$(wildcard include/server/sys/*.h) \
					$(wildcard include/server/sys/nicks/*.h) \
					$(wildcard include/server/sys/servers/*.h) \
					$(wildcard include/server/fleas/*.h) \
					$(wildcard include/server/fleas/wgs/*.h) \
					$(wildcard include/server/acc/*.h) \
					$(wildcard include/scheduler/*.h) \
					$(wildcard include/scheduler/fleas/*.h)

LIB_PATH = lib/lib$(LIB)
LIB_INCLUDE = lib/lib$(LIB)/include

CFLAGS = -Wall -rdynamic
FILES = $(C_FILES) $(H_FILES)
bin/$(PRG): $(FILES) $(LIB_PATH)/lib$(LIB).a
	gcc $(CFLAGS) $(FILES) -o bin/$(PRG) \
	  -Iinclude -I$(LIB_INCLUDE) \
		-L$(LIB_PATH) -l$(LIB) \
		-lgc -lpthread -lm
