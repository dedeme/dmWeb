PRG = Mconta

C_FILES = $(wildcard src/*.c) $(wildcard src/pgs/*.c)
H_FILES = $(wildcard include/*.h) $(wildcard include/pgs/*.h)

CFLAGS = -Wall -rdynamic
FILES = $(C_FILES) $(H_FILES)
bin/$(PRG): $(FILES) lib/libdmc/libdmc.a
	gcc $(CFLAGS) $(FILES) -o bin/$(PRG) \
	  -Iinclude \
	  -Ilib/libdmc/include -Llib/libdmc -ldmc \
		-Ilib/libmarket/include -Llib/libmarket -lmarket \
		-lgc
