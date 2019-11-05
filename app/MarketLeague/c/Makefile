LIB_PATH_match = lib/libmatch
LIB_INCLUDE_match = $(LIB_PATH_match)/include

LIB_PATH_dmc = lib/libdmc
LIB_INCLUDE_dmc = $(LIB_PATH_dmc)/include

CFLAGS = -Wall -rdynamic
bin/MarketLeague : src/main.c include/main.h lib/liblocalcompilation/liblocalcompilation.a
	gcc $(CFLAGS) src/main.c -o bin/MarketLeague \
		-Iinclude \
		-I$(LIB_INCLUDE_match) \
		-I$(LIB_INCLUDE_dmc) \
		-Llib/liblocalcompilation -llocalcompilation \
		-L$(LIB_PATH_match) -lmatch \
		-L$(LIB_PATH_dmc) -ldmc \
		-lgc -lpthread -lm

lib/liblocalcompilation/liblocalcompilation.a : lib/liblocalcompilation/obj/main.o lib/liblocalcompilation/obj/pages/pgmain.o lib/liblocalcompilation/obj/io/conf.o lib/liblocalcompilation/obj/pages/settings.o lib/liblocalcompilation/obj/pages/backups.o lib/liblocalcompilation/obj/io/backupsdb.o lib/liblocalcompilation/obj/pages/leagues.o lib/liblocalcompilation/obj/io/leaguesdb.o lib/liblocalcompilation/obj/io/quotes.o lib/liblocalcompilation/obj/data/Quote.o lib/liblocalcompilation/obj/DEFS.o lib/liblocalcompilation/obj/data/Session.o lib/liblocalcompilation/obj/pages/league.o
	ar rcs lib/liblocalcompilation/liblocalcompilation.a lib/liblocalcompilation/obj/main.o lib/liblocalcompilation/obj/pages/pgmain.o lib/liblocalcompilation/obj/io/conf.o lib/liblocalcompilation/obj/pages/settings.o lib/liblocalcompilation/obj/pages/backups.o lib/liblocalcompilation/obj/io/backupsdb.o lib/liblocalcompilation/obj/pages/leagues.o lib/liblocalcompilation/obj/io/leaguesdb.o lib/liblocalcompilation/obj/io/quotes.o lib/liblocalcompilation/obj/data/Quote.o lib/liblocalcompilation/obj/DEFS.o lib/liblocalcompilation/obj/data/Session.o lib/liblocalcompilation/obj/pages/league.o

lib/liblocalcompilation/obj/main.o : include/main.h src/main.c include/pages/pgmain.h src/pages/pgmain.c include/pages/settings.h src/pages/settings.c include/pages/backups.h src/pages/backups.c include/io/backupsdb.h src/io/backupsdb.c include/pages/leagues.h src/pages/leagues.c include/pages/league.h src/pages/league.c
	gcc $(CFLAGS) -c src/main.c -o lib/liblocalcompilation/obj/main.o \
		-Iinclude \
		-I$(LIB_INCLUDE_match) \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/pages/pgmain.o : include/pages/pgmain.h src/pages/pgmain.c include/io/conf.h src/io/conf.c
	gcc $(CFLAGS) -c src/pages/pgmain.c -o lib/liblocalcompilation/obj/pages/pgmain.o \
		-Iinclude \
		-I$(LIB_INCLUDE_match) \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/io/conf.o : include/io/conf.h src/io/conf.c
	gcc $(CFLAGS) -c src/io/conf.c -o lib/liblocalcompilation/obj/io/conf.o \
		-Iinclude \
		-I$(LIB_INCLUDE_match) \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/pages/settings.o : include/pages/settings.h src/pages/settings.c include/io/conf.h src/io/conf.c
	gcc $(CFLAGS) -c src/pages/settings.c -o lib/liblocalcompilation/obj/pages/settings.o \
		-Iinclude \
		-I$(LIB_INCLUDE_match) \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/pages/backups.o : include/pages/backups.h src/pages/backups.c include/io/backupsdb.h src/io/backupsdb.c
	gcc $(CFLAGS) -c src/pages/backups.c -o lib/liblocalcompilation/obj/pages/backups.o \
		-Iinclude \
		-I$(LIB_INCLUDE_match) \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/io/backupsdb.o : include/io/backupsdb.h src/io/backupsdb.c include/main.h src/main.c
	gcc $(CFLAGS) -c src/io/backupsdb.c -o lib/liblocalcompilation/obj/io/backupsdb.o \
		-Iinclude \
		-I$(LIB_INCLUDE_match) \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/pages/leagues.o : include/pages/leagues.h src/pages/leagues.c include/io/leaguesdb.h src/io/leaguesdb.c
	gcc $(CFLAGS) -c src/pages/leagues.c -o lib/liblocalcompilation/obj/pages/leagues.o \
		-Iinclude \
		-I$(LIB_INCLUDE_match) \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/io/leaguesdb.o : include/io/leaguesdb.h src/io/leaguesdb.c include/io/quotes.h src/io/quotes.c include/data/Session.h src/data/Session.c include/data/Quote.h src/data/Quote.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/io/leaguesdb.c -o lib/liblocalcompilation/obj/io/leaguesdb.o \
		-Iinclude \
		-I$(LIB_INCLUDE_match) \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/io/quotes.o : include/io/quotes.h src/io/quotes.c include/data/Quote.h src/data/Quote.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/io/quotes.c -o lib/liblocalcompilation/obj/io/quotes.o \
		-Iinclude \
		-I$(LIB_INCLUDE_match) \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Quote.o : include/data/Quote.h src/data/Quote.c
	gcc $(CFLAGS) -c src/data/Quote.c -o lib/liblocalcompilation/obj/data/Quote.o \
		-Iinclude \
		-I$(LIB_INCLUDE_match) \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/DEFS.o : include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/DEFS.c -o lib/liblocalcompilation/obj/DEFS.o \
		-Iinclude \
		-I$(LIB_INCLUDE_match) \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Session.o : include/data/Session.h src/data/Session.c
	gcc $(CFLAGS) -c src/data/Session.c -o lib/liblocalcompilation/obj/data/Session.o \
		-Iinclude \
		-I$(LIB_INCLUDE_match) \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/pages/league.o : include/pages/league.h src/pages/league.c include/io/leaguesdb.h src/io/leaguesdb.c
	gcc $(CFLAGS) -c src/pages/league.c -o lib/liblocalcompilation/obj/pages/league.o \
		-Iinclude \
		-I$(LIB_INCLUDE_match) \
		-I$(LIB_INCLUDE_dmc)

