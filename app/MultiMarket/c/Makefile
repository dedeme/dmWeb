LIB_PATH_dmc = lib/libdmc
LIB_INCLUDE_dmc = $(LIB_PATH_dmc)/include

CFLAGS = -Wall -rdynamic
bin/MultiMarket : src/multimarket.c include/multimarket.h lib/liblocalcompilation/liblocalcompilation.a
	gcc $(CFLAGS) src/multimarket.c -o bin/MultiMarket \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc) \
		-Llib/liblocalcompilation -llocalcompilation \
		-L$(LIB_PATH_dmc) -ldmc \
		-lgc -lpthread -lm

lib/liblocalcompilation/liblocalcompilation.a : lib/liblocalcompilation/obj/multimarket.o lib/liblocalcompilation/obj/io/io.o lib/liblocalcompilation/obj/DEFS.o lib/liblocalcompilation/obj/server.o lib/liblocalcompilation/obj/io/log.o lib/liblocalcompilation/obj/server/hub.o lib/liblocalcompilation/obj/io/backups.o lib/liblocalcompilation/obj/io/trash.o lib/liblocalcompilation/obj/io/conf.o lib/liblocalcompilation/obj/server/sys.o lib/liblocalcompilation/obj/server/sys/sys__main.o lib/liblocalcompilation/obj/server/sys/sys__home.o lib/liblocalcompilation/obj/io/fleasdb.o lib/liblocalcompilation/obj/io/quotes.o lib/liblocalcompilation/obj/io/nicks.o lib/liblocalcompilation/obj/io/servers.o lib/liblocalcompilation/obj/io/net.o lib/liblocalcompilation/obj/io/accdb.o lib/liblocalcompilation/obj/io/managerdb.o lib/liblocalcompilation/obj/data/dfleas/dfleas__models.o lib/liblocalcompilation/obj/data/Model.o lib/liblocalcompilation/obj/data/Facc.o lib/liblocalcompilation/obj/data/broker.o lib/liblocalcompilation/obj/data/Qmatrix.o lib/liblocalcompilation/obj/data/Nick.o lib/liblocalcompilation/obj/data/Flea.o lib/liblocalcompilation/obj/data/Gen.o lib/liblocalcompilation/obj/data/Order.o lib/liblocalcompilation/obj/data/Rs.o lib/liblocalcompilation/obj/data/NickSets.o lib/liblocalcompilation/obj/data/ModelMxMn.o lib/liblocalcompilation/obj/data/dfleas/dfleas__Approx.o lib/liblocalcompilation/obj/data/dfleas/dfleas__Approx2.o lib/liblocalcompilation/obj/data/dfleas/dfleas__Approx3A.o lib/liblocalcompilation/obj/data/dfleas/dfleas__Approx3B.o lib/liblocalcompilation/obj/data/dfleas/dfleas__GA.o lib/liblocalcompilation/obj/data/dfleas/dfleas__Incr2.o lib/liblocalcompilation/obj/data/dfleas/dfleas__Incr3.o lib/liblocalcompilation/obj/data/dfleas/dfleas__Incr3a.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MA.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MA2.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MM1.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MM2A.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MM2Aa.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MM2Ab.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MM2B.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MM4.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMBack1.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMBack2.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMBack2a.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMBack2b.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMWin1.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMWin2.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMWin2a.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMWin2b.o lib/liblocalcompilation/obj/data/ModelParams.o lib/liblocalcompilation/obj/data/Manager.o lib/liblocalcompilation/obj/data/Acc.o lib/liblocalcompilation/obj/io/sbox.o lib/liblocalcompilation/obj/data/DailyEntry.o lib/liblocalcompilation/obj/data/HistoricEntry.o lib/liblocalcompilation/obj/data/Quote.o lib/liblocalcompilation/obj/data/Server.o lib/liblocalcompilation/obj/data/Rconf.o lib/liblocalcompilation/obj/data/NickClose.o lib/liblocalcompilation/obj/data/EMsg.o lib/liblocalcompilation/obj/scheduler/fleas.o lib/liblocalcompilation/obj/server/sys/sys__settings.o lib/liblocalcompilation/obj/server/sys/sys__chpass.o lib/liblocalcompilation/obj/server/sys/sys__backups.o lib/liblocalcompilation/obj/server/sys/sys__schedule.o lib/liblocalcompilation/obj/io/calendar.o lib/liblocalcompilation/obj/data/MarketDay.o lib/liblocalcompilation/obj/data/Timetable.o lib/liblocalcompilation/obj/server/sys/sys__nicks.o lib/liblocalcompilation/obj/server/sys/nicks/sys__nicks__wnick.o lib/liblocalcompilation/obj/server/sys/nicks/sys__nicks__editor.o lib/liblocalcompilation/obj/server/sys/sys__servers.o lib/liblocalcompilation/obj/server/sys/servers/sys__servers__names.o lib/liblocalcompilation/obj/server/sys/servers/sys__servers__codes.o lib/liblocalcompilation/obj/server/sys/servers/sys__servers__download.o lib/liblocalcompilation/obj/server/sys/sys__annotations.o lib/liblocalcompilation/obj/server/sys/sys__models.o lib/liblocalcompilation/obj/scheduler/management.o lib/liblocalcompilation/obj/server/fleas.o lib/liblocalcompilation/obj/server/fleas/fleas__main.o lib/liblocalcompilation/obj/server/fleas/fleas__bests.o lib/liblocalcompilation/obj/server/fleas/fleas__charts.o lib/liblocalcompilation/obj/server/fleas/fleas__model.o lib/liblocalcompilation/obj/server/fleas/fleas__champions.o lib/liblocalcompilation/obj/server/fleas/wgs/fleas__wgs__wcharts.o lib/liblocalcompilation/obj/server/fleas/wgs/fleas__wgs__dtable.o lib/liblocalcompilation/obj/server/acc.o lib/liblocalcompilation/obj/server/acc/acc__downloader.o lib/liblocalcompilation/obj/server/acc/acc__companies.o lib/liblocalcompilation/obj/server/acc/acc__balance.o lib/liblocalcompilation/obj/server/acc/acc__trading.o lib/liblocalcompilation/obj/server/acc/acc__profits.o lib/liblocalcompilation/obj/server/daily.o lib/liblocalcompilation/obj/io/dailydb.o lib/liblocalcompilation/obj/scheduler.o lib/liblocalcompilation/obj/scheduler/historic.o lib/liblocalcompilation/obj/scheduler/acc.o
	ar rcs lib/liblocalcompilation/liblocalcompilation.a lib/liblocalcompilation/obj/multimarket.o lib/liblocalcompilation/obj/io/io.o lib/liblocalcompilation/obj/DEFS.o lib/liblocalcompilation/obj/server.o lib/liblocalcompilation/obj/io/log.o lib/liblocalcompilation/obj/server/hub.o lib/liblocalcompilation/obj/io/backups.o lib/liblocalcompilation/obj/io/trash.o lib/liblocalcompilation/obj/io/conf.o lib/liblocalcompilation/obj/server/sys.o lib/liblocalcompilation/obj/server/sys/sys__main.o lib/liblocalcompilation/obj/server/sys/sys__home.o lib/liblocalcompilation/obj/io/fleasdb.o lib/liblocalcompilation/obj/io/quotes.o lib/liblocalcompilation/obj/io/nicks.o lib/liblocalcompilation/obj/io/servers.o lib/liblocalcompilation/obj/io/net.o lib/liblocalcompilation/obj/io/accdb.o lib/liblocalcompilation/obj/io/managerdb.o lib/liblocalcompilation/obj/data/dfleas/dfleas__models.o lib/liblocalcompilation/obj/data/Model.o lib/liblocalcompilation/obj/data/Facc.o lib/liblocalcompilation/obj/data/broker.o lib/liblocalcompilation/obj/data/Qmatrix.o lib/liblocalcompilation/obj/data/Nick.o lib/liblocalcompilation/obj/data/Flea.o lib/liblocalcompilation/obj/data/Gen.o lib/liblocalcompilation/obj/data/Order.o lib/liblocalcompilation/obj/data/Rs.o lib/liblocalcompilation/obj/data/NickSets.o lib/liblocalcompilation/obj/data/ModelMxMn.o lib/liblocalcompilation/obj/data/dfleas/dfleas__Approx.o lib/liblocalcompilation/obj/data/dfleas/dfleas__Approx2.o lib/liblocalcompilation/obj/data/dfleas/dfleas__Approx3A.o lib/liblocalcompilation/obj/data/dfleas/dfleas__Approx3B.o lib/liblocalcompilation/obj/data/dfleas/dfleas__GA.o lib/liblocalcompilation/obj/data/dfleas/dfleas__Incr2.o lib/liblocalcompilation/obj/data/dfleas/dfleas__Incr3.o lib/liblocalcompilation/obj/data/dfleas/dfleas__Incr3a.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MA.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MA2.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MM1.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MM2A.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MM2Aa.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MM2Ab.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MM2B.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MM4.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMBack1.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMBack2.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMBack2a.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMBack2b.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMWin1.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMWin2.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMWin2a.o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMWin2b.o lib/liblocalcompilation/obj/data/ModelParams.o lib/liblocalcompilation/obj/data/Manager.o lib/liblocalcompilation/obj/data/Acc.o lib/liblocalcompilation/obj/io/sbox.o lib/liblocalcompilation/obj/data/DailyEntry.o lib/liblocalcompilation/obj/data/HistoricEntry.o lib/liblocalcompilation/obj/data/Quote.o lib/liblocalcompilation/obj/data/Server.o lib/liblocalcompilation/obj/data/Rconf.o lib/liblocalcompilation/obj/data/NickClose.o lib/liblocalcompilation/obj/data/EMsg.o lib/liblocalcompilation/obj/scheduler/fleas.o lib/liblocalcompilation/obj/server/sys/sys__settings.o lib/liblocalcompilation/obj/server/sys/sys__chpass.o lib/liblocalcompilation/obj/server/sys/sys__backups.o lib/liblocalcompilation/obj/server/sys/sys__schedule.o lib/liblocalcompilation/obj/io/calendar.o lib/liblocalcompilation/obj/data/MarketDay.o lib/liblocalcompilation/obj/data/Timetable.o lib/liblocalcompilation/obj/server/sys/sys__nicks.o lib/liblocalcompilation/obj/server/sys/nicks/sys__nicks__wnick.o lib/liblocalcompilation/obj/server/sys/nicks/sys__nicks__editor.o lib/liblocalcompilation/obj/server/sys/sys__servers.o lib/liblocalcompilation/obj/server/sys/servers/sys__servers__names.o lib/liblocalcompilation/obj/server/sys/servers/sys__servers__codes.o lib/liblocalcompilation/obj/server/sys/servers/sys__servers__download.o lib/liblocalcompilation/obj/server/sys/sys__annotations.o lib/liblocalcompilation/obj/server/sys/sys__models.o lib/liblocalcompilation/obj/scheduler/management.o lib/liblocalcompilation/obj/server/fleas.o lib/liblocalcompilation/obj/server/fleas/fleas__main.o lib/liblocalcompilation/obj/server/fleas/fleas__bests.o lib/liblocalcompilation/obj/server/fleas/fleas__charts.o lib/liblocalcompilation/obj/server/fleas/fleas__model.o lib/liblocalcompilation/obj/server/fleas/fleas__champions.o lib/liblocalcompilation/obj/server/fleas/wgs/fleas__wgs__wcharts.o lib/liblocalcompilation/obj/server/fleas/wgs/fleas__wgs__dtable.o lib/liblocalcompilation/obj/server/acc.o lib/liblocalcompilation/obj/server/acc/acc__downloader.o lib/liblocalcompilation/obj/server/acc/acc__companies.o lib/liblocalcompilation/obj/server/acc/acc__balance.o lib/liblocalcompilation/obj/server/acc/acc__trading.o lib/liblocalcompilation/obj/server/acc/acc__profits.o lib/liblocalcompilation/obj/server/daily.o lib/liblocalcompilation/obj/io/dailydb.o lib/liblocalcompilation/obj/scheduler.o lib/liblocalcompilation/obj/scheduler/historic.o lib/liblocalcompilation/obj/scheduler/acc.o

lib/liblocalcompilation/obj/multimarket.o : include/multimarket.h src/multimarket.c include/io/io.h src/io/io.c include/DEFS.h src/DEFS.c include/server.h src/server.c include/scheduler.h src/scheduler.c include/io/log.h src/io/log.c include/io/conf.h src/io/conf.c include/io/backups.h src/io/backups.c include/io/trash.h src/io/trash.c include/io/nicks.h src/io/nicks.c include/io/quotes.h src/io/quotes.c include/io/servers.h src/io/servers.c include/io/calendar.h src/io/calendar.c include/io/fleasdb.h src/io/fleasdb.c include/io/accdb.h src/io/accdb.c include/io/sbox.h src/io/sbox.c include/io/dailydb.h src/io/dailydb.c include/io/managerdb.h src/io/managerdb.c include/data/dfleas/dfleas__models.h src/data/dfleas/dfleas__models.c include/scheduler/fleas.h src/scheduler/fleas.c
	gcc $(CFLAGS) -c src/multimarket.c -o lib/liblocalcompilation/obj/multimarket.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/io/io.o : include/io/io.h src/io/io.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/io/io.c -o lib/liblocalcompilation/obj/io/io.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/DEFS.o : include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/DEFS.c -o lib/liblocalcompilation/obj/DEFS.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server.o : include/server.h src/server.c include/io/io.h src/io/io.c include/io/log.h src/io/log.c include/DEFS.h src/DEFS.c include/server/hub.h src/server/hub.c
	gcc $(CFLAGS) -c src/server.c -o lib/liblocalcompilation/obj/server.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/io/log.o : include/io/log.h src/io/log.c include/io/io.h src/io/io.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/io/log.c -o lib/liblocalcompilation/obj/io/log.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/hub.o : include/server/hub.h src/server/hub.c include/DEFS.h src/DEFS.c include/io/backups.h src/io/backups.c include/io/conf.h src/io/conf.c include/server/sys.h src/server/sys.c include/server/fleas.h src/server/fleas.c include/server/acc.h src/server/acc.c include/server/daily.h src/server/daily.c
	gcc $(CFLAGS) -c src/server/hub.c -o lib/liblocalcompilation/obj/server/hub.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/io/backups.o : include/io/backups.h src/io/backups.c include/io/io.h src/io/io.c include/io/trash.h src/io/trash.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/io/backups.c -o lib/liblocalcompilation/obj/io/backups.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/io/trash.o : include/io/trash.h src/io/trash.c include/io/io.h src/io/io.c
	gcc $(CFLAGS) -c src/io/trash.c -o lib/liblocalcompilation/obj/io/trash.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/io/conf.o : include/io/conf.h src/io/conf.c include/io/io.h src/io/io.c include/io/log.h src/io/log.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/io/conf.c -o lib/liblocalcompilation/obj/io/conf.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/sys.o : include/server/sys.h src/server/sys.c include/server/sys/sys__main.h src/server/sys/sys__main.c include/server/sys/sys__home.h src/server/sys/sys__home.c include/server/sys/sys__settings.h src/server/sys/sys__settings.c include/server/sys/sys__chpass.h src/server/sys/sys__chpass.c include/server/sys/sys__backups.h src/server/sys/sys__backups.c include/server/sys/sys__schedule.h src/server/sys/sys__schedule.c include/server/sys/sys__nicks.h src/server/sys/sys__nicks.c include/server/sys/nicks/sys__nicks__wnick.h src/server/sys/nicks/sys__nicks__wnick.c include/server/sys/nicks/sys__nicks__editor.h src/server/sys/nicks/sys__nicks__editor.c include/server/sys/sys__servers.h src/server/sys/sys__servers.c include/server/sys/servers/sys__servers__names.h src/server/sys/servers/sys__servers__names.c include/server/sys/servers/sys__servers__codes.h src/server/sys/servers/sys__servers__codes.c include/server/sys/servers/sys__servers__download.h src/server/sys/servers/sys__servers__download.c include/server/sys/sys__annotations.h src/server/sys/sys__annotations.c include/server/sys/sys__models.h src/server/sys/sys__models.c
	gcc $(CFLAGS) -c src/server/sys.c -o lib/liblocalcompilation/obj/server/sys.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/sys/sys__main.o : include/server/sys/sys__main.h src/server/sys/sys__main.c include/io/conf.h src/io/conf.c
	gcc $(CFLAGS) -c src/server/sys/sys__main.c -o lib/liblocalcompilation/obj/server/sys/sys__main.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/sys/sys__home.o : include/server/sys/sys__home.h src/server/sys/sys__home.c include/io/log.h src/io/log.c include/io/conf.h src/io/conf.c include/io/fleasdb.h src/io/fleasdb.c include/scheduler/fleas.h src/scheduler/fleas.c
	gcc $(CFLAGS) -c src/server/sys/sys__home.c -o lib/liblocalcompilation/obj/server/sys/sys__home.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/io/fleasdb.o : include/io/fleasdb.h src/io/fleasdb.c include/io/io.h src/io/io.c include/io/quotes.h src/io/quotes.c include/data/Rs.h src/data/Rs.c include/data/Model.h src/data/Model.c include/data/dfleas/dfleas__models.h src/data/dfleas/dfleas__models.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/io/fleasdb.c -o lib/liblocalcompilation/obj/io/fleasdb.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/io/quotes.o : include/io/quotes.h src/io/quotes.c include/io/nicks.h src/io/nicks.c include/io/log.h src/io/log.c include/io/io.h src/io/io.c include/data/Nick.h src/data/Nick.c include/data/NickClose.h src/data/NickClose.c include/DEFS.h src/DEFS.c include/data/Quote.h src/data/Quote.c include/data/EMsg.h src/data/EMsg.c include/data/Qmatrix.h src/data/Qmatrix.c include/data/NickSets.h src/data/NickSets.c
	gcc $(CFLAGS) -c src/io/quotes.c -o lib/liblocalcompilation/obj/io/quotes.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/io/nicks.o : include/io/nicks.h src/io/nicks.c include/io/io.h src/io/io.c include/io/quotes.h src/io/quotes.c include/io/servers.h src/io/servers.c include/data/Nick.h src/data/Nick.c
	gcc $(CFLAGS) -c src/io/nicks.c -o lib/liblocalcompilation/obj/io/nicks.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/io/servers.o : include/io/servers.h src/io/servers.c include/io/log.h src/io/log.c include/io/io.h src/io/io.c include/io/nicks.h src/io/nicks.c include/io/net.h src/io/net.c include/DEFS.h src/DEFS.c include/data/Server.h src/data/Server.c include/data/Rconf.h src/data/Rconf.c
	gcc $(CFLAGS) -c src/io/servers.c -o lib/liblocalcompilation/obj/io/servers.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/io/net.o : include/io/net.h src/io/net.c include/io/log.h src/io/log.c include/io/servers.h src/io/servers.c include/io/quotes.h src/io/quotes.c include/io/nicks.h src/io/nicks.c include/io/conf.h src/io/conf.c include/io/accdb.h src/io/accdb.c include/io/sbox.h src/io/sbox.c include/data/DailyEntry.h src/data/DailyEntry.c include/data/HistoricEntry.h src/data/HistoricEntry.c include/data/Server.h src/data/Server.c include/data/NickClose.h src/data/NickClose.c include/DEFS.h src/DEFS.c include/data/Rconf.h src/data/Rconf.c include/data/EMsg.h src/data/EMsg.c
	gcc $(CFLAGS) -c src/io/net.c -o lib/liblocalcompilation/obj/io/net.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/io/accdb.o : include/io/accdb.h src/io/accdb.c include/io/io.h src/io/io.c include/io/conf.h src/io/conf.c include/io/quotes.h src/io/quotes.c include/io/managerdb.h src/io/managerdb.c include/data/dfleas/dfleas__models.h src/data/dfleas/dfleas__models.c include/data/Model.h src/data/Model.c include/DEFS.h src/DEFS.c include/data/Qmatrix.h src/data/Qmatrix.c include/data/Acc.h src/data/Acc.c include/data/Rs.h src/data/Rs.c
	gcc $(CFLAGS) -c src/io/accdb.c -o lib/liblocalcompilation/obj/io/accdb.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/io/managerdb.o : include/io/managerdb.h src/io/managerdb.c include/io/io.h src/io/io.c include/data/dfleas/dfleas__models.h src/data/dfleas/dfleas__models.c include/data/Manager.h src/data/Manager.c
	gcc $(CFLAGS) -c src/io/managerdb.c -o lib/liblocalcompilation/obj/io/managerdb.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__models.o : include/data/dfleas/dfleas__models.h src/data/dfleas/dfleas__models.c include/data/Model.h src/data/Model.c include/data/dfleas/dfleas__Approx.h src/data/dfleas/dfleas__Approx.c include/data/dfleas/dfleas__Approx2.h src/data/dfleas/dfleas__Approx2.c include/data/dfleas/dfleas__Approx3A.h src/data/dfleas/dfleas__Approx3A.c include/data/dfleas/dfleas__Approx3B.h src/data/dfleas/dfleas__Approx3B.c include/data/dfleas/dfleas__GA.h src/data/dfleas/dfleas__GA.c include/data/dfleas/dfleas__Incr2.h src/data/dfleas/dfleas__Incr2.c include/data/dfleas/dfleas__Incr3.h src/data/dfleas/dfleas__Incr3.c include/data/dfleas/dfleas__Incr3a.h src/data/dfleas/dfleas__Incr3a.c include/data/dfleas/dfleas__MA.h src/data/dfleas/dfleas__MA.c include/data/dfleas/dfleas__MA2.h src/data/dfleas/dfleas__MA2.c include/data/dfleas/dfleas__MM1.h src/data/dfleas/dfleas__MM1.c include/data/dfleas/dfleas__MM2A.h src/data/dfleas/dfleas__MM2A.c include/data/dfleas/dfleas__MM2Aa.h src/data/dfleas/dfleas__MM2Aa.c include/data/dfleas/dfleas__MM2Ab.h src/data/dfleas/dfleas__MM2Ab.c include/data/dfleas/dfleas__MM2B.h src/data/dfleas/dfleas__MM2B.c include/data/dfleas/dfleas__MM4.h src/data/dfleas/dfleas__MM4.c include/data/dfleas/dfleas__MMBack1.h src/data/dfleas/dfleas__MMBack1.c include/data/dfleas/dfleas__MMBack2.h src/data/dfleas/dfleas__MMBack2.c include/data/dfleas/dfleas__MMBack2a.h src/data/dfleas/dfleas__MMBack2a.c include/data/dfleas/dfleas__MMBack2b.h src/data/dfleas/dfleas__MMBack2b.c include/data/dfleas/dfleas__MMWin1.h src/data/dfleas/dfleas__MMWin1.c include/data/dfleas/dfleas__MMWin2.h src/data/dfleas/dfleas__MMWin2.c include/data/dfleas/dfleas__MMWin2a.h src/data/dfleas/dfleas__MMWin2a.c include/data/dfleas/dfleas__MMWin2b.h src/data/dfleas/dfleas__MMWin2b.c include/data/ModelParams.h src/data/ModelParams.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__models.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__models.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Model.o : include/data/Model.h src/data/Model.c include/data/Facc.h src/data/Facc.c include/data/Nick.h src/data/Nick.c include/data/broker.h src/data/broker.c include/DEFS.h src/DEFS.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/data/Rs.h src/data/Rs.c include/data/Qmatrix.h src/data/Qmatrix.c include/data/NickSets.h src/data/NickSets.c include/data/ModelMxMn.h src/data/ModelMxMn.c
	gcc $(CFLAGS) -c src/data/Model.c -o lib/liblocalcompilation/obj/data/Model.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Facc.o : include/data/Facc.h src/data/Facc.c include/DEFS.h src/DEFS.c include/data/broker.h src/data/broker.c include/data/Qmatrix.h src/data/Qmatrix.c
	gcc $(CFLAGS) -c src/data/Facc.c -o lib/liblocalcompilation/obj/data/Facc.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/broker.o : include/data/broker.h src/data/broker.c
	gcc $(CFLAGS) -c src/data/broker.c -o lib/liblocalcompilation/obj/data/broker.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Qmatrix.o : include/data/Qmatrix.h src/data/Qmatrix.c
	gcc $(CFLAGS) -c src/data/Qmatrix.c -o lib/liblocalcompilation/obj/data/Qmatrix.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Nick.o : include/data/Nick.h src/data/Nick.c
	gcc $(CFLAGS) -c src/data/Nick.c -o lib/liblocalcompilation/obj/data/Nick.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Flea.o : include/data/Flea.h src/data/Flea.c include/data/Gen.h src/data/Gen.c
	gcc $(CFLAGS) -c src/data/Flea.c -o lib/liblocalcompilation/obj/data/Flea.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Gen.o : include/data/Gen.h src/data/Gen.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/data/Gen.c -o lib/liblocalcompilation/obj/data/Gen.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Order.o : include/data/Order.h src/data/Order.c
	gcc $(CFLAGS) -c src/data/Order.c -o lib/liblocalcompilation/obj/data/Order.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Rs.o : include/data/Rs.h src/data/Rs.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c
	gcc $(CFLAGS) -c src/data/Rs.c -o lib/liblocalcompilation/obj/data/Rs.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/NickSets.o : include/data/NickSets.h src/data/NickSets.c
	gcc $(CFLAGS) -c src/data/NickSets.c -o lib/liblocalcompilation/obj/data/NickSets.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/ModelMxMn.o : include/data/ModelMxMn.h src/data/ModelMxMn.c
	gcc $(CFLAGS) -c src/data/ModelMxMn.c -o lib/liblocalcompilation/obj/data/ModelMxMn.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__Approx.o : include/data/dfleas/dfleas__Approx.h src/data/dfleas/dfleas__Approx.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__Approx.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__Approx.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__Approx2.o : include/data/dfleas/dfleas__Approx2.h src/data/dfleas/dfleas__Approx2.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__Approx2.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__Approx2.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__Approx3A.o : include/data/dfleas/dfleas__Approx3A.h src/data/dfleas/dfleas__Approx3A.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__Approx3A.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__Approx3A.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__Approx3B.o : include/data/dfleas/dfleas__Approx3B.h src/data/dfleas/dfleas__Approx3B.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__Approx3B.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__Approx3B.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__GA.o : include/data/dfleas/dfleas__GA.h src/data/dfleas/dfleas__GA.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__GA.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__GA.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__Incr2.o : include/data/dfleas/dfleas__Incr2.h src/data/dfleas/dfleas__Incr2.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__Incr2.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__Incr2.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__Incr3.o : include/data/dfleas/dfleas__Incr3.h src/data/dfleas/dfleas__Incr3.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__Incr3.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__Incr3.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__Incr3a.o : include/data/dfleas/dfleas__Incr3a.h src/data/dfleas/dfleas__Incr3a.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__Incr3a.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__Incr3a.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__MA.o : include/data/dfleas/dfleas__MA.h src/data/dfleas/dfleas__MA.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__MA.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__MA.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__MA2.o : include/data/dfleas/dfleas__MA2.h src/data/dfleas/dfleas__MA2.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__MA2.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__MA2.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__MM1.o : include/data/dfleas/dfleas__MM1.h src/data/dfleas/dfleas__MM1.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__MM1.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__MM1.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__MM2A.o : include/data/dfleas/dfleas__MM2A.h src/data/dfleas/dfleas__MM2A.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__MM2A.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__MM2A.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__MM2Aa.o : include/data/dfleas/dfleas__MM2Aa.h src/data/dfleas/dfleas__MM2Aa.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__MM2Aa.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__MM2Aa.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__MM2Ab.o : include/data/dfleas/dfleas__MM2Ab.h src/data/dfleas/dfleas__MM2Ab.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__MM2Ab.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__MM2Ab.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__MM2B.o : include/data/dfleas/dfleas__MM2B.h src/data/dfleas/dfleas__MM2B.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__MM2B.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__MM2B.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__MM4.o : include/data/dfleas/dfleas__MM4.h src/data/dfleas/dfleas__MM4.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__MM4.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__MM4.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__MMBack1.o : include/data/dfleas/dfleas__MMBack1.h src/data/dfleas/dfleas__MMBack1.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__MMBack1.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMBack1.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__MMBack2.o : include/data/dfleas/dfleas__MMBack2.h src/data/dfleas/dfleas__MMBack2.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__MMBack2.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMBack2.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__MMBack2a.o : include/data/dfleas/dfleas__MMBack2a.h src/data/dfleas/dfleas__MMBack2a.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__MMBack2a.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMBack2a.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__MMBack2b.o : include/data/dfleas/dfleas__MMBack2b.h src/data/dfleas/dfleas__MMBack2b.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__MMBack2b.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMBack2b.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__MMWin1.o : include/data/dfleas/dfleas__MMWin1.h src/data/dfleas/dfleas__MMWin1.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__MMWin1.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMWin1.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__MMWin2.o : include/data/dfleas/dfleas__MMWin2.h src/data/dfleas/dfleas__MMWin2.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__MMWin2.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMWin2.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__MMWin2a.o : include/data/dfleas/dfleas__MMWin2a.h src/data/dfleas/dfleas__MMWin2a.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__MMWin2a.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMWin2a.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/dfleas/dfleas__MMWin2b.o : include/data/dfleas/dfleas__MMWin2b.h src/data/dfleas/dfleas__MMWin2b.c include/data/Flea.h src/data/Flea.c include/data/Order.h src/data/Order.c include/DEFS.h src/DEFS.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/dfleas/dfleas__MMWin2b.c -o lib/liblocalcompilation/obj/data/dfleas/dfleas__MMWin2b.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/ModelParams.o : include/data/ModelParams.h src/data/ModelParams.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/data/ModelParams.c -o lib/liblocalcompilation/obj/data/ModelParams.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Manager.o : include/data/Manager.h src/data/Manager.c include/data/ModelMxMn.h src/data/ModelMxMn.c include/data/ModelParams.h src/data/ModelParams.c
	gcc $(CFLAGS) -c src/data/Manager.c -o lib/liblocalcompilation/obj/data/Manager.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Acc.o : include/data/Acc.h src/data/Acc.c include/data/broker.h src/data/broker.c
	gcc $(CFLAGS) -c src/data/Acc.c -o lib/liblocalcompilation/obj/data/Acc.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/io/sbox.o : include/io/sbox.h src/io/sbox.c include/io/io.h src/io/io.c include/io/servers.h src/io/servers.c include/io/log.h src/io/log.c
	gcc $(CFLAGS) -c src/io/sbox.c -o lib/liblocalcompilation/obj/io/sbox.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/DailyEntry.o : include/data/DailyEntry.h src/data/DailyEntry.c
	gcc $(CFLAGS) -c src/data/DailyEntry.c -o lib/liblocalcompilation/obj/data/DailyEntry.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/HistoricEntry.o : include/data/HistoricEntry.h src/data/HistoricEntry.c include/data/Quote.h src/data/Quote.c
	gcc $(CFLAGS) -c src/data/HistoricEntry.c -o lib/liblocalcompilation/obj/data/HistoricEntry.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Quote.o : include/data/Quote.h src/data/Quote.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/data/Quote.c -o lib/liblocalcompilation/obj/data/Quote.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Server.o : include/data/Server.h src/data/Server.c include/data/Rconf.h src/data/Rconf.c include/data/Nick.h src/data/Nick.c include/data/DailyEntry.h src/data/DailyEntry.c include/data/NickClose.h src/data/NickClose.c include/data/HistoricEntry.h src/data/HistoricEntry.c include/data/Quote.h src/data/Quote.c
	gcc $(CFLAGS) -c src/data/Server.c -o lib/liblocalcompilation/obj/data/Server.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Rconf.o : include/data/Rconf.h src/data/Rconf.c
	gcc $(CFLAGS) -c src/data/Rconf.c -o lib/liblocalcompilation/obj/data/Rconf.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/NickClose.o : include/data/NickClose.h src/data/NickClose.c
	gcc $(CFLAGS) -c src/data/NickClose.c -o lib/liblocalcompilation/obj/data/NickClose.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/EMsg.o : include/data/EMsg.h src/data/EMsg.c
	gcc $(CFLAGS) -c src/data/EMsg.c -o lib/liblocalcompilation/obj/data/EMsg.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/scheduler/fleas.o : include/scheduler/fleas.h src/scheduler/fleas.c include/data/dfleas/dfleas__models.h src/data/dfleas/dfleas__models.c include/data/Model.h src/data/Model.c include/data/Rs.h src/data/Rs.c include/io/log.h src/io/log.c include/io/fleasdb.h src/io/fleasdb.c include/io/conf.h src/io/conf.c include/io/quotes.h src/io/quotes.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/scheduler/fleas.c -o lib/liblocalcompilation/obj/scheduler/fleas.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/sys/sys__settings.o : include/server/sys/sys__settings.h src/server/sys/sys__settings.c include/io/conf.h src/io/conf.c
	gcc $(CFLAGS) -c src/server/sys/sys__settings.c -o lib/liblocalcompilation/obj/server/sys/sys__settings.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/sys/sys__chpass.o : include/server/sys/sys__chpass.h src/server/sys/sys__chpass.c
	gcc $(CFLAGS) -c src/server/sys/sys__chpass.c -o lib/liblocalcompilation/obj/server/sys/sys__chpass.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/sys/sys__backups.o : include/server/sys/sys__backups.h src/server/sys/sys__backups.c include/io/backups.h src/io/backups.c include/io/trash.h src/io/trash.c
	gcc $(CFLAGS) -c src/server/sys/sys__backups.c -o lib/liblocalcompilation/obj/server/sys/sys__backups.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/sys/sys__schedule.o : include/server/sys/sys__schedule.h src/server/sys/sys__schedule.c include/io/calendar.h src/io/calendar.c include/data/Timetable.h src/data/Timetable.c include/data/MarketDay.h src/data/MarketDay.c
	gcc $(CFLAGS) -c src/server/sys/sys__schedule.c -o lib/liblocalcompilation/obj/server/sys/sys__schedule.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/io/calendar.o : include/io/calendar.h src/io/calendar.c include/io/io.h src/io/io.c include/data/MarketDay.h src/data/MarketDay.c include/DEFS.h src/DEFS.c include/data/Timetable.h src/data/Timetable.c
	gcc $(CFLAGS) -c src/io/calendar.c -o lib/liblocalcompilation/obj/io/calendar.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/MarketDay.o : include/data/MarketDay.h src/data/MarketDay.c
	gcc $(CFLAGS) -c src/data/MarketDay.c -o lib/liblocalcompilation/obj/data/MarketDay.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Timetable.o : include/data/Timetable.h src/data/Timetable.c
	gcc $(CFLAGS) -c src/data/Timetable.c -o lib/liblocalcompilation/obj/data/Timetable.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/sys/sys__nicks.o : include/server/sys/sys__nicks.h src/server/sys/sys__nicks.c include/io/conf.h src/io/conf.c include/io/nicks.h src/io/nicks.c include/io/quotes.h src/io/quotes.c include/io/net.h src/io/net.c
	gcc $(CFLAGS) -c src/server/sys/sys__nicks.c -o lib/liblocalcompilation/obj/server/sys/sys__nicks.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/sys/nicks/sys__nicks__wnick.o : include/server/sys/nicks/sys__nicks__wnick.h src/server/sys/nicks/sys__nicks__wnick.c include/io/nicks.h src/io/nicks.c
	gcc $(CFLAGS) -c src/server/sys/nicks/sys__nicks__wnick.c -o lib/liblocalcompilation/obj/server/sys/nicks/sys__nicks__wnick.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/sys/nicks/sys__nicks__editor.o : include/server/sys/nicks/sys__nicks__editor.h src/server/sys/nicks/sys__nicks__editor.c include/io/nicks.h src/io/nicks.c include/io/quotes.h src/io/quotes.c include/io/servers.h src/io/servers.c include/data/Quote.h src/data/Quote.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/server/sys/nicks/sys__nicks__editor.c -o lib/liblocalcompilation/obj/server/sys/nicks/sys__nicks__editor.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/sys/sys__servers.o : include/server/sys/sys__servers.h src/server/sys/sys__servers.c include/io/servers.h src/io/servers.c include/io/nicks.h src/io/nicks.c include/io/conf.h src/io/conf.c
	gcc $(CFLAGS) -c src/server/sys/sys__servers.c -o lib/liblocalcompilation/obj/server/sys/sys__servers.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/sys/servers/sys__servers__names.o : include/server/sys/servers/sys__servers__names.h src/server/sys/servers/sys__servers__names.c include/io/servers.h src/io/servers.c
	gcc $(CFLAGS) -c src/server/sys/servers/sys__servers__names.c -o lib/liblocalcompilation/obj/server/sys/servers/sys__servers__names.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/sys/servers/sys__servers__codes.o : include/server/sys/servers/sys__servers__codes.h src/server/sys/servers/sys__servers__codes.c include/io/servers.h src/io/servers.c include/data/Server.h src/data/Server.c
	gcc $(CFLAGS) -c src/server/sys/servers/sys__servers__codes.c -o lib/liblocalcompilation/obj/server/sys/servers/sys__servers__codes.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/sys/servers/sys__servers__download.o : include/server/sys/servers/sys__servers__download.h src/server/sys/servers/sys__servers__download.c include/io/servers.h src/io/servers.c include/data/Rconf.h src/data/Rconf.c include/io/log.h src/io/log.c
	gcc $(CFLAGS) -c src/server/sys/servers/sys__servers__download.c -o lib/liblocalcompilation/obj/server/sys/servers/sys__servers__download.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/sys/sys__annotations.o : include/server/sys/sys__annotations.h src/server/sys/sys__annotations.c include/io/conf.h src/io/conf.c include/io/accdb.h src/io/accdb.c include/io/log.h src/io/log.c include/data/Acc.h src/data/Acc.c
	gcc $(CFLAGS) -c src/server/sys/sys__annotations.c -o lib/liblocalcompilation/obj/server/sys/sys__annotations.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/sys/sys__models.o : include/server/sys/sys__models.h src/server/sys/sys__models.c include/data/dfleas/dfleas__models.h src/data/dfleas/dfleas__models.c include/data/Manager.h src/data/Manager.c include/io/managerdb.h src/io/managerdb.c include/io/nicks.h src/io/nicks.c include/scheduler/management.h src/scheduler/management.c
	gcc $(CFLAGS) -c src/server/sys/sys__models.c -o lib/liblocalcompilation/obj/server/sys/sys__models.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/scheduler/management.o : include/scheduler/management.h src/scheduler/management.c include/io/managerdb.h src/io/managerdb.c include/io/nicks.h src/io/nicks.c include/io/quotes.h src/io/quotes.c include/data/Rs.h src/data/Rs.c
	gcc $(CFLAGS) -c src/scheduler/management.c -o lib/liblocalcompilation/obj/scheduler/management.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/fleas.o : include/server/fleas.h src/server/fleas.c include/server/fleas/fleas__main.h src/server/fleas/fleas__main.c include/server/fleas/fleas__bests.h src/server/fleas/fleas__bests.c include/server/fleas/fleas__charts.h src/server/fleas/fleas__charts.c include/server/fleas/fleas__model.h src/server/fleas/fleas__model.c include/server/fleas/fleas__champions.h src/server/fleas/fleas__champions.c include/server/fleas/wgs/fleas__wgs__wcharts.h src/server/fleas/wgs/fleas__wgs__wcharts.c include/server/fleas/wgs/fleas__wgs__dtable.h src/server/fleas/wgs/fleas__wgs__dtable.c
	gcc $(CFLAGS) -c src/server/fleas.c -o lib/liblocalcompilation/obj/server/fleas.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/fleas/fleas__main.o : include/server/fleas/fleas__main.h src/server/fleas/fleas__main.c include/io/conf.h src/io/conf.c include/data/Model.h src/data/Model.c include/data/ModelMxMn.h src/data/ModelMxMn.c include/data/dfleas/dfleas__models.h src/data/dfleas/dfleas__models.c
	gcc $(CFLAGS) -c src/server/fleas/fleas__main.c -o lib/liblocalcompilation/obj/server/fleas/fleas__main.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/fleas/fleas__bests.o : include/server/fleas/fleas__bests.h src/server/fleas/fleas__bests.c include/io/conf.h src/io/conf.c include/io/fleasdb.h src/io/fleasdb.c include/data/dfleas/dfleas__models.h src/data/dfleas/dfleas__models.c include/data/Model.h src/data/Model.c include/data/ModelMxMn.h src/data/ModelMxMn.c
	gcc $(CFLAGS) -c src/server/fleas/fleas__bests.c -o lib/liblocalcompilation/obj/server/fleas/fleas__bests.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/fleas/fleas__charts.o : include/server/fleas/fleas__charts.h src/server/fleas/fleas__charts.c include/io/conf.h src/io/conf.c include/io/fleasdb.h src/io/fleasdb.c
	gcc $(CFLAGS) -c src/server/fleas/fleas__charts.c -o lib/liblocalcompilation/obj/server/fleas/fleas__charts.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/fleas/fleas__model.o : include/server/fleas/fleas__model.h src/server/fleas/fleas__model.c include/io/fleasdb.h src/io/fleasdb.c include/data/dfleas/dfleas__models.h src/data/dfleas/dfleas__models.c include/data/Model.h src/data/Model.c include/data/ModelMxMn.h src/data/ModelMxMn.c
	gcc $(CFLAGS) -c src/server/fleas/fleas__model.c -o lib/liblocalcompilation/obj/server/fleas/fleas__model.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/fleas/fleas__champions.o : include/server/fleas/fleas__champions.h src/server/fleas/fleas__champions.c include/io/conf.h src/io/conf.c include/io/fleasdb.h src/io/fleasdb.c include/data/dfleas/dfleas__models.h src/data/dfleas/dfleas__models.c include/data/Model.h src/data/Model.c
	gcc $(CFLAGS) -c src/server/fleas/fleas__champions.c -o lib/liblocalcompilation/obj/server/fleas/fleas__champions.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/fleas/wgs/fleas__wgs__wcharts.o : include/server/fleas/wgs/fleas__wgs__wcharts.h src/server/fleas/wgs/fleas__wgs__wcharts.c include/io/fleasdb.h src/io/fleasdb.c
	gcc $(CFLAGS) -c src/server/fleas/wgs/fleas__wgs__wcharts.c -o lib/liblocalcompilation/obj/server/fleas/wgs/fleas__wgs__wcharts.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/fleas/wgs/fleas__wgs__dtable.o : include/server/fleas/wgs/fleas__wgs__dtable.h src/server/fleas/wgs/fleas__wgs__dtable.c include/data/Nick.h src/data/Nick.c include/io/nicks.h src/io/nicks.c
	gcc $(CFLAGS) -c src/server/fleas/wgs/fleas__wgs__dtable.c -o lib/liblocalcompilation/obj/server/fleas/wgs/fleas__wgs__dtable.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/acc.o : include/server/acc.h src/server/acc.c include/server/acc/acc__downloader.h src/server/acc/acc__downloader.c include/server/acc/acc__companies.h src/server/acc/acc__companies.c include/server/acc/acc__balance.h src/server/acc/acc__balance.c include/server/acc/acc__trading.h src/server/acc/acc__trading.c include/server/acc/acc__profits.h src/server/acc/acc__profits.c
	gcc $(CFLAGS) -c src/server/acc.c -o lib/liblocalcompilation/obj/server/acc.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/acc/acc__downloader.o : include/server/acc/acc__downloader.h src/server/acc/acc__downloader.c include/io/net.h src/io/net.c
	gcc $(CFLAGS) -c src/server/acc/acc__downloader.c -o lib/liblocalcompilation/obj/server/acc/acc__downloader.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/acc/acc__companies.o : include/server/acc/acc__companies.h src/server/acc/acc__companies.c include/io/conf.h src/io/conf.c include/io/nicks.h src/io/nicks.c include/io/accdb.h src/io/accdb.c include/io/log.h src/io/log.c include/io/servers.h src/io/servers.c include/io/quotes.h src/io/quotes.c include/io/calendar.h src/io/calendar.c include/data/dfleas/dfleas__models.h src/data/dfleas/dfleas__models.c include/data/Acc.h src/data/Acc.c include/data/Model.h src/data/Model.c include/data/Quote.h src/data/Quote.c include/data/ModelParams.h src/data/ModelParams.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/server/acc/acc__companies.c -o lib/liblocalcompilation/obj/server/acc/acc__companies.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/acc/acc__balance.o : include/server/acc/acc__balance.h src/server/acc/acc__balance.c include/io/accdb.h src/io/accdb.c include/io/log.h src/io/log.c include/io/managerdb.h src/io/managerdb.c include/data/Acc.h src/data/Acc.c
	gcc $(CFLAGS) -c src/server/acc/acc__balance.c -o lib/liblocalcompilation/obj/server/acc/acc__balance.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/acc/acc__trading.o : include/server/acc/acc__trading.h src/server/acc/acc__trading.c include/io/nicks.h src/io/nicks.c include/io/accdb.h src/io/accdb.c include/io/log.h src/io/log.c include/io/conf.h src/io/conf.c include/io/managerdb.h src/io/managerdb.c include/data/Acc.h src/data/Acc.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/server/acc/acc__trading.c -o lib/liblocalcompilation/obj/server/acc/acc__trading.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/acc/acc__profits.o : include/server/acc/acc__profits.h src/server/acc/acc__profits.c include/io/accdb.h src/io/accdb.c include/io/conf.h src/io/conf.c include/io/log.h src/io/log.c include/data/Acc.h src/data/Acc.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/server/acc/acc__profits.c -o lib/liblocalcompilation/obj/server/acc/acc__profits.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/daily.o : include/server/daily.h src/server/daily.c include/io/conf.h src/io/conf.c include/io/sbox.h src/io/sbox.c include/io/dailydb.h src/io/dailydb.c include/data/Server.h src/data/Server.c
	gcc $(CFLAGS) -c src/server/daily.c -o lib/liblocalcompilation/obj/server/daily.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/io/dailydb.o : include/io/dailydb.h src/io/dailydb.c include/io/io.h src/io/io.c include/data/Nick.h src/data/Nick.c include/data/Acc.h src/data/Acc.c include/data/Rs.h src/data/Rs.c include/io/accdb.h src/io/accdb.c include/io/quotes.h src/io/quotes.c include/io/nicks.h src/io/nicks.c include/io/log.h src/io/log.c
	gcc $(CFLAGS) -c src/io/dailydb.c -o lib/liblocalcompilation/obj/io/dailydb.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/scheduler.o : include/scheduler.h src/scheduler.c include/io/net.h src/io/net.c include/io/io.h src/io/io.c include/io/conf.h src/io/conf.c include/io/calendar.h src/io/calendar.c include/io/quotes.h src/io/quotes.c include/io/sbox.h src/io/sbox.c include/io/dailydb.h src/io/dailydb.c include/io/log.h src/io/log.c include/DEFS.h src/DEFS.c include/scheduler/historic.h src/scheduler/historic.c include/scheduler/fleas.h src/scheduler/fleas.c include/scheduler/acc.h src/scheduler/acc.c include/scheduler/management.h src/scheduler/management.c
	gcc $(CFLAGS) -c src/scheduler.c -o lib/liblocalcompilation/obj/scheduler.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/scheduler/historic.o : include/scheduler/historic.h src/scheduler/historic.c include/io/nicks.h src/io/nicks.c include/io/servers.h src/io/servers.c include/io/log.h src/io/log.c include/io/net.h src/io/net.c include/io/io.h src/io/io.c
	gcc $(CFLAGS) -c src/scheduler/historic.c -o lib/liblocalcompilation/obj/scheduler/historic.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/scheduler/acc.o : include/scheduler/acc.h src/scheduler/acc.c include/data/Acc.h src/data/Acc.c include/io/accdb.h src/io/accdb.c include/io/log.h src/io/log.c
	gcc $(CFLAGS) -c src/scheduler/acc.c -o lib/liblocalcompilation/obj/scheduler/acc.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

