LIB_PATH_dmc = lib/libdmc
LIB_INCLUDE_dmc = $(LIB_PATH_dmc)/include

CFLAGS = -Wall -rdynamic
bin/MMarket : src/mmarket.c include/mmarket.h lib/liblocalcompilation/liblocalcompilation.a
	gcc $(CFLAGS) src/mmarket.c -o bin/MMarket \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc) \
		-Llib/liblocalcompilation -llocalcompilation \
		-L$(LIB_PATH_dmc) -ldmc \
		-lgc -lpthread -lm

lib/liblocalcompilation/liblocalcompilation.a : lib/liblocalcompilation/obj/mmarket.o lib/liblocalcompilation/obj/db/init.o lib/liblocalcompilation/obj/db/calendar.o lib/liblocalcompilation/obj/data/MarketDay.o lib/liblocalcompilation/obj/data/Timetable.o lib/liblocalcompilation/obj/DEFS.o lib/liblocalcompilation/obj/db/dailyqs.o lib/liblocalcompilation/obj/data/Qtable.o lib/liblocalcompilation/obj/data/Nick.o lib/liblocalcompilation/obj/db/global.o lib/liblocalcompilation/obj/db/log.o lib/liblocalcompilation/obj/data/LogEntry.o lib/liblocalcompilation/obj/db/nicks.o lib/liblocalcompilation/obj/db/quotes.o lib/liblocalcompilation/obj/data/Quote.o lib/liblocalcompilation/obj/data/Emsg.o lib/liblocalcompilation/obj/db/servers.o lib/liblocalcompilation/obj/net/net.o lib/liblocalcompilation/obj/db/sbox.o lib/liblocalcompilation/obj/data/Daily.o lib/liblocalcompilation/obj/data/Historic.o lib/liblocalcompilation/obj/data/Server.o lib/liblocalcompilation/obj/db/fleas/flog.o lib/liblocalcompilation/obj/db/fleas/fmodels.o lib/liblocalcompilation/obj/data/flea/fmodels.o lib/liblocalcompilation/obj/data/flea/Fmodel.o lib/liblocalcompilation/obj/data/broker.o lib/liblocalcompilation/obj/data/flea/Dorder.o lib/liblocalcompilation/obj/data/flea/Flea.o lib/liblocalcompilation/obj/data/flea/Gen.o lib/liblocalcompilation/obj/data/flea/results/Rs.o lib/liblocalcompilation/obj/data/flea/models/Approx.o lib/liblocalcompilation/obj/scheduler/fleas.o lib/liblocalcompilation/obj/data/flea/Investor.o lib/liblocalcompilation/obj/db/fleas/ranking.o lib/liblocalcompilation/obj/server.o lib/liblocalcompilation/obj/data/activity.o lib/liblocalcompilation/obj/server/hub.o lib/liblocalcompilation/obj/server/main/main.o lib/liblocalcompilation/obj/server/home/main.o lib/liblocalcompilation/obj/server/settings/main.o lib/liblocalcompilation/obj/server/settings/settings.o lib/liblocalcompilation/obj/server/settings/changepass.o lib/liblocalcompilation/obj/server/settings/calendar.o lib/liblocalcompilation/obj/server/fleas/main.o lib/liblocalcompilation/obj/server/fleas/ftests/references.o lib/liblocalcompilation/obj/server/fleas/ftests/orders.o lib/liblocalcompilation/obj/server/fleas/ftests/selection.o lib/liblocalcompilation/obj/server/fleas/charts/main.o lib/liblocalcompilation/obj/server/fleas/charts/summary.o lib/liblocalcompilation/obj/chart/summary.o lib/liblocalcompilation/obj/server/fleas/charts/companies.o lib/liblocalcompilation/obj/scheduler.o
	ar rcs lib/liblocalcompilation/liblocalcompilation.a lib/liblocalcompilation/obj/mmarket.o lib/liblocalcompilation/obj/db/init.o lib/liblocalcompilation/obj/db/calendar.o lib/liblocalcompilation/obj/data/MarketDay.o lib/liblocalcompilation/obj/data/Timetable.o lib/liblocalcompilation/obj/DEFS.o lib/liblocalcompilation/obj/db/dailyqs.o lib/liblocalcompilation/obj/data/Qtable.o lib/liblocalcompilation/obj/data/Nick.o lib/liblocalcompilation/obj/db/global.o lib/liblocalcompilation/obj/db/log.o lib/liblocalcompilation/obj/data/LogEntry.o lib/liblocalcompilation/obj/db/nicks.o lib/liblocalcompilation/obj/db/quotes.o lib/liblocalcompilation/obj/data/Quote.o lib/liblocalcompilation/obj/data/Emsg.o lib/liblocalcompilation/obj/db/servers.o lib/liblocalcompilation/obj/net/net.o lib/liblocalcompilation/obj/db/sbox.o lib/liblocalcompilation/obj/data/Daily.o lib/liblocalcompilation/obj/data/Historic.o lib/liblocalcompilation/obj/data/Server.o lib/liblocalcompilation/obj/db/fleas/flog.o lib/liblocalcompilation/obj/db/fleas/fmodels.o lib/liblocalcompilation/obj/data/flea/fmodels.o lib/liblocalcompilation/obj/data/flea/Fmodel.o lib/liblocalcompilation/obj/data/broker.o lib/liblocalcompilation/obj/data/flea/Dorder.o lib/liblocalcompilation/obj/data/flea/Flea.o lib/liblocalcompilation/obj/data/flea/Gen.o lib/liblocalcompilation/obj/data/flea/results/Rs.o lib/liblocalcompilation/obj/data/flea/models/Approx.o lib/liblocalcompilation/obj/scheduler/fleas.o lib/liblocalcompilation/obj/data/flea/Investor.o lib/liblocalcompilation/obj/db/fleas/ranking.o lib/liblocalcompilation/obj/server.o lib/liblocalcompilation/obj/data/activity.o lib/liblocalcompilation/obj/server/hub.o lib/liblocalcompilation/obj/server/main/main.o lib/liblocalcompilation/obj/server/home/main.o lib/liblocalcompilation/obj/server/settings/main.o lib/liblocalcompilation/obj/server/settings/settings.o lib/liblocalcompilation/obj/server/settings/changepass.o lib/liblocalcompilation/obj/server/settings/calendar.o lib/liblocalcompilation/obj/server/fleas/main.o lib/liblocalcompilation/obj/server/fleas/ftests/references.o lib/liblocalcompilation/obj/server/fleas/ftests/orders.o lib/liblocalcompilation/obj/server/fleas/ftests/selection.o lib/liblocalcompilation/obj/server/fleas/charts/main.o lib/liblocalcompilation/obj/server/fleas/charts/summary.o lib/liblocalcompilation/obj/chart/summary.o lib/liblocalcompilation/obj/server/fleas/charts/companies.o lib/liblocalcompilation/obj/scheduler.o

lib/liblocalcompilation/obj/mmarket.o : include/mmarket.h src/mmarket.c include/db/init.h src/db/init.c include/DEFS.h src/DEFS.c include/server.h src/server.c include/scheduler.h src/scheduler.c
	gcc $(CFLAGS) -c src/mmarket.c -o lib/liblocalcompilation/obj/mmarket.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/db/init.o : include/db/init.h src/db/init.c include/db/calendar.h src/db/calendar.c include/db/dailyqs.h src/db/dailyqs.c include/db/global.h src/db/global.c include/db/log.h src/db/log.c include/db/nicks.h src/db/nicks.c include/db/quotes.h src/db/quotes.c include/db/sbox.h src/db/sbox.c include/db/servers.h src/db/servers.c include/db/fleas/flog.h src/db/fleas/flog.c include/db/fleas/fmodels.h src/db/fleas/fmodels.c include/db/fleas/ranking.h src/db/fleas/ranking.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/db/init.c -o lib/liblocalcompilation/obj/db/init.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/db/calendar.o : include/db/calendar.h src/db/calendar.c include/data/MarketDay.h src/data/MarketDay.c include/data/Timetable.h src/data/Timetable.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/db/calendar.c -o lib/liblocalcompilation/obj/db/calendar.o \
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

lib/liblocalcompilation/obj/DEFS.o : include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/DEFS.c -o lib/liblocalcompilation/obj/DEFS.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/db/dailyqs.o : include/db/dailyqs.h src/db/dailyqs.c include/DEFS.h src/DEFS.c include/data/Qtable.h src/data/Qtable.c
	gcc $(CFLAGS) -c src/db/dailyqs.c -o lib/liblocalcompilation/obj/db/dailyqs.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Qtable.o : include/data/Qtable.h src/data/Qtable.c include/data/Nick.h src/data/Nick.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/data/Qtable.c -o lib/liblocalcompilation/obj/data/Qtable.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Nick.o : include/data/Nick.h src/data/Nick.c
	gcc $(CFLAGS) -c src/data/Nick.c -o lib/liblocalcompilation/obj/data/Nick.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/db/global.o : include/db/global.h src/db/global.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/db/global.c -o lib/liblocalcompilation/obj/db/global.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/db/log.o : include/db/log.h src/db/log.c include/data/LogEntry.h src/data/LogEntry.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/db/log.c -o lib/liblocalcompilation/obj/db/log.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/LogEntry.o : include/data/LogEntry.h src/data/LogEntry.c
	gcc $(CFLAGS) -c src/data/LogEntry.c -o lib/liblocalcompilation/obj/data/LogEntry.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/db/nicks.o : include/db/nicks.h src/db/nicks.c include/db/quotes.h src/db/quotes.c include/db/servers.h src/db/servers.c include/DEFS.h src/DEFS.c include/data/Nick.h src/data/Nick.c
	gcc $(CFLAGS) -c src/db/nicks.c -o lib/liblocalcompilation/obj/db/nicks.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/db/quotes.o : include/db/quotes.h src/db/quotes.c include/db/nicks.h src/db/nicks.c include/db/log.h src/db/log.c include/data/Nick.h src/data/Nick.c include/DEFS.h src/DEFS.c include/data/Quote.h src/data/Quote.c include/data/Emsg.h src/data/Emsg.c include/data/Qtable.h src/data/Qtable.c
	gcc $(CFLAGS) -c src/db/quotes.c -o lib/liblocalcompilation/obj/db/quotes.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Quote.o : include/data/Quote.h src/data/Quote.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/data/Quote.c -o lib/liblocalcompilation/obj/data/Quote.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Emsg.o : include/data/Emsg.h src/data/Emsg.c
	gcc $(CFLAGS) -c src/data/Emsg.c -o lib/liblocalcompilation/obj/data/Emsg.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/db/servers.o : include/db/servers.h src/db/servers.c include/db/log.h src/db/log.c include/db/nicks.h src/db/nicks.c include/net/net.h src/net/net.c include/DEFS.h src/DEFS.c include/data/Server.h src/data/Server.c
	gcc $(CFLAGS) -c src/db/servers.c -o lib/liblocalcompilation/obj/db/servers.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/net/net.o : include/net/net.h src/net/net.c include/db/log.h src/db/log.c include/db/servers.h src/db/servers.c include/db/quotes.h src/db/quotes.c include/db/nicks.h src/db/nicks.c include/db/global.h src/db/global.c include/db/dailyqs.h src/db/dailyqs.c include/db/sbox.h src/db/sbox.c include/data/Daily.h src/data/Daily.c include/data/Historic.h src/data/Historic.c include/data/Server.h src/data/Server.c include/DEFS.h src/DEFS.c include/data/Emsg.h src/data/Emsg.c
	gcc $(CFLAGS) -c src/net/net.c -o lib/liblocalcompilation/obj/net/net.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/db/sbox.o : include/db/sbox.h src/db/sbox.c include/db/servers.h src/db/servers.c include/db/log.h src/db/log.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/db/sbox.c -o lib/liblocalcompilation/obj/db/sbox.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Daily.o : include/data/Daily.h src/data/Daily.c
	gcc $(CFLAGS) -c src/data/Daily.c -o lib/liblocalcompilation/obj/data/Daily.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Historic.o : include/data/Historic.h src/data/Historic.c include/data/Quote.h src/data/Quote.c
	gcc $(CFLAGS) -c src/data/Historic.c -o lib/liblocalcompilation/obj/data/Historic.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/Server.o : include/data/Server.h src/data/Server.c include/data/Nick.h src/data/Nick.c include/data/Daily.h src/data/Daily.c include/data/Historic.h src/data/Historic.c include/data/Quote.h src/data/Quote.c
	gcc $(CFLAGS) -c src/data/Server.c -o lib/liblocalcompilation/obj/data/Server.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/db/fleas/flog.o : include/db/fleas/flog.h src/db/fleas/flog.c include/data/LogEntry.h src/data/LogEntry.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/db/fleas/flog.c -o lib/liblocalcompilation/obj/db/fleas/flog.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/db/fleas/fmodels.o : include/db/fleas/fmodels.h src/db/fleas/fmodels.c include/data/flea/fmodels.h src/data/flea/fmodels.c include/scheduler/fleas.h src/scheduler/fleas.c include/DEFS.h src/DEFS.c include/data/flea/Fmodel.h src/data/flea/Fmodel.c
	gcc $(CFLAGS) -c src/db/fleas/fmodels.c -o lib/liblocalcompilation/obj/db/fleas/fmodels.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/flea/fmodels.o : include/data/flea/fmodels.h src/data/flea/fmodels.c include/data/flea/Fmodel.h src/data/flea/Fmodel.c include/data/flea/models/Approx.h src/data/flea/models/Approx.c
	gcc $(CFLAGS) -c src/data/flea/fmodels.c -o lib/liblocalcompilation/obj/data/flea/fmodels.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/flea/Fmodel.o : include/data/flea/Fmodel.h src/data/flea/Fmodel.c include/data/broker.h src/data/broker.c include/data/Nick.h src/data/Nick.c include/data/flea/Dorder.h src/data/flea/Dorder.c include/DEFS.h src/DEFS.c include/data/Quote.h src/data/Quote.c include/data/Qtable.h src/data/Qtable.c include/data/flea/Flea.h src/data/flea/Flea.c include/data/flea/results/Rs.h src/data/flea/results/Rs.c
	gcc $(CFLAGS) -c src/data/flea/Fmodel.c -o lib/liblocalcompilation/obj/data/flea/Fmodel.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/broker.o : include/data/broker.h src/data/broker.c
	gcc $(CFLAGS) -c src/data/broker.c -o lib/liblocalcompilation/obj/data/broker.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/flea/Dorder.o : include/data/flea/Dorder.h src/data/flea/Dorder.c
	gcc $(CFLAGS) -c src/data/flea/Dorder.c -o lib/liblocalcompilation/obj/data/flea/Dorder.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/flea/Flea.o : include/data/flea/Flea.h src/data/flea/Flea.c include/DEFS.h src/DEFS.c include/data/flea/Gen.h src/data/flea/Gen.c
	gcc $(CFLAGS) -c src/data/flea/Flea.c -o lib/liblocalcompilation/obj/data/flea/Flea.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/flea/Gen.o : include/data/flea/Gen.h src/data/flea/Gen.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/data/flea/Gen.c -o lib/liblocalcompilation/obj/data/flea/Gen.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/flea/results/Rs.o : include/data/flea/results/Rs.h src/data/flea/results/Rs.c
	gcc $(CFLAGS) -c src/data/flea/results/Rs.c -o lib/liblocalcompilation/obj/data/flea/results/Rs.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/flea/models/Approx.o : include/data/flea/models/Approx.h src/data/flea/models/Approx.c include/DEFS.h src/DEFS.c include/data/flea/Fmodel.h src/data/flea/Fmodel.c
	gcc $(CFLAGS) -c src/data/flea/models/Approx.c -o lib/liblocalcompilation/obj/data/flea/models/Approx.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/scheduler/fleas.o : include/scheduler/fleas.h src/scheduler/fleas.c include/data/flea/fmodels.h src/data/flea/fmodels.c include/data/flea/Investor.h src/data/flea/Investor.c include/db/quotes.h src/db/quotes.c include/db/fleas/flog.h src/db/fleas/flog.c include/db/fleas/fmodels.h src/db/fleas/fmodels.c include/db/fleas/ranking.h src/db/fleas/ranking.c include/db/log.h src/db/log.c include/DEFS.h src/DEFS.c include/data/flea/Fmodel.h src/data/flea/Fmodel.c
	gcc $(CFLAGS) -c src/scheduler/fleas.c -o lib/liblocalcompilation/obj/scheduler/fleas.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/flea/Investor.o : include/data/flea/Investor.h src/data/flea/Investor.c include/data/flea/fmodels.h src/data/flea/fmodels.c include/data/flea/Flea.h src/data/flea/Flea.c include/data/flea/Fmodel.h src/data/flea/Fmodel.c include/scheduler/fleas.h src/scheduler/fleas.c
	gcc $(CFLAGS) -c src/data/flea/Investor.c -o lib/liblocalcompilation/obj/data/flea/Investor.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/db/fleas/ranking.o : include/db/fleas/ranking.h src/db/fleas/ranking.c include/data/flea/Investor.h src/data/flea/Investor.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/db/fleas/ranking.c -o lib/liblocalcompilation/obj/db/fleas/ranking.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server.o : include/server.h src/server.c include/data/activity.h src/data/activity.c include/db/log.h src/db/log.c include/server/hub.h src/server/hub.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/server.c -o lib/liblocalcompilation/obj/server.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/data/activity.o : include/data/activity.h src/data/activity.c
	gcc $(CFLAGS) -c src/data/activity.c -o lib/liblocalcompilation/obj/data/activity.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/hub.o : include/server/hub.h src/server/hub.c include/server/main/main.h src/server/main/main.c include/server/home/main.h src/server/home/main.c include/server/settings/main.h src/server/settings/main.c include/server/fleas/main.h src/server/fleas/main.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/server/hub.c -o lib/liblocalcompilation/obj/server/hub.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/main/main.o : include/server/main/main.h src/server/main/main.c include/db/global.h src/db/global.c
	gcc $(CFLAGS) -c src/server/main/main.c -o lib/liblocalcompilation/obj/server/main/main.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/home/main.o : include/server/home/main.h src/server/home/main.c include/db/log.h src/db/log.c
	gcc $(CFLAGS) -c src/server/home/main.c -o lib/liblocalcompilation/obj/server/home/main.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/settings/main.o : include/server/settings/main.h src/server/settings/main.c include/server/settings/settings.h src/server/settings/settings.c include/server/settings/changepass.h src/server/settings/changepass.c include/server/settings/calendar.h src/server/settings/calendar.c
	gcc $(CFLAGS) -c src/server/settings/main.c -o lib/liblocalcompilation/obj/server/settings/main.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/settings/settings.o : include/server/settings/settings.h src/server/settings/settings.c include/db/global.h src/db/global.c
	gcc $(CFLAGS) -c src/server/settings/settings.c -o lib/liblocalcompilation/obj/server/settings/settings.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/settings/changepass.o : include/server/settings/changepass.h src/server/settings/changepass.c
	gcc $(CFLAGS) -c src/server/settings/changepass.c -o lib/liblocalcompilation/obj/server/settings/changepass.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/settings/calendar.o : include/server/settings/calendar.h src/server/settings/calendar.c include/data/MarketDay.h src/data/MarketDay.c include/db/global.h src/db/global.c include/db/calendar.h src/db/calendar.c
	gcc $(CFLAGS) -c src/server/settings/calendar.c -o lib/liblocalcompilation/obj/server/settings/calendar.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/fleas/main.o : include/server/fleas/main.h src/server/fleas/main.c include/server/fleas/ftests/references.h src/server/fleas/ftests/references.c include/server/fleas/ftests/orders.h src/server/fleas/ftests/orders.c include/server/fleas/ftests/selection.h src/server/fleas/ftests/selection.c include/server/fleas/charts/main.h src/server/fleas/charts/main.c include/server/fleas/charts/summary.h src/server/fleas/charts/summary.c include/server/fleas/charts/companies.h src/server/fleas/charts/companies.c include/data/flea/fmodels.h src/data/flea/fmodels.c include/data/flea/Fmodel.h src/data/flea/Fmodel.c
	gcc $(CFLAGS) -c src/server/fleas/main.c -o lib/liblocalcompilation/obj/server/fleas/main.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/fleas/ftests/references.o : include/server/fleas/ftests/references.h src/server/fleas/ftests/references.c include/data/flea/fmodels.h src/data/flea/fmodels.c include/data/flea/Fmodel.h src/data/flea/Fmodel.c include/data/Nick.h src/data/Nick.c include/db/quotes.h src/db/quotes.c include/db/nicks.h src/db/nicks.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/server/fleas/ftests/references.c -o lib/liblocalcompilation/obj/server/fleas/ftests/references.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/fleas/ftests/orders.o : include/server/fleas/ftests/orders.h src/server/fleas/ftests/orders.c include/data/flea/fmodels.h src/data/flea/fmodels.c include/data/flea/Fmodel.h src/data/flea/Fmodel.c include/data/Nick.h src/data/Nick.c include/db/quotes.h src/db/quotes.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/server/fleas/ftests/orders.c -o lib/liblocalcompilation/obj/server/fleas/ftests/orders.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/fleas/ftests/selection.o : include/server/fleas/ftests/selection.h src/server/fleas/ftests/selection.c include/data/flea/fmodels.h src/data/flea/fmodels.c include/data/flea/Fmodel.h src/data/flea/Fmodel.c include/db/fleas/flog.h src/db/fleas/flog.c include/db/quotes.h src/db/quotes.c include/scheduler/fleas.h src/scheduler/fleas.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/server/fleas/ftests/selection.c -o lib/liblocalcompilation/obj/server/fleas/ftests/selection.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/fleas/charts/main.o : include/server/fleas/charts/main.h src/server/fleas/charts/main.c include/data/flea/fmodels.h src/data/flea/fmodels.c include/data/flea/Fmodel.h src/data/flea/Fmodel.c include/db/fleas/fmodels.h src/db/fleas/fmodels.c include/scheduler/fleas.h src/scheduler/fleas.c include/DEFS.h src/DEFS.c
	gcc $(CFLAGS) -c src/server/fleas/charts/main.c -o lib/liblocalcompilation/obj/server/fleas/charts/main.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/fleas/charts/summary.o : include/server/fleas/charts/summary.h src/server/fleas/charts/summary.c include/data/flea/Fmodel.h src/data/flea/Fmodel.c include/data/flea/fmodels.h src/data/flea/fmodels.c include/db/quotes.h src/db/quotes.c include/chart/summary.h src/chart/summary.c
	gcc $(CFLAGS) -c src/server/fleas/charts/summary.c -o lib/liblocalcompilation/obj/server/fleas/charts/summary.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/chart/summary.o : include/chart/summary.h src/chart/summary.c include/db/quotes.h src/db/quotes.c include/data/Qtable.h src/data/Qtable.c include/data/broker.h src/data/broker.c include/data/flea/Dorder.h src/data/flea/Dorder.c include/DEFS.h src/DEFS.c include/data/flea/Fmodel.h src/data/flea/Fmodel.c include/data/flea/Flea.h src/data/flea/Flea.c
	gcc $(CFLAGS) -c src/chart/summary.c -o lib/liblocalcompilation/obj/chart/summary.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/server/fleas/charts/companies.o : include/server/fleas/charts/companies.h src/server/fleas/charts/companies.c
	gcc $(CFLAGS) -c src/server/fleas/charts/companies.c -o lib/liblocalcompilation/obj/server/fleas/charts/companies.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

lib/liblocalcompilation/obj/scheduler.o : include/scheduler.h src/scheduler.c include/data/activity.h src/data/activity.c include/net/net.h src/net/net.c include/db/calendar.h src/db/calendar.c include/db/log.h src/db/log.c include/db/global.h src/db/global.c include/DEFS.h src/DEFS.c include/scheduler/fleas.h src/scheduler/fleas.c
	gcc $(CFLAGS) -c src/scheduler.c -o lib/liblocalcompilation/obj/scheduler.o \
		-Iinclude \
		-I$(LIB_INCLUDE_dmc)

