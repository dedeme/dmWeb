
CFLAGS = -Wall -rdynamic
OBJECTS = $(subst src,lib/obj,$(patsubst %.c,%.o,$(wildcard src/*.c)))

DIRS = data data/Result data/Bet data/Tbet data/Strategy data/Decision \
       data/Profits db tests net web web/profits strategies
OBJECTS2 = $(foreach DIR, $(DIRS),\
  $(subst src/$(DIR)/,lib/obj/$(subst /,_,$(DIR)_),\
	$(patsubst %.c,%.o,$(wildcard src/$(DIR)/*.c))))

bin/Bet1x2 : lib/libBet1x2.a
	gcc $(CFLAGS) src/Bet1x2.c -o bin/Bet1x2 \
		-Iinclude \
		-Ilib/libtx/include \
		-Ilib/libdmc/include \
		-Llib -lBet1x2 \
		-Llib/libdmc -ldmc \
		-Llib/libtx -ltx \
		-lgc -lm

lib/libBet1x2.a : $(OBJECTS) $(OBJECTS2)
	ar rcs lib/libBet1x2.a $(OBJECTS) $(OBJECTS2)

lib/obj/%.o : src/%.c include/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_%.o : src/data/%.c include/data/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_Result_%.o : src/data/Result/%.c include/data/Result/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_Bet_%.o : src/data/Bet/%.c include/data/Bet/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_Tbet_%.o : src/data/Tbet/%.c include/data/Tbet/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_Strategy_%.o : src/data/Strategy/%.c include/data/Strategy/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_Decision_%.o : src/data/Decision/%.c include/data/Decision/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_Profits_%.o : src/data/Profits/%.c include/data/Profits/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/db_%.o : src/db/%.c include/db/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/net_%.o : src/net/%.c include/net/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libtx/include \
		-Ilib/libdmc/include

lib/obj/web_%.o : src/web/%.c include/web/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/web_profits_%.o : src/web/profits/%.c include/web/profits/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/strategies_%.o : src/strategies/%.c include/strategies/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

