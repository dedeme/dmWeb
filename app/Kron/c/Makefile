
CFLAGS = -Wall -rdynamic
OBJECTS = $(subst src,lib/obj,$(patsubst %.c,%.o,$(wildcard src/*.c)))

DIRS = data data/LogEntry data/Ann data/Exe db pgs procs
OBJECTS2 = $(foreach DIR, $(DIRS),\
  $(subst src/$(DIR)/,lib/obj/$(subst /,_,$(DIR)_),\
	$(patsubst %.c,%.o,$(wildcard src/$(DIR)/*.c))))

bin/Kron : lib/libKron.a
	gcc $(CFLAGS) src/Kron.c -o bin/Kron \
		-Iinclude \
		-Ilib/libdmc/include \
		-Llib -lKron \
		-Llib/libdmc -ldmc \
		-lgc -lpthread -lm

lib/libKron.a : $(OBJECTS) $(OBJECTS2)
	ar rcs lib/libKron.a $(OBJECTS) $(OBJECTS2)

lib/obj/%.o : src/%.c include/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_%.o : src/data/%.c include/data/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_LogEntry_%.o : src/data/LogEntry/%.c include/data/LogEntry/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_Ann_%.o : src/data/Ann/%.c include/data/Ann/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_Exe_%.o : src/data/Exe/%.c include/data/Exe/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/db_%.o : src/db/%.c include/db/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/pgs_%.o : src/pgs/%.c include/pgs/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/procs_%.o : src/procs/%.c include/procs/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

