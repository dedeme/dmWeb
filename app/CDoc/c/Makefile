
CFLAGS = -Wall -rdynamic
OBJECTS = $(subst src,lib/obj,$(patsubst %.c,%.o,$(wildcard src/*.c)))

DIRS = data data/DocEntry data/Dpath data/IndexTree data/Doc data/DocRs \
			 db pgs readers
OBJECTS2 = $(foreach DIR, $(DIRS),\
  $(subst src/$(DIR)/,lib/obj/$(subst /,_,$(DIR)_),\
	$(patsubst %.c,%.o,$(wildcard src/$(DIR)/*.c))))

bin/CDoc : lib/libCDoc.a
	gcc $(CFLAGS) src/CDoc.c -o bin/CDoc \
		-Iinclude \
		-Ilib/libdmc/include \
		-Ilib/libtx/include \
		-Llib -lCDoc \
		-Llib/libdmc -ldmc \
		-Llib/libtx -ltx \
		-lgc -lpthread -lm

lib/libCDoc.a : $(OBJECTS) $(OBJECTS2)
	ar rcs lib/libCDoc.a $(OBJECTS) $(OBJECTS2)

lib/obj/%.o : src/%.c include/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_%.o : src/data/%.c include/data/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_DocEntry_%.o : src/data/DocEntry/%.c include/data/DocEntry/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_Dpath_%.o : src/data/Dpath/%.c include/data/Dpath/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_IndexTree_%.o : src/data/IndexTree/%.c include/data/IndexTree/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_Doc_%.o : src/data/Doc/%.c include/data/Doc/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_DocRs_%.o : src/data/DocRs/%.c include/data/DocRs/%.h
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

lib/obj/readers_%.o : src/readers/%.c include/readers/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include \
		-Ilib/libtx/include

