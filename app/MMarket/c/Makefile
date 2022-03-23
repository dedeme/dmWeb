
CFLAGS = -Wall -rdynamic
OBJECTS = $(subst src,lib/obj,$(patsubst %.c,%.o,$(wildcard src/*.c)))

DIRS = data data/Model data/models data/modelDocs data/ADouble data/Order \
			 data/eval data/eval/ModelEval data/simulation \
			 data/simulation/SimProfitsRow pgs external db
OBJECTS2 = $(foreach DIR, $(DIRS),\
  $(subst src/$(DIR)/,lib/obj/$(subst /,_,$(DIR)_),\
	$(patsubst %.c,%.o,$(wildcard src/$(DIR)/*.c))))

bin/MMarket : lib/libMMarket.a
	gcc $(CFLAGS) src/MMarket.c -o bin/MMarket \
		-Iinclude \
		-Ilib/libdmc/include \
		-Llib -lMMarket \
		-Llib/libdmc -ldmc \
		-lgc -lpthread -lm

lib/libMMarket.a : $(OBJECTS) $(OBJECTS2)
	ar rcs lib/libMMarket.a $(OBJECTS) $(OBJECTS2)

lib/obj/%.o : src/%.c include/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_%.o : src/data/%.c include/data/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_Model_%.o : src/data/Model/%.c include/data/Model/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_models_%.o : src/data/models/%.c include/data/models/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_ADouble_%.o : src/data/ADouble/%.c include/data/ADouble/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_Order_%.o : src/data/Order/%.c include/data/Order/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_modelDocs_%.o : src/data/modelDocs/%.c include/data/modelDocs/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_eval_%.o : src/data/eval/%.c include/data/eval/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_eval_ModelEval_%.o : src/data/eval/ModelEval/%.c include/data/eval/ModelEval/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_simulation_%.o : src/data/simulation/%.c include/data/simulation/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/data_simulation_SimProfitsRow_%.o : src/data/simulation/SimProfitsRow/%.c include/data/simulation/SimProfitsRow/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/pgs_%.o : src/pgs/%.c include/pgs/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/external_%.o : src/external/%.c include/external/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

lib/obj/db_%.o : src/db/%.c include/db/%.h
	gcc $(CFLAGS) -c $< -o $@ \
		-Iinclude \
		-Ilib/libdmc/include

