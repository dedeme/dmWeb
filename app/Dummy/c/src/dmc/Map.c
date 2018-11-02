// Copyright 16-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "dmc/Map.h"

#include "dmc/std.h"

struct map_Kv {
  char *key;
  void *value;
  void (*ffree)(void *);
};

static Kv *kv_new(const char *key, void *value, void (*ffree)(void *)) {
  Kv *this = malloc(sizeof(Kv));
  this->key = str_new(key);
  this->value = value;
  this->ffree = ffree;
  return this;
}

static void kv_free(Kv *this) {
  free(this->key);
  this->ffree(this->value);
  free(this);
}

struct map_Map {
  void **es;
  void **end;
  void **endbf;
  void (*ffree)(void *);
  void (*vfree)(void *);
};

Map *map_new(void (*ffree)(void *)) {
  Map *this = malloc(sizeof(Map));
  void **es = malloc(15 * sizeof(void *));
  this->es = es;
  this->end = es;
  this->endbf = es + 15;
  this->ffree = (FPROC)kv_free;
  this->vfree = ffree;
  return this;
}

void map_free(Map *this) {
  void (*ffree)(void *) = this->ffree;
  void **p = this->es;
  void **end = this->end;
  while (p < end) {
    ffree(*p++);
  }
  free(this);
}

int map_size(Map *this) {
  return arr_size((Arr *) this);
}

void map_put(Map *this, const char *key, void *value) {
  int no_added = 1;
  EACH(this, Kv, e)
    if (str_eq(e->key, key)) {
      this->vfree(e->value);
      e->value = value;
      no_added = 0;
      break;
    }
  _EACH
  if (no_added) {
    arr_push((Arr *)this, kv_new(key, value, this->vfree));
  }
}

void *map_get_null(Map *this, const char *key) {
  EACH(this, Kv, e)
    if (str_eq(e->key, key)) {
      return e->value;
    }
  _EACH
  return NULL;
}

void map_remove(Map *this, const char *key) {
  int ix = -1;
  EACH_IX(this, Kv, e, i)
    if (str_eq(e->key, key)) {
      ix = i;
      break;
    }
  _EACH
  if (ix != -1) {
    arr_remove((Arr *)this, ix);
  }
}

// Varr[char]
Varr *map_keys_new(Map *this) {
  // Varr[char]
  Varr *r = varr_new();
  EACH(this, Kv, e)
    varr_push(r, e->key);
  _EACH
  return r;
}

Arr *map_kvs(Map *this) {
  return (Arr *)this;
}

char *map_key(Kv *entry) {
  return entry->key;
}

void *map_value(Kv *entry) {
  return entry->value;
}

void map_sort(Map *this) {
  int greater(Kv *e1, Kv *e2) {
    return str_greater(e1->key, e2->key);
  }
  varr_sort((Varr *)this, (FGREATER) greater);
}

void map_sort_locale(Map *this) {
  int greater(Kv *e1, Kv *e2) {
    return str_greater_locale(e1->key, e2->key);
  }
  varr_sort((Varr *)this, (FGREATER) greater);
}

Js *map_to_js_new(Map *this, Js *(*to_new)(void *e)) {
  // Arr[Js]
  Arr *a = arr_new(free);
  Kv **p = (Kv **)this->es;
  Kv **end = (Kv **)this->end;
  while (p < end) {
    Kv *kv = *p++;
    // Arr[Js]
    Arr *akv = arr_new(free);
    arr_push(akv, js_ws_new(kv->key));
    arr_push(akv, to_new(kv->value));
    arr_push(a, js_wa_new(akv));
    arr_free(akv);
  }
  Js *r = js_wa_new(a);
  arr_free(a);
  return r;
}

Map *map_from_js_new(
  Js *js,
  void *(*from_new)(Js *jse),
  void (*ffree)(void *e)
) {
  Map *this = map_new(ffree);
  // Arr[Js]
  Arr *a = js_ra_new(js);
  Js **p = (Js **)arr_start(a);
  Js **end = (Js **)arr_end(a);
  while (p < end) {
    // Arr[Js]
    Arr *akv = js_ra_new(*p++);
    char *key = js_rs_new(arr_get(akv, 0));
    void *value = from_new(arr_get(akv, 1));
    map_put(this, key, value);
    free(key);
    arr_free(akv);
  }
  arr_free(a);
  return this;
}
