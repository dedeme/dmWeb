// Copyright 16-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Map structure

#ifndef DMC_MAP_H
  #define DMC_MAP_H

#include "dmc/Arr.h"
#include "dmc/Varr.h"

/// Map entries type.<br>
/// For example:
///   EACH(map, Kv, e)
///   ...
///   _EACH
typedef struct map_Kv Kv;

///
typedef struct map_Map Map;

/// map_new initializates a map. Map can be cast to Arr.<br>
/// 'ffree' is the function to free values of Map.
Map *map_new(void(*ffree)(void *));

///
void map_free(Map *this);

///
int map_size(Map *this);

/// map_put puts 'value' with key 'key'. If key already exists its value
/// is changed.
///   this  : The map
///   key   : Entry key. It must not be NULL.
///   value : New value
void map_put(Map *this, const char *key, void *value);

/// map_get_null returns the value pointed by key or NULL if 'key' does
/// not exist
void *map_get_null(Map *this, const char *key);

/// map_remove removes value with key 'key' or does nothing if 'key' does
/// not exist
void map_remove(Map *this, const char *key);

/// map_keys_new returns keys of this in a Varr[char]
Varr *map_keys_new(Map *this);

/// Equals to (Arr *)this.<br>
/// Returns an Arr[Kv]
Arr *map_kvs(Map *this);

/// map_key returns the key of entry
char *map_key(Kv *entry);

/// map_key returns the value of entry
void *map_value(Kv *entry);

/// map_sort sorts 'this' from keys
void map_sort(Map *this);

/// map_sort_locale sorts 'this' in locale from keys
void map_sort_locale(Map *this);

/// 'to_new' returns a Js from a value of 'this'
Js *map_to_js_new(Map *this, Js *(*to_new)(void *e));

/// 'from_new' parse a Js to a value of 'this'.<br>
/// 'free' frees a value of 'this'
Map *map_from_js_new(
  Js *js,
  void *(*from_new)(Js *jse),
  void (*ffree)(void *e)
);


#endif
