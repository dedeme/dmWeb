// Copyright 16-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Map structure

#ifndef DMC_VMAP_H
  #define DMC_VMAP_H

#include "dmc/Varr.h"
#include "dmc/Arr.h"

/// Vmap entries type.<br>
/// For example:
///   EACH(map, Vkv, e)
///   ...
///   _EACH
typedef struct vmap_Kv Vkv;

///
typedef struct vmap_Vmap Vmap;

/// vmap_new initializates a map. VMap can be cast to Arr.
Vmap *vmap_new(void);

///
void vmap_free(Vmap *this);

///
int vmap_size(Vmap *this);

/// vmap_put puts 'value' with key 'key'. If key already exists its value
/// is changed.
///   this  : The map
///   key   : Entry key. It must not be NULL.
///   value : New value
void vmap_put(Vmap *this, const char *key, void *value);

/// vmap_get_null returns the value pointed by key or NULL if 'key' does
/// not exist
void *vmap_get_null(Vmap *this, const char *key);

/// vmap_remove removes value with key 'key' or does nothing if 'key' does
/// not exist
void vmap_remove(Vmap *this, const char *key);

/// vmap_keys_new returns keys of this in a Varr[char]
Varr *vmap_keys_new(Vmap *this);

/// Equals to (Arr *)this.<br>
/// Returns an Arr[Vkv]
Arr *vmap_kvs(Vmap *this);

/// vmap_key returns the key of entry
char *vmap_key(Vkv *entry);

/// vmap_key returns the value of entry
void *vmap_value(Vkv *entry);

/// vmap_sort sorts 'this' from keys
void vmap_sort(Vmap *this);

/// vmap_sort_locale sorts 'this' in locale from keys
void vmap_sort_locale(Vmap *this);

#endif
