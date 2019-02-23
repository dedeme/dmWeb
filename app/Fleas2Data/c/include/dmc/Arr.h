// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Array structure.<p>
/// This structure is owner of its elements and frees them with 'arr_free".

#ifndef DMC_ARR_H
  #define DMC_ARR_H

#include "DEFS.h"

typedef struct js_Js Js;

///
typedef struct arr_Arr Arr;

/// Creates a new Array.<br>
/// 'ffree' is the function to free elements of Arr.
Arr *arr_new(void(*ffree)(void *));

/// buffer must be > 0
Arr *arr_bf_new(int buffer, void(*ffree)(void *));

/// If ix is < 0 then is changed to 'arr_size - ix'
Arr *arr_left_new(Arr *this, int ix, void *(*copy_new)(void *));

/// If ix is < 0 then is changed to 'arr_size - ix'
Arr *arr_right_new(Arr *this, int ix, void *(*copy_new)(void *));

/// If begin or end are < 0 then is changed to 'arr_size - itsValue'
Arr *arr_sub_new(Arr *this, int begin, int end, void *(*copy_new)(void *));

///
void arr_free(Arr *this);

///
int arr_size(Arr *this);

/// If ix is < 0 then is changed to 'arr_size - ix'
void *arr_get(Arr *this, int ix);

/// Return a pointer to the first element of 'this'
void **arr_start(Arr *this);

/// Returns a pointer to the next element after the last element of 'this'.
/// 'arr_end' does not point to a valid element.
void **arr_end(Arr *this);

/// Returns the 'free' function for elements
FPROC arr_ffree(Arr *this);
//void (*arr_ffree(Arr *this))(void *);

/// Adds an element at the end of 'this'. 'e' will be freed by 'this'.
void arr_push(Arr *this, void *e);

/// Returns and removes the last element.
void *arr_pop_new(Arr *this);

/// Returns the las element.
void *arr_peek(Arr *this);

/// If ix is < 0 then is changed to 'arr_size - ix'.<br>
/// 'e' will be freed by 'this'.
void arr_set(Arr *this, int ix, void *e);

/// If ix is < 0 then is changed to 'arr_size - ix'.<br>
/// 'e' will be freed by 'this'.
void arr_insert(Arr *this, int ix, void *e);

/// If ix is < 0 then is changed to 'arr_size - ix'
void arr_remove(Arr *this, int ix);

/// Adds other to 'this' using the function 'copy_new'
void arr_cat(Arr *this, Arr *other, void *(*copy_new)(void *));

/// If ix is < 0 then is changed to 'arr_size - ix'
void arr_insert_arr(Arr *this, int ix, Arr *other, void *(*copy_new)(void *));

/// If begin or end are < 0 then is changed to 'arr_size - itsValue'
void arr_remove_range(Arr *this, int begin, int end);

/// Reverses elements of 'this'
void arr_reverse(Arr *this);

/// Sorts 'this' using the function 'greater' that returns 'true' if
/// 'e1' > 'e2'
void arr_sort(Arr *this, int (*greater)(void *e1, void *e2));

/// arr_shuflle remix 'this' elements. It should be used after calling
/// rnd_init() or sys_init()
void arr_shuffle(Arr *this);

/// arr_index returns the index of the first elements which returns 'true'
/// with 'pred', or -1 if such element does not exist.
int arr_index(Arr *this, int (*pred)(void *e));

/// arr_filter removes every element which returns 'false' with 'pred'.
void arr_filter(Arr *this, int (*pred)(void *e));

/// 'to_new' returns a Js from an element of 'this'
Js *arr_to_js_new(Arr *this, Js *(*to_new)(void *e));

/// 'from_new' parse a Js to an element of 'this'.<br>
/// 'free' frees an element of 'this'
Arr *arr_from_js_new(
  Js *js,
  void *(*from_new)(Js *jse),
  void (*ffree)(void *e)
);

#endif
