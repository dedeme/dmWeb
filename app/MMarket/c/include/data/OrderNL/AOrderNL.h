// Copyright 29-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Arr[OrderNL *].

#ifndef DATA_ORDERNL_AORDERNL_H
  #define DATA_ORDERNL_AORDERNL_H

#include "dmc/Arr.h"
#include "data/OrderNL/OOrderNL.h"

#include "data/OrderNL.h"

/// Arr[OrderNL *].
struct aOrderNL_AOrderNL {
  OrderNL **es; // Start elements.
  OrderNL **end; // End elements. (Elements are between 'es' (inclusive) and 'end'
              // exclusive.
  OrderNL **endbf; // End buffer.
};

/// Arr[OrderNL *].
typedef struct aOrderNL_AOrderNL AOrderNL;

/// Creates a new Array with buffer size of 15 elements.
AOrderNL *aOrderNL_new (void);

/// 'buffer' must be > 0.
AOrderNL *aOrderNL_bf_new (int buffer);

/// Creates a new array from several elements.
/// Elements list must finish with NULL.
AOrderNL *aOrderNL_new_from (OrderNL *e, ...);

/// Creates a new array from a C array. For example:
///   Arr *a = arr_new_c(3, (void *[]){"c", "d", "e"});
/// If 'size' is less than C array length, result is ok (only will be
/// used 'size' first elements); but if 'size' is greater, the result is
/// undetermined.
AOrderNL *aOrderNL_new_c (int size, OrderNL **es);

/// Returns a new array with elements of 'this'.
AOrderNL *aOrderNL_copy (AOrderNL *this);

///
int aOrderNL_size (AOrderNL *this);

/// Resturn the element at position ix.
OrderNL *aOrderNL_get (AOrderNL *this, int ix);

/// Adds an element at the end of 'this'. 'e' will be freed by 'this'.
void aOrderNL_push (AOrderNL *this, OrderNL *e);

/// Returns and removes the last element.
OrderNL *aOrderNL_pop (AOrderNL *this);

/// Returns the las element.
OrderNL *aOrderNL_peek (AOrderNL *this);

/// Sets the element at position ix.
void aOrderNL_set (AOrderNL *this, int ix, OrderNL *e);

/// Inserts an element at position ix.
void aOrderNL_insert (AOrderNL *this, int ix, OrderNL *e);

/// Removes an element at position ix. Buffer size of 'this' does not change.
void aOrderNL_remove (AOrderNL *this, int ix);

/// Adds pointer to elements of 'other' to 'this'.
void aOrderNL_cat (AOrderNL *this, AOrderNL *other);

/// Inserts pointer to elements of 'other' at 'ix'
void aOrderNL_insert_arr (AOrderNL *this, int ix, AOrderNL *other);

/// Removes elements between [begin-end). Buffer size of 'this' does not change.
void aOrderNL_remove_range (AOrderNL *this, int begin, int end);

/// Removes every element of 'this'.
void aOrderNL_clear (AOrderNL *this);

/// Reverses elements of 'this'.
void aOrderNL_reverse (AOrderNL *this);

/// Sorts 'this' ascendantly using the function 'greater' that returns '1'
/// if 'e1' > 'e2'.
void aOrderNL_sort (AOrderNL *this, int (*greater)(OrderNL *e1, OrderNL *e2));

/// aOrderNL_shuflle remix 'this' elements. It should be used after calling
/// rnd_init() or sys_init().
void aOrderNL_shuffle (AOrderNL *this);

/// Returns '1' if every element of 'this' yields '1' with 'pred'.
int aOrderNL_all (AOrderNL *this, int (*pred)(OrderNL *e));

/// Returns '1' if some element of 'this' yields '1' with 'pred'.
int aOrderNL_any (AOrderNL *this, int (*pred)(OrderNL *e));

/// Returns the index of the first elements which returns '1'
/// with 'pred', or -1 if such element does not exist.
int aOrderNL_index (AOrderNL *this, int (*pred)(OrderNL *e));

/// Returns the index of the last elements which returns '1'
/// with 'pred', or -1 if such element does not exist.
int aOrderNL_last_index (AOrderNL *this, int (*pred)(OrderNL *e));

/// Returns the first element which pruduces '1' with 'pred' or 'tp_none'.
OOrderNL *aOrderNL_find(AOrderNL *this, int (*pred)(OrderNL *e));

/// Returns the last element which pruduces '1' with 'pred' or 'tp_none'.
OOrderNL *aOrderNL_find_last(AOrderNL *this, int (*pred)(OrderNL *e));

/// aOrderNL_filter_in removes every element which returns '0' with 'pred'.
void aOrderNL_filter_in (AOrderNL *this, int (*pred)(OrderNL *e));

/// Returns a new Arr with the n first elements of this.
/// If this has less elements than n, returs a copy of this.
AOrderNL *aOrderNL_take (AOrderNL *this, int n);

/// Returns a new Arr with the first elements which return '1' with 'predicate'.
AOrderNL *aOrderNL_takef (AOrderNL *this, int (*predicate)(OrderNL *e));

/// Returns a new Arr with elements left after aOrderNL_take.
AOrderNL *aOrderNL_drop (AOrderNL *this, int n);

/// Returns a new Arr with elements left after aOrderNL_takef.
AOrderNL *aOrderNL_dropf (AOrderNL *this, int (*predicate)(OrderNL *e));

/// Returns a new Arr with every element which returns '1' with 'pred'.
AOrderNL *aOrderNL_filter_to (AOrderNL *this, int (*predicate)(OrderNL *e));

/// Returns a new Arr with elements generated by converter.
Arr *aOrderNL_map (AOrderNL *this, void *(*converter)(OrderNL *e));

/// Returns a new Arr whit the first element generated by conv1 and the rest
/// by conv2.
Arr *aOrderNL_map2 (AOrderNL *this, void *(*conv1)(OrderNL *e), void *(*conv2)(OrderNL *e));

/// Returns a new Arr mixing values of 'a1' and 'a2'. The size of the resultant
/// array is the less of 'a1' size and 'a2' size.
Arr *aOrderNL_zip (AOrderNL *a1, AOrderNL *a2, void *(*converter)(OrderNL *e1, OrderNL *e2));

/// Returns a new Arr mixing values of 'a1', 'a2' and 'a3'. The size of the
/// resultant array is the less of 'a1' size, 'a2' size and 'a3' size.
Arr *aOrderNL_zip3 (
  AOrderNL *a1, AOrderNL *a2, AOrderNL *a3,
  void *(*conveter)(OrderNL*e1, OrderNL*e2, OrderNL*e3)
);

/// Removes duplicates with function 'feq=1' and returns them in a new array.
/// It returns only the first duplicated element.
AOrderNL *aOrderNL_duplicates (AOrderNL *this, int (feq)(OrderNL *e1, OrderNL *e2));

/// Returns this JSONized.
///   this: Container.
char *aOrderNL_to_js (AOrderNL *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
AOrderNL *aOrderNL_from_js (char *js);


//--// Not remove

#endif