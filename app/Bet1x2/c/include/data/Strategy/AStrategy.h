// Copyright 28-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Arr[Strategy *].

#ifndef DATA_STRATEGY_ASTRATEGY_H
  #define DATA_STRATEGY_ASTRATEGY_H

#include "dmc/Arr.h"
#include "data/Strategy/OStrategy.h"

#include "data/Strategy.h"

/// Arr[Strategy *].
struct aStrategy_AStrategy {
  Strategy **es; // Start elements.
  Strategy **end; // End elements. (Elements are between 'es' (inclusive) and 'end'
              // exclusive.
  Strategy **endbf; // End buffer.
};

/// Arr[Strategy *].
typedef struct aStrategy_AStrategy AStrategy;

/// Creates a new Array with buffer size of 15 elements.
AStrategy *aStrategy_new (void);

/// 'buffer' must be > 0.
AStrategy *aStrategy_bf_new (int buffer);

/// Creates a new array from several elements.
/// Elements list must finish with NULL.
AStrategy *aStrategy_new_from (Strategy *e, ...);

/// Creates a new array from a C array. For example:
///   Arr *a = arr_new_c(3, (void *[]){"c", "d", "e"});
/// If 'size' is less than C array length, result is ok (only will be
/// used 'size' first elements); but if 'size' is greater, the result is
/// undetermined.
AStrategy *aStrategy_new_c (int size, Strategy **es);

/// Returns a new array with elements of 'this'.
AStrategy *aStrategy_copy (AStrategy *this);

///
int aStrategy_size (AStrategy *this);

/// Resturn the element at position ix.
Strategy *aStrategy_get (AStrategy *this, int ix);

/// Adds an element at the end of 'this'. 'e' will be freed by 'this'.
void aStrategy_push (AStrategy *this, Strategy *e);

/// Returns and removes the last element.
Strategy *aStrategy_pop (AStrategy *this);

/// Returns the las element.
Strategy *aStrategy_peek (AStrategy *this);

/// Sets the element at position ix.
void aStrategy_set (AStrategy *this, int ix, Strategy *e);

/// Inserts an element at position ix.
void aStrategy_insert (AStrategy *this, int ix, Strategy *e);

/// Removes an element at position ix. Buffer size of 'this' does not change.
void aStrategy_remove (AStrategy *this, int ix);

/// Adds pointer to elements of 'other' to 'this'.
void aStrategy_cat (AStrategy *this, AStrategy *other);

/// Inserts pointer to elements of 'other' at 'ix'
void aStrategy_insert_arr (AStrategy *this, int ix, AStrategy *other);

/// Removes elements between [begin-end). Buffer size of 'this' does not change.
void aStrategy_remove_range (AStrategy *this, int begin, int end);

/// Removes every element of 'this'.
void aStrategy_clear (AStrategy *this);

/// Reverses elements of 'this'.
void aStrategy_reverse (AStrategy *this);

/// Sorts 'this' ascendantly using the function 'greater' that returns '1'
/// if 'e1' > 'e2'.
void aStrategy_sort (AStrategy *this, int (*greater)(Strategy *e1, Strategy *e2));

/// aStrategy_shuflle remix 'this' elements. It should be used after calling
/// rnd_init() or sys_init().
void aStrategy_shuffle (AStrategy *this);

/// Returns '1' if every element of 'this' yields '1' with 'pred'.
int aStrategy_all (AStrategy *this, int (*pred)(Strategy *e));

/// Returns '1' if some element of 'this' yields '1' with 'pred'.
int aStrategy_any (AStrategy *this, int (*pred)(Strategy *e));

/// Returns the index of the first elements which returns '1'
/// with 'pred', or -1 if such element does not exist.
int aStrategy_index (AStrategy *this, int (*pred)(Strategy *e));

/// Returns the index of the last elements which returns '1'
/// with 'pred', or -1 if such element does not exist.
int aStrategy_last_index (AStrategy *this, int (*pred)(Strategy *e));

/// Returns the first element which pruduces '1' with 'pred' or 'tp_none'.
OStrategy *aStrategy_find(AStrategy *this, int (*pred)(Strategy *e));

/// Returns the last element which pruduces '1' with 'pred' or 'tp_none'.
OStrategy *aStrategy_find_last(AStrategy *this, int (*pred)(Strategy *e));

/// aStrategy_filter_in removes every element which returns '0' with 'pred'.
void aStrategy_filter_in (AStrategy *this, int (*pred)(Strategy *e));

/// Returns a new Arr with the n first elements of this.
/// If this has less elements than n, returs a copy of this.
AStrategy *aStrategy_take (AStrategy *this, int n);

/// Returns a new Arr with the first elements which return '1' with 'predicate'.
AStrategy *aStrategy_takef (AStrategy *this, int (*predicate)(Strategy *e));

/// Returns a new Arr with elements left after aStrategy_take.
AStrategy *aStrategy_drop (AStrategy *this, int n);

/// Returns a new Arr with elements left after aStrategy_takef.
AStrategy *aStrategy_dropf (AStrategy *this, int (*predicate)(Strategy *e));

/// Returns a new Arr with every element which returns '1' with 'pred'.
AStrategy *aStrategy_filter_to (AStrategy *this, int (*predicate)(Strategy *e));

/// Returns a new Arr with elements generated by converter.
Arr *aStrategy_map (AStrategy *this, void *(*converter)(Strategy *e));

/// Returns a new Arr whit the first element generated by conv1 and the rest
/// by conv2.
Arr *aStrategy_map2 (AStrategy *this, void *(*conv1)(Strategy *e), void *(*conv2)(Strategy *e));

/// Returns a new Arr mixing values of 'a1' and 'a2'. The size of the resultant
/// array is the less of 'a1' size and 'a2' size.
Arr *aStrategy_zip (AStrategy *a1, AStrategy *a2, void *(*converter)(Strategy *e1, Strategy *e2));

/// Returns a new Arr mixing values of 'a1', 'a2' and 'a3'. The size of the
/// resultant array is the less of 'a1' size, 'a2' size and 'a3' size.
Arr *aStrategy_zip3 (
  AStrategy *a1, AStrategy *a2, AStrategy *a3,
  void *(*conveter)(Strategy*e1, Strategy*e2, Strategy*e3)
);

/// Removes duplicates with function 'feq=1' and returns them in a new array.
/// It returns only the first duplicated element.
AStrategy *aStrategy_duplicates (AStrategy *this, int (feq)(Strategy *e1, Strategy *e2));

/// Returns this JSONized.
///   this: Container.
char *aStrategy_to_js (AStrategy *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
AStrategy *aStrategy_from_js (char *js);


//--// Not remove

#endif