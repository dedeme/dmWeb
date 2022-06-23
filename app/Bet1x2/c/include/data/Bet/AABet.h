// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Arr[ABet *].

#ifndef DATA_BET_AABET_H
  #define DATA_BET_AABET_H

#include "dmc/Arr.h"
#include "data/Bet/OABet.h"

#include "data/Bet/ABet.h"

/// Arr[ABet *].
struct aABet_AABet {
  ABet **es; // Start elements.
  ABet **end; // End elements. (Elements are between 'es' (inclusive) and 'end'
              // exclusive.
  ABet **endbf; // End buffer.
};

/// Arr[ABet *].
typedef struct aABet_AABet AABet;

/// Creates a new Array with buffer size of 15 elements.
AABet *aABet_new (void);

/// 'buffer' must be > 0.
AABet *aABet_bf_new (int buffer);

/// Creates a new array from several elements.
/// Elements list must finish with NULL.
AABet *aABet_new_from (ABet *e, ...);

/// Creates a new array from a C array. For example:
///   Arr *a = arr_new_c(3, (void *[]){"c", "d", "e"});
/// If 'size' is less than C array length, result is ok (only will be
/// used 'size' first elements); but if 'size' is greater, the result is
/// undetermined.
AABet *aABet_new_c (int size, ABet **es);

/// Returns a new array with elements of 'this'.
AABet *aABet_copy (AABet *this);

///
int aABet_size (AABet *this);

/// Resturn the element at position ix.
ABet *aABet_get (AABet *this, int ix);

/// Adds an element at the end of 'this'. 'e' will be freed by 'this'.
void aABet_push (AABet *this, ABet *e);

/// Returns and removes the last element.
ABet *aABet_pop (AABet *this);

/// Returns the las element.
ABet *aABet_peek (AABet *this);

/// Sets the element at position ix.
void aABet_set (AABet *this, int ix, ABet *e);

/// Inserts an element at position ix.
void aABet_insert (AABet *this, int ix, ABet *e);

/// Removes an element at position ix. Buffer size of 'this' does not change.
void aABet_remove (AABet *this, int ix);

/// Adds pointer to elements of 'other' to 'this'.
void aABet_cat (AABet *this, AABet *other);

/// Inserts pointer to elements of 'other' at 'ix'
void aABet_insert_arr (AABet *this, int ix, AABet *other);

/// Removes elements between [begin-end). Buffer size of 'this' does not change.
void aABet_remove_range (AABet *this, int begin, int end);

/// Removes every element of 'this'.
void aABet_clear (AABet *this);

/// Reverses elements of 'this'.
void aABet_reverse (AABet *this);

/// Sorts 'this' ascendantly using the function 'greater' that returns '1'
/// if 'e1' > 'e2'.
void aABet_sort (AABet *this, int (*greater)(ABet *e1, ABet *e2));

/// aABet_shuflle remix 'this' elements. It should be used after calling
/// rnd_init() or sys_init().
void aABet_shuffle (AABet *this);

/// Returns '1' if every element of 'this' yields '1' with 'pred'.
int aABet_all (AABet *this, int (*pred)(ABet *e));

/// Returns '1' if some element of 'this' yields '1' with 'pred'.
int aABet_any (AABet *this, int (*pred)(ABet *e));

/// Returns the index of the first elements which returns '1'
/// with 'pred', or -1 if such element does not exist.
int aABet_index (AABet *this, int (*pred)(ABet *e));

/// Returns the index of the last elements which returns '1'
/// with 'pred', or -1 if such element does not exist.
int aABet_last_index (AABet *this, int (*pred)(ABet *e));

/// Returns the first element which pruduces '1' with 'pred' or 'tp_none'.
OABet *aABet_find(AABet *this, int (*pred)(ABet *e));

/// Returns the last element which pruduces '1' with 'pred' or 'tp_none'.
OABet *aABet_find_last(AABet *this, int (*pred)(ABet *e));

/// aABet_filter_in removes every element which returns '0' with 'pred'.
void aABet_filter_in (AABet *this, int (*pred)(ABet *e));

/// Returns a new Arr with the n first elements of this.
/// If this has less elements than n, returs a copy of this.
AABet *aABet_take (AABet *this, int n);

/// Returns a new Arr with the first elements which return '1' with 'predicate'.
AABet *aABet_takef (AABet *this, int (*predicate)(ABet *e));

/// Returns a new Arr with elements left after aABet_take.
AABet *aABet_drop (AABet *this, int n);

/// Returns a new Arr with elements left after aABet_takef.
AABet *aABet_dropf (AABet *this, int (*predicate)(ABet *e));

/// Returns a new Arr with every element which returns '1' with 'pred'.
AABet *aABet_filter_to (AABet *this, int (*predicate)(ABet *e));

/// Returns a new Arr with elements generated by converter.
Arr *aABet_map (AABet *this, void *(*converter)(ABet *e));

/// Returns a new Arr whit the first element generated by conv1 and the rest
/// by conv2.
Arr *aABet_map2 (AABet *this, void *(*conv1)(ABet *e), void *(*conv2)(ABet *e));

/// Returns a new Arr mixing values of 'a1' and 'a2'. The size of the resultant
/// array is the less of 'a1' size and 'a2' size.
Arr *aABet_zip (AABet *a1, AABet *a2, void *(*converter)(ABet *e1, ABet *e2));

/// Returns a new Arr mixing values of 'a1', 'a2' and 'a3'. The size of the
/// resultant array is the less of 'a1' size, 'a2' size and 'a3' size.
Arr *aABet_zip3 (
  AABet *a1, AABet *a2, AABet *a3,
  void *(*conveter)(ABet*e1, ABet*e2, ABet*e3)
);

/// Removes duplicates with function 'feq=1' and returns them in a new array.
/// It returns only the first duplicated element.
AABet *aABet_duplicates (AABet *this, int (feq)(ABet *e1, ABet *e2));

/// Returns this JSONized.
///   this: Container.
char *aABet_to_js (AABet *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
AABet *aABet_from_js (char *js);


//--// Not remove

#endif