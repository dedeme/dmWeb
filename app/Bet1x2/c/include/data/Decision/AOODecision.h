// Copyright 28-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Arr[OODecision *].

#ifndef DATA_DECISION_AOODECISION_H
  #define DATA_DECISION_AOODECISION_H

#include "dmc/Arr.h"
#include "data/Decision/OOODecision.h"

#include "data/Decision/OODecision.h"

/// Arr[OODecision *].
struct aOODecision_AOODecision {
  OODecision **es; // Start elements.
  OODecision **end; // End elements. (Elements are between 'es' (inclusive) and 'end'
              // exclusive.
  OODecision **endbf; // End buffer.
};

/// Arr[OODecision *].
typedef struct aOODecision_AOODecision AOODecision;

/// Creates a new Array with buffer size of 15 elements.
AOODecision *aOODecision_new (void);

/// 'buffer' must be > 0.
AOODecision *aOODecision_bf_new (int buffer);

/// Creates a new array from several elements.
/// Elements list must finish with NULL.
AOODecision *aOODecision_new_from (OODecision *e, ...);

/// Creates a new array from a C array. For example:
///   Arr *a = arr_new_c(3, (void *[]){"c", "d", "e"});
/// If 'size' is less than C array length, result is ok (only will be
/// used 'size' first elements); but if 'size' is greater, the result is
/// undetermined.
AOODecision *aOODecision_new_c (int size, OODecision **es);

/// Returns a new array with elements of 'this'.
AOODecision *aOODecision_copy (AOODecision *this);

///
int aOODecision_size (AOODecision *this);

/// Resturn the element at position ix.
OODecision *aOODecision_get (AOODecision *this, int ix);

/// Adds an element at the end of 'this'. 'e' will be freed by 'this'.
void aOODecision_push (AOODecision *this, OODecision *e);

/// Returns and removes the last element.
OODecision *aOODecision_pop (AOODecision *this);

/// Returns the las element.
OODecision *aOODecision_peek (AOODecision *this);

/// Sets the element at position ix.
void aOODecision_set (AOODecision *this, int ix, OODecision *e);

/// Inserts an element at position ix.
void aOODecision_insert (AOODecision *this, int ix, OODecision *e);

/// Removes an element at position ix. Buffer size of 'this' does not change.
void aOODecision_remove (AOODecision *this, int ix);

/// Adds pointer to elements of 'other' to 'this'.
void aOODecision_cat (AOODecision *this, AOODecision *other);

/// Inserts pointer to elements of 'other' at 'ix'
void aOODecision_insert_arr (AOODecision *this, int ix, AOODecision *other);

/// Removes elements between [begin-end). Buffer size of 'this' does not change.
void aOODecision_remove_range (AOODecision *this, int begin, int end);

/// Removes every element of 'this'.
void aOODecision_clear (AOODecision *this);

/// Reverses elements of 'this'.
void aOODecision_reverse (AOODecision *this);

/// Sorts 'this' ascendantly using the function 'greater' that returns '1'
/// if 'e1' > 'e2'.
void aOODecision_sort (AOODecision *this, int (*greater)(OODecision *e1, OODecision *e2));

/// aOODecision_shuflle remix 'this' elements. It should be used after calling
/// rnd_init() or sys_init().
void aOODecision_shuffle (AOODecision *this);

/// Returns '1' if every element of 'this' yields '1' with 'pred'.
int aOODecision_all (AOODecision *this, int (*pred)(OODecision *e));

/// Returns '1' if some element of 'this' yields '1' with 'pred'.
int aOODecision_any (AOODecision *this, int (*pred)(OODecision *e));

/// Returns the index of the first elements which returns '1'
/// with 'pred', or -1 if such element does not exist.
int aOODecision_index (AOODecision *this, int (*pred)(OODecision *e));

/// Returns the index of the last elements which returns '1'
/// with 'pred', or -1 if such element does not exist.
int aOODecision_last_index (AOODecision *this, int (*pred)(OODecision *e));

/// Returns the first element which pruduces '1' with 'pred' or 'tp_none'.
OOODecision *aOODecision_find(AOODecision *this, int (*pred)(OODecision *e));

/// Returns the last element which pruduces '1' with 'pred' or 'tp_none'.
OOODecision *aOODecision_find_last(AOODecision *this, int (*pred)(OODecision *e));

/// aOODecision_filter_in removes every element which returns '0' with 'pred'.
void aOODecision_filter_in (AOODecision *this, int (*pred)(OODecision *e));

/// Returns a new Arr with the n first elements of this.
/// If this has less elements than n, returs a copy of this.
AOODecision *aOODecision_take (AOODecision *this, int n);

/// Returns a new Arr with the first elements which return '1' with 'predicate'.
AOODecision *aOODecision_takef (AOODecision *this, int (*predicate)(OODecision *e));

/// Returns a new Arr with elements left after aOODecision_take.
AOODecision *aOODecision_drop (AOODecision *this, int n);

/// Returns a new Arr with elements left after aOODecision_takef.
AOODecision *aOODecision_dropf (AOODecision *this, int (*predicate)(OODecision *e));

/// Returns a new Arr with every element which returns '1' with 'pred'.
AOODecision *aOODecision_filter_to (AOODecision *this, int (*predicate)(OODecision *e));

/// Returns a new Arr with elements generated by converter.
Arr *aOODecision_map (AOODecision *this, void *(*converter)(OODecision *e));

/// Returns a new Arr whit the first element generated by conv1 and the rest
/// by conv2.
Arr *aOODecision_map2 (AOODecision *this, void *(*conv1)(OODecision *e), void *(*conv2)(OODecision *e));

/// Returns a new Arr mixing values of 'a1' and 'a2'. The size of the resultant
/// array is the less of 'a1' size and 'a2' size.
Arr *aOODecision_zip (AOODecision *a1, AOODecision *a2, void *(*converter)(OODecision *e1, OODecision *e2));

/// Returns a new Arr mixing values of 'a1', 'a2' and 'a3'. The size of the
/// resultant array is the less of 'a1' size, 'a2' size and 'a3' size.
Arr *aOODecision_zip3 (
  AOODecision *a1, AOODecision *a2, AOODecision *a3,
  void *(*conveter)(OODecision*e1, OODecision*e2, OODecision*e3)
);

/// Removes duplicates with function 'feq=1' and returns them in a new array.
/// It returns only the first duplicated element.
AOODecision *aOODecision_duplicates (AOODecision *this, int (feq)(OODecision *e1, OODecision *e2));

/// Returns this JSONized.
///   this: Container.
char *aOODecision_to_js (AOODecision *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
AOODecision *aOODecision_from_js (char *js);


//--// Not remove

#endif