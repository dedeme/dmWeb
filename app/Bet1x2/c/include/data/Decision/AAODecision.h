// Copyright 28-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Arr[AODecision *].

#ifndef DATA_DECISION_AAODECISION_H
  #define DATA_DECISION_AAODECISION_H

#include "dmc/Arr.h"
#include "data/Decision/OAODecision.h"

#include "data/Decision/AODecision.h"

/// Arr[AODecision *].
struct aAODecision_AAODecision {
  AODecision **es; // Start elements.
  AODecision **end; // End elements. (Elements are between 'es' (inclusive) and 'end'
              // exclusive.
  AODecision **endbf; // End buffer.
};

/// Arr[AODecision *].
typedef struct aAODecision_AAODecision AAODecision;

/// Creates a new Array with buffer size of 15 elements.
AAODecision *aAODecision_new (void);

/// 'buffer' must be > 0.
AAODecision *aAODecision_bf_new (int buffer);

/// Creates a new array from several elements.
/// Elements list must finish with NULL.
AAODecision *aAODecision_new_from (AODecision *e, ...);

/// Creates a new array from a C array. For example:
///   Arr *a = arr_new_c(3, (void *[]){"c", "d", "e"});
/// If 'size' is less than C array length, result is ok (only will be
/// used 'size' first elements); but if 'size' is greater, the result is
/// undetermined.
AAODecision *aAODecision_new_c (int size, AODecision **es);

/// Returns a new array with elements of 'this'.
AAODecision *aAODecision_copy (AAODecision *this);

///
int aAODecision_size (AAODecision *this);

/// Resturn the element at position ix.
AODecision *aAODecision_get (AAODecision *this, int ix);

/// Adds an element at the end of 'this'. 'e' will be freed by 'this'.
void aAODecision_push (AAODecision *this, AODecision *e);

/// Returns and removes the last element.
AODecision *aAODecision_pop (AAODecision *this);

/// Returns the las element.
AODecision *aAODecision_peek (AAODecision *this);

/// Sets the element at position ix.
void aAODecision_set (AAODecision *this, int ix, AODecision *e);

/// Inserts an element at position ix.
void aAODecision_insert (AAODecision *this, int ix, AODecision *e);

/// Removes an element at position ix. Buffer size of 'this' does not change.
void aAODecision_remove (AAODecision *this, int ix);

/// Adds pointer to elements of 'other' to 'this'.
void aAODecision_cat (AAODecision *this, AAODecision *other);

/// Inserts pointer to elements of 'other' at 'ix'
void aAODecision_insert_arr (AAODecision *this, int ix, AAODecision *other);

/// Removes elements between [begin-end). Buffer size of 'this' does not change.
void aAODecision_remove_range (AAODecision *this, int begin, int end);

/// Removes every element of 'this'.
void aAODecision_clear (AAODecision *this);

/// Reverses elements of 'this'.
void aAODecision_reverse (AAODecision *this);

/// Sorts 'this' ascendantly using the function 'greater' that returns '1'
/// if 'e1' > 'e2'.
void aAODecision_sort (AAODecision *this, int (*greater)(AODecision *e1, AODecision *e2));

/// aAODecision_shuflle remix 'this' elements. It should be used after calling
/// rnd_init() or sys_init().
void aAODecision_shuffle (AAODecision *this);

/// Returns '1' if every element of 'this' yields '1' with 'pred'.
int aAODecision_all (AAODecision *this, int (*pred)(AODecision *e));

/// Returns '1' if some element of 'this' yields '1' with 'pred'.
int aAODecision_any (AAODecision *this, int (*pred)(AODecision *e));

/// Returns the index of the first elements which returns '1'
/// with 'pred', or -1 if such element does not exist.
int aAODecision_index (AAODecision *this, int (*pred)(AODecision *e));

/// Returns the index of the last elements which returns '1'
/// with 'pred', or -1 if such element does not exist.
int aAODecision_last_index (AAODecision *this, int (*pred)(AODecision *e));

/// Returns the first element which pruduces '1' with 'pred' or 'tp_none'.
OAODecision *aAODecision_find(AAODecision *this, int (*pred)(AODecision *e));

/// Returns the last element which pruduces '1' with 'pred' or 'tp_none'.
OAODecision *aAODecision_find_last(AAODecision *this, int (*pred)(AODecision *e));

/// aAODecision_filter_in removes every element which returns '0' with 'pred'.
void aAODecision_filter_in (AAODecision *this, int (*pred)(AODecision *e));

/// Returns a new Arr with the n first elements of this.
/// If this has less elements than n, returs a copy of this.
AAODecision *aAODecision_take (AAODecision *this, int n);

/// Returns a new Arr with the first elements which return '1' with 'predicate'.
AAODecision *aAODecision_takef (AAODecision *this, int (*predicate)(AODecision *e));

/// Returns a new Arr with elements left after aAODecision_take.
AAODecision *aAODecision_drop (AAODecision *this, int n);

/// Returns a new Arr with elements left after aAODecision_takef.
AAODecision *aAODecision_dropf (AAODecision *this, int (*predicate)(AODecision *e));

/// Returns a new Arr with every element which returns '1' with 'pred'.
AAODecision *aAODecision_filter_to (AAODecision *this, int (*predicate)(AODecision *e));

/// Returns a new Arr with elements generated by converter.
Arr *aAODecision_map (AAODecision *this, void *(*converter)(AODecision *e));

/// Returns a new Arr whit the first element generated by conv1 and the rest
/// by conv2.
Arr *aAODecision_map2 (AAODecision *this, void *(*conv1)(AODecision *e), void *(*conv2)(AODecision *e));

/// Returns a new Arr mixing values of 'a1' and 'a2'. The size of the resultant
/// array is the less of 'a1' size and 'a2' size.
Arr *aAODecision_zip (AAODecision *a1, AAODecision *a2, void *(*converter)(AODecision *e1, AODecision *e2));

/// Returns a new Arr mixing values of 'a1', 'a2' and 'a3'. The size of the
/// resultant array is the less of 'a1' size, 'a2' size and 'a3' size.
Arr *aAODecision_zip3 (
  AAODecision *a1, AAODecision *a2, AAODecision *a3,
  void *(*conveter)(AODecision*e1, AODecision*e2, AODecision*e3)
);

/// Removes duplicates with function 'feq=1' and returns them in a new array.
/// It returns only the first duplicated element.
AAODecision *aAODecision_duplicates (AAODecision *this, int (feq)(AODecision *e1, AODecision *e2));

/// Returns this JSONized.
///   this: Container.
char *aAODecision_to_js (AAODecision *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
AAODecision *aAODecision_from_js (char *js);


//--// Not remove

#endif