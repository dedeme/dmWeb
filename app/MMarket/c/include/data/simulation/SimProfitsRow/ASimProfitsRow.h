// Copyright 28-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Arr[SimProfitsRow *].

#ifndef DATA_SIMULATION_SIMPROFITSROW_ASIMPROFITSROW_H
  #define DATA_SIMULATION_SIMPROFITSROW_ASIMPROFITSROW_H

#include "dmc/Arr.h"
#include "data/simulation/SimProfitsRow/OSimProfitsRow.h"

#include "data/simulation/SimProfitsRow.h"

/// Arr[SimProfitsRow *].
struct aSimProfitsRow_ASimProfitsRow {
  SimProfitsRow **es; // Start elements.
  SimProfitsRow **end; // End elements. (Elements are between 'es' (inclusive) and 'end'
              // exclusive.
  SimProfitsRow **endbf; // End buffer.
};

/// Arr[SimProfitsRow *].
typedef struct aSimProfitsRow_ASimProfitsRow ASimProfitsRow;

/// Creates a new Array with buffer size of 15 elements.
ASimProfitsRow *aSimProfitsRow_new (void);

/// 'buffer' must be > 0.
ASimProfitsRow *aSimProfitsRow_bf_new (int buffer);

/// Creates a new array from several elements.
/// Elements list must finish with NULL.
ASimProfitsRow *aSimProfitsRow_new_from (SimProfitsRow *e, ...);

/// Creates a new array from a C array. For example:
///   Arr *a = arr_new_c(3, (void *[]){"c", "d", "e"});
/// If 'size' is less than C array length, result is ok (only will be
/// used 'size' first elements); but if 'size' is greater, the result is
/// undetermined.
ASimProfitsRow *aSimProfitsRow_new_c (int size, SimProfitsRow **es);

/// Returns a new array with elements of 'this'.
ASimProfitsRow *aSimProfitsRow_copy (ASimProfitsRow *this);

///
int aSimProfitsRow_size (ASimProfitsRow *this);

/// Resturn the element at position ix.
SimProfitsRow *aSimProfitsRow_get (ASimProfitsRow *this, int ix);

/// Adds an element at the end of 'this'. 'e' will be freed by 'this'.
void aSimProfitsRow_push (ASimProfitsRow *this, SimProfitsRow *e);

/// Returns and removes the last element.
SimProfitsRow *aSimProfitsRow_pop (ASimProfitsRow *this);

/// Returns the las element.
SimProfitsRow *aSimProfitsRow_peek (ASimProfitsRow *this);

/// Sets the element at position ix.
void aSimProfitsRow_set (ASimProfitsRow *this, int ix, SimProfitsRow *e);

/// Inserts an element at position ix.
void aSimProfitsRow_insert (ASimProfitsRow *this, int ix, SimProfitsRow *e);

/// Removes an element at position ix. Buffer size of 'this' does not change.
void aSimProfitsRow_remove (ASimProfitsRow *this, int ix);

/// Adds pointer to elements of 'other' to 'this'.
void aSimProfitsRow_cat (ASimProfitsRow *this, ASimProfitsRow *other);

/// Inserts pointer to elements of 'other' at 'ix'
void aSimProfitsRow_insert_arr (ASimProfitsRow *this, int ix, ASimProfitsRow *other);

/// Removes elements between [begin-end). Buffer size of 'this' does not change.
void aSimProfitsRow_remove_range (ASimProfitsRow *this, int begin, int end);

/// Removes every element of 'this'.
void aSimProfitsRow_clear (ASimProfitsRow *this);

/// Reverses elements of 'this'.
void aSimProfitsRow_reverse (ASimProfitsRow *this);

/// Sorts 'this' ascendantly using the function 'greater' that returns '1'
/// if 'e1' > 'e2'.
void aSimProfitsRow_sort (ASimProfitsRow *this, int (*greater)(SimProfitsRow *e1, SimProfitsRow *e2));

/// aSimProfitsRow_shuflle remix 'this' elements. It should be used after calling
/// rnd_init() or sys_init().
void aSimProfitsRow_shuffle (ASimProfitsRow *this);

/// Returns '1' if every element of 'this' yields '1' with 'pred'.
int aSimProfitsRow_all (ASimProfitsRow *this, int (*pred)(SimProfitsRow *e));

/// Returns '1' if some element of 'this' yields '1' with 'pred'.
int aSimProfitsRow_any (ASimProfitsRow *this, int (*pred)(SimProfitsRow *e));

/// Returns the index of the first elements which returns '1'
/// with 'pred', or -1 if such element does not exist.
int aSimProfitsRow_index (ASimProfitsRow *this, int (*pred)(SimProfitsRow *e));

/// Returns the index of the last elements which returns '1'
/// with 'pred', or -1 if such element does not exist.
int aSimProfitsRow_last_index (ASimProfitsRow *this, int (*pred)(SimProfitsRow *e));

/// Returns the first element which pruduces '1' with 'pred' or 'tp_none'.
OSimProfitsRow *aSimProfitsRow_find(ASimProfitsRow *this, int (*pred)(SimProfitsRow *e));

/// Returns the last element which pruduces '1' with 'pred' or 'tp_none'.
OSimProfitsRow *aSimProfitsRow_find_last(ASimProfitsRow *this, int (*pred)(SimProfitsRow *e));

/// aSimProfitsRow_filter_in removes every element which returns '0' with 'pred'.
void aSimProfitsRow_filter_in (ASimProfitsRow *this, int (*pred)(SimProfitsRow *e));

/// Returns a new Arr with the n first elements of this.
/// If this has less elements than n, returs a copy of this.
ASimProfitsRow *aSimProfitsRow_take (ASimProfitsRow *this, int n);

/// Returns a new Arr with the first elements which return '1' with 'predicate'.
ASimProfitsRow *aSimProfitsRow_takef (ASimProfitsRow *this, int (*predicate)(SimProfitsRow *e));

/// Returns a new Arr with elements left after aSimProfitsRow_take.
ASimProfitsRow *aSimProfitsRow_drop (ASimProfitsRow *this, int n);

/// Returns a new Arr with elements left after aSimProfitsRow_takef.
ASimProfitsRow *aSimProfitsRow_dropf (ASimProfitsRow *this, int (*predicate)(SimProfitsRow *e));

/// Returns a new Arr with every element which returns '1' with 'pred'.
ASimProfitsRow *aSimProfitsRow_filter_to (ASimProfitsRow *this, int (*predicate)(SimProfitsRow *e));

/// Returns a new Arr with elements generated by converter.
Arr *aSimProfitsRow_map (ASimProfitsRow *this, void *(*converter)(SimProfitsRow *e));

/// Returns a new Arr whit the first element generated by conv1 and the rest
/// by conv2.
Arr *aSimProfitsRow_map2 (ASimProfitsRow *this, void *(*conv1)(SimProfitsRow *e), void *(*conv2)(SimProfitsRow *e));

/// Returns a new Arr mixing values of 'a1' and 'a2'. The size of the resultant
/// array is the less of 'a1' size and 'a2' size.
Arr *aSimProfitsRow_zip (ASimProfitsRow *a1, ASimProfitsRow *a2, void *(*converter)(SimProfitsRow *e1, SimProfitsRow *e2));

/// Returns a new Arr mixing values of 'a1', 'a2' and 'a3'. The size of the
/// resultant array is the less of 'a1' size, 'a2' size and 'a3' size.
Arr *aSimProfitsRow_zip3 (
  ASimProfitsRow *a1, ASimProfitsRow *a2, ASimProfitsRow *a3,
  void *(*conveter)(SimProfitsRow*e1, SimProfitsRow*e2, SimProfitsRow*e3)
);

/// Removes duplicates with function 'feq=1' and returns them in a new array.
/// It returns only the first duplicated element.
ASimProfitsRow *aSimProfitsRow_duplicates (ASimProfitsRow *this, int (feq)(SimProfitsRow *e1, SimProfitsRow *e2));

/// Returns this JSONized.
///   this: Container.
char *aSimProfitsRow_to_js (ASimProfitsRow *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
ASimProfitsRow *aSimProfitsRow_from_js (char *js);


//--// Not remove

#endif