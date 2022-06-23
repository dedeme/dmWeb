// Copyright 25-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Arr[Tbet *].

#ifndef DATA_TBET_ATBET_H
  #define DATA_TBET_ATBET_H

#include "dmc/Arr.h"
#include "data/Tbet/OTbet.h"

#include "data/Tbet.h"

/// Arr[Tbet *].
struct aTbet_ATbet {
  Tbet **es; // Start elements.
  Tbet **end; // End elements. (Elements are between 'es' (inclusive) and 'end'
              // exclusive.
  Tbet **endbf; // End buffer.
};

/// Arr[Tbet *].
typedef struct aTbet_ATbet ATbet;

/// Creates a new Array with buffer size of 15 elements.
ATbet *aTbet_new (void);

/// 'buffer' must be > 0.
ATbet *aTbet_bf_new (int buffer);

/// Creates a new array from several elements.
/// Elements list must finish with NULL.
ATbet *aTbet_new_from (Tbet *e, ...);

/// Creates a new array from a C array. For example:
///   Arr *a = arr_new_c(3, (void *[]){"c", "d", "e"});
/// If 'size' is less than C array length, result is ok (only will be
/// used 'size' first elements); but if 'size' is greater, the result is
/// undetermined.
ATbet *aTbet_new_c (int size, Tbet **es);

/// Returns a new array with elements of 'this'.
ATbet *aTbet_copy (ATbet *this);

///
int aTbet_size (ATbet *this);

/// Resturn the element at position ix.
Tbet *aTbet_get (ATbet *this, int ix);

/// Adds an element at the end of 'this'. 'e' will be freed by 'this'.
void aTbet_push (ATbet *this, Tbet *e);

/// Returns and removes the last element.
Tbet *aTbet_pop (ATbet *this);

/// Returns the las element.
Tbet *aTbet_peek (ATbet *this);

/// Sets the element at position ix.
void aTbet_set (ATbet *this, int ix, Tbet *e);

/// Inserts an element at position ix.
void aTbet_insert (ATbet *this, int ix, Tbet *e);

/// Removes an element at position ix. Buffer size of 'this' does not change.
void aTbet_remove (ATbet *this, int ix);

/// Adds pointer to elements of 'other' to 'this'.
void aTbet_cat (ATbet *this, ATbet *other);

/// Inserts pointer to elements of 'other' at 'ix'
void aTbet_insert_arr (ATbet *this, int ix, ATbet *other);

/// Removes elements between [begin-end). Buffer size of 'this' does not change.
void aTbet_remove_range (ATbet *this, int begin, int end);

/// Removes every element of 'this'.
void aTbet_clear (ATbet *this);

/// Reverses elements of 'this'.
void aTbet_reverse (ATbet *this);

/// Sorts 'this' ascendantly using the function 'greater' that returns '1'
/// if 'e1' > 'e2'.
void aTbet_sort (ATbet *this, int (*greater)(Tbet *e1, Tbet *e2));

/// aTbet_shuflle remix 'this' elements. It should be used after calling
/// rnd_init() or sys_init().
void aTbet_shuffle (ATbet *this);

/// Returns '1' if every element of 'this' yields '1' with 'pred'.
int aTbet_all (ATbet *this, int (*pred)(Tbet *e));

/// Returns '1' if some element of 'this' yields '1' with 'pred'.
int aTbet_any (ATbet *this, int (*pred)(Tbet *e));

/// Returns the index of the first elements which returns '1'
/// with 'pred', or -1 if such element does not exist.
int aTbet_index (ATbet *this, int (*pred)(Tbet *e));

/// Returns the index of the last elements which returns '1'
/// with 'pred', or -1 if such element does not exist.
int aTbet_last_index (ATbet *this, int (*pred)(Tbet *e));

/// Returns the first element which pruduces '1' with 'pred' or 'tp_none'.
OTbet *aTbet_find(ATbet *this, int (*pred)(Tbet *e));

/// Returns the last element which pruduces '1' with 'pred' or 'tp_none'.
OTbet *aTbet_find_last(ATbet *this, int (*pred)(Tbet *e));

/// aTbet_filter_in removes every element which returns '0' with 'pred'.
void aTbet_filter_in (ATbet *this, int (*pred)(Tbet *e));

/// Returns a new Arr with the n first elements of this.
/// If this has less elements than n, returs a copy of this.
ATbet *aTbet_take (ATbet *this, int n);

/// Returns a new Arr with the first elements which return '1' with 'predicate'.
ATbet *aTbet_takef (ATbet *this, int (*predicate)(Tbet *e));

/// Returns a new Arr with elements left after aTbet_take.
ATbet *aTbet_drop (ATbet *this, int n);

/// Returns a new Arr with elements left after aTbet_takef.
ATbet *aTbet_dropf (ATbet *this, int (*predicate)(Tbet *e));

/// Returns a new Arr with every element which returns '1' with 'pred'.
ATbet *aTbet_filter_to (ATbet *this, int (*predicate)(Tbet *e));

/// Returns a new Arr with elements generated by converter.
Arr *aTbet_map (ATbet *this, void *(*converter)(Tbet *e));

/// Returns a new Arr whit the first element generated by conv1 and the rest
/// by conv2.
Arr *aTbet_map2 (ATbet *this, void *(*conv1)(Tbet *e), void *(*conv2)(Tbet *e));

/// Returns a new Arr mixing values of 'a1' and 'a2'. The size of the resultant
/// array is the less of 'a1' size and 'a2' size.
Arr *aTbet_zip (ATbet *a1, ATbet *a2, void *(*converter)(Tbet *e1, Tbet *e2));

/// Returns a new Arr mixing values of 'a1', 'a2' and 'a3'. The size of the
/// resultant array is the less of 'a1' size, 'a2' size and 'a3' size.
Arr *aTbet_zip3 (
  ATbet *a1, ATbet *a2, ATbet *a3,
  void *(*conveter)(Tbet*e1, Tbet*e2, Tbet*e3)
);

/// Removes duplicates with function 'feq=1' and returns them in a new array.
/// It returns only the first duplicated element.
ATbet *aTbet_duplicates (ATbet *this, int (feq)(Tbet *e1, Tbet *e2));

/// Returns this JSONized.
///   this: Container.
char *aTbet_to_js (ATbet *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
ATbet *aTbet_from_js (char *js);


//--// Not remove

#endif