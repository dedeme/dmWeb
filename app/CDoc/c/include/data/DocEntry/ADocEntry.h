// Copyright 25-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Arr[DocEntry *].

#ifndef DATA_DOCENTRY_ADOCENTRY_H
  #define DATA_DOCENTRY_ADOCENTRY_H

#include "dmc/Arr.h"
#include "data/DocEntry/ODocEntry.h"

#include "data/DocEntry.h"

/// Arr[DocEntry *].
struct aDocEntry_ADocEntry {
  DocEntry **es; // Start elements.
  DocEntry **end; // End elements. (Elements are between 'es' (inclusive) and 'end'
              // exclusive.
  DocEntry **endbf; // End buffer.
};

/// Arr[DocEntry *].
typedef struct aDocEntry_ADocEntry ADocEntry;

/// Creates a new Array with buffer size of 15 elements.
ADocEntry *aDocEntry_new (void);

/// 'buffer' must be > 0.
ADocEntry *aDocEntry_bf_new (int buffer);

/// Creates a new array from several elements.
/// Elements list must finish with NULL.
ADocEntry *aDocEntry_new_from (DocEntry *e, ...);

/// Creates a new array from a C array. For example:
///   Arr *a = arr_new_c(3, (void *[]){"c", "d", "e"});
/// If 'size' is less than C array length, result is ok (only will be
/// used 'size' first elements); but if 'size' is greater, the result is
/// undetermined.
ADocEntry *aDocEntry_new_c (int size, DocEntry **es);

/// Returns a new array with elements of 'this'.
ADocEntry *aDocEntry_copy (ADocEntry *this);

///
int aDocEntry_size (ADocEntry *this);

/// Resturn the element at position ix.
DocEntry *aDocEntry_get (ADocEntry *this, int ix);

/// Adds an element at the end of 'this'. 'e' will be freed by 'this'.
void aDocEntry_push (ADocEntry *this, DocEntry *e);

/// Returns and removes the last element.
DocEntry *aDocEntry_pop (ADocEntry *this);

/// Returns the las element.
DocEntry *aDocEntry_peek (ADocEntry *this);

/// Sets the element at position ix.
void aDocEntry_set (ADocEntry *this, int ix, DocEntry *e);

/// Inserts an element at position ix.
void aDocEntry_insert (ADocEntry *this, int ix, DocEntry *e);

/// Removes an element at position ix. Buffer size of 'this' does not change.
void aDocEntry_remove (ADocEntry *this, int ix);

/// Adds pointer to elements of 'other' to 'this'.
void aDocEntry_cat (ADocEntry *this, ADocEntry *other);

/// Inserts pointer to elements of 'other' at 'ix'
void aDocEntry_insert_arr (ADocEntry *this, int ix, ADocEntry *other);

/// Removes elements between [begin-end). Buffer size of 'this' does not change.
void aDocEntry_remove_range (ADocEntry *this, int begin, int end);

/// Removes every element of 'this'.
void aDocEntry_clear (ADocEntry *this);

/// Reverses elements of 'this'.
void aDocEntry_reverse (ADocEntry *this);

/// Sorts 'this' ascendantly using the function 'greater' that returns '1'
/// if 'e1' > 'e2'.
void aDocEntry_sort (ADocEntry *this, int (*greater)(DocEntry *e1, DocEntry *e2));

/// aDocEntry_shuflle remix 'this' elements. It should be used after calling
/// rnd_init() or sys_init().
void aDocEntry_shuffle (ADocEntry *this);

/// Returns '1' if every element of 'this' yields '1' with 'pred'.
int aDocEntry_all (ADocEntry *this, int (*pred)(DocEntry *e));

/// Returns '1' if some element of 'this' yields '1' with 'pred'.
int aDocEntry_any (ADocEntry *this, int (*pred)(DocEntry *e));

/// Returns the index of the first elements which returns '1'
/// with 'pred', or -1 if such element does not exist.
int aDocEntry_index (ADocEntry *this, int (*pred)(DocEntry *e));

/// Returns the index of the last elements which returns '1'
/// with 'pred', or -1 if such element does not exist.
int aDocEntry_last_index (ADocEntry *this, int (*pred)(DocEntry *e));

/// Returns the first element which pruduces '1' with 'pred' or 'tp_none'.
ODocEntry *aDocEntry_find(ADocEntry *this, int (*pred)(DocEntry *e));

/// Returns the last element which pruduces '1' with 'pred' or 'tp_none'.
ODocEntry *aDocEntry_find_last(ADocEntry *this, int (*pred)(DocEntry *e));

/// aDocEntry_filter_in removes every element which returns '0' with 'pred'.
void aDocEntry_filter_in (ADocEntry *this, int (*pred)(DocEntry *e));

/// Returns a new Arr with the n first elements of this.
/// If this has less elements than n, returs a copy of this.
ADocEntry *aDocEntry_take (ADocEntry *this, int n);

/// Returns a new Arr with the first elements which return '1' with 'predicate'.
ADocEntry *aDocEntry_takef (ADocEntry *this, int (*predicate)(DocEntry *e));

/// Returns a new Arr with elements left after aDocEntry_take.
ADocEntry *aDocEntry_drop (ADocEntry *this, int n);

/// Returns a new Arr with elements left after aDocEntry_takef.
ADocEntry *aDocEntry_dropf (ADocEntry *this, int (*predicate)(DocEntry *e));

/// Returns a new Arr with every element which returns '1' with 'pred'.
ADocEntry *aDocEntry_filter_to (ADocEntry *this, int (*predicate)(DocEntry *e));

/// Returns a new Arr with elements generated by converter.
Arr *aDocEntry_map (ADocEntry *this, void *(*converter)(DocEntry *e));

/// Returns a new Arr whit the first element generated by conv1 and the rest
/// by conv2.
Arr *aDocEntry_map2 (ADocEntry *this, void *(*conv1)(DocEntry *e), void *(*conv2)(DocEntry *e));

/// Returns a new Arr mixing values of 'a1' and 'a2'. The size of the resultant
/// array is the less of 'a1' size and 'a2' size.
Arr *aDocEntry_zip (ADocEntry *a1, ADocEntry *a2, void *(*converter)(DocEntry *e1, DocEntry *e2));

/// Returns a new Arr mixing values of 'a1', 'a2' and 'a3'. The size of the
/// resultant array is the less of 'a1' size, 'a2' size and 'a3' size.
Arr *aDocEntry_zip3 (
  ADocEntry *a1, ADocEntry *a2, ADocEntry *a3,
  void *(*conveter)(DocEntry*e1, DocEntry*e2, DocEntry*e3)
);

/// Removes duplicates with function 'feq=1' and returns them in a new array.
/// It returns only the first duplicated element.
ADocEntry *aDocEntry_duplicates (ADocEntry *this, int (feq)(DocEntry *e1, DocEntry *e2));

/// Returns this JSONized.
///   this: Container.
char *aDocEntry_to_js (ADocEntry *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
ADocEntry *aDocEntry_from_js (char *js);


//--// Not remove

#endif