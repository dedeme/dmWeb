// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Arr[Profits *].

#ifndef DATA_PROFITS_APROFITS_H
  #define DATA_PROFITS_APROFITS_H

#include "dmc/Arr.h"
#include "data/Profits/OProfits.h"

#include "data/Profits.h"

/// Arr[Profits *].
struct aProfits_AProfits {
  Profits **es; // Start elements.
  Profits **end; // End elements. (Elements are between 'es' (inclusive) and 'end'
              // exclusive.
  Profits **endbf; // End buffer.
};

/// Arr[Profits *].
typedef struct aProfits_AProfits AProfits;

/// Creates a new Array with buffer size of 15 elements.
AProfits *aProfits_new (void);

/// 'buffer' must be > 0.
AProfits *aProfits_bf_new (int buffer);

/// Creates a new array from several elements.
/// Elements list must finish with NULL.
AProfits *aProfits_new_from (Profits *e, ...);

/// Creates a new array from a C array. For example:
///   Arr *a = arr_new_c(3, (void *[]){"c", "d", "e"});
/// If 'size' is less than C array length, result is ok (only will be
/// used 'size' first elements); but if 'size' is greater, the result is
/// undetermined.
AProfits *aProfits_new_c (int size, Profits **es);

/// Returns a new array with elements of 'this'.
AProfits *aProfits_copy (AProfits *this);

///
int aProfits_size (AProfits *this);

/// Resturn the element at position ix.
Profits *aProfits_get (AProfits *this, int ix);

/// Adds an element at the end of 'this'. 'e' will be freed by 'this'.
void aProfits_push (AProfits *this, Profits *e);

/// Returns and removes the last element.
Profits *aProfits_pop (AProfits *this);

/// Returns the las element.
Profits *aProfits_peek (AProfits *this);

/// Sets the element at position ix.
void aProfits_set (AProfits *this, int ix, Profits *e);

/// Inserts an element at position ix.
void aProfits_insert (AProfits *this, int ix, Profits *e);

/// Removes an element at position ix. Buffer size of 'this' does not change.
void aProfits_remove (AProfits *this, int ix);

/// Adds pointer to elements of 'other' to 'this'.
void aProfits_cat (AProfits *this, AProfits *other);

/// Inserts pointer to elements of 'other' at 'ix'
void aProfits_insert_arr (AProfits *this, int ix, AProfits *other);

/// Removes elements between [begin-end). Buffer size of 'this' does not change.
void aProfits_remove_range (AProfits *this, int begin, int end);

/// Removes every element of 'this'.
void aProfits_clear (AProfits *this);

/// Reverses elements of 'this'.
void aProfits_reverse (AProfits *this);

/// Sorts 'this' ascendantly using the function 'greater' that returns '1'
/// if 'e1' > 'e2'.
void aProfits_sort (AProfits *this, int (*greater)(Profits *e1, Profits *e2));

/// aProfits_shuflle remix 'this' elements. It should be used after calling
/// rnd_init() or sys_init().
void aProfits_shuffle (AProfits *this);

/// Returns '1' if every element of 'this' yields '1' with 'pred'.
int aProfits_all (AProfits *this, int (*pred)(Profits *e));

/// Returns '1' if some element of 'this' yields '1' with 'pred'.
int aProfits_any (AProfits *this, int (*pred)(Profits *e));

/// Returns the index of the first elements which returns '1'
/// with 'pred', or -1 if such element does not exist.
int aProfits_index (AProfits *this, int (*pred)(Profits *e));

/// Returns the index of the last elements which returns '1'
/// with 'pred', or -1 if such element does not exist.
int aProfits_last_index (AProfits *this, int (*pred)(Profits *e));

/// Returns the first element which pruduces '1' with 'pred' or 'tp_none'.
OProfits *aProfits_find(AProfits *this, int (*pred)(Profits *e));

/// Returns the last element which pruduces '1' with 'pred' or 'tp_none'.
OProfits *aProfits_find_last(AProfits *this, int (*pred)(Profits *e));

/// aProfits_filter_in removes every element which returns '0' with 'pred'.
void aProfits_filter_in (AProfits *this, int (*pred)(Profits *e));

/// Returns a new Arr with the n first elements of this.
/// If this has less elements than n, returs a copy of this.
AProfits *aProfits_take (AProfits *this, int n);

/// Returns a new Arr with the first elements which return '1' with 'predicate'.
AProfits *aProfits_takef (AProfits *this, int (*predicate)(Profits *e));

/// Returns a new Arr with elements left after aProfits_take.
AProfits *aProfits_drop (AProfits *this, int n);

/// Returns a new Arr with elements left after aProfits_takef.
AProfits *aProfits_dropf (AProfits *this, int (*predicate)(Profits *e));

/// Returns a new Arr with every element which returns '1' with 'pred'.
AProfits *aProfits_filter_to (AProfits *this, int (*predicate)(Profits *e));

/// Returns a new Arr with elements generated by converter.
Arr *aProfits_map (AProfits *this, void *(*converter)(Profits *e));

/// Returns a new Arr whit the first element generated by conv1 and the rest
/// by conv2.
Arr *aProfits_map2 (AProfits *this, void *(*conv1)(Profits *e), void *(*conv2)(Profits *e));

/// Returns a new Arr mixing values of 'a1' and 'a2'. The size of the resultant
/// array is the less of 'a1' size and 'a2' size.
Arr *aProfits_zip (AProfits *a1, AProfits *a2, void *(*converter)(Profits *e1, Profits *e2));

/// Returns a new Arr mixing values of 'a1', 'a2' and 'a3'. The size of the
/// resultant array is the less of 'a1' size, 'a2' size and 'a3' size.
Arr *aProfits_zip3 (
  AProfits *a1, AProfits *a2, AProfits *a3,
  void *(*conveter)(Profits*e1, Profits*e2, Profits*e3)
);

/// Removes duplicates with function 'feq=1' and returns them in a new array.
/// It returns only the first duplicated element.
AProfits *aProfits_duplicates (AProfits *this, int (feq)(Profits *e1, Profits *e2));

/// Returns this JSONized.
///   this: Container.
char *aProfits_to_js (AProfits *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
AProfits *aProfits_from_js (char *js);


//--// Not remove

/// Returns the result of sum 'this', using description to the return.
Profits *aProfits_sum(AProfits *this, char *description);

#endif
