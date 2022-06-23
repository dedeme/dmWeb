// Copyright 13-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Arr[LogEntry *].

#ifndef DATA_LOGENTRY_ALOGENTRY_H
  #define DATA_LOGENTRY_ALOGENTRY_H

#include "dmc/Arr.h"
#include "data/LogEntry/OLogEntry.h"

#include "data/LogEntry.h"

/// Arr[LogEntry *].
struct aLogEntry_ALogEntry {
  LogEntry **es; // Start elements.
  LogEntry **end; // End elements. (Elements are between 'es' (inclusive) and 'end'
              // exclusive.
  LogEntry **endbf; // End buffer.
};

/// Arr[LogEntry *].
typedef struct aLogEntry_ALogEntry ALogEntry;

/// Creates a new Array with buffer size of 15 elements.
ALogEntry *aLogEntry_new (void);

/// 'buffer' must be > 0.
ALogEntry *aLogEntry_bf_new (int buffer);

/// Creates a new array from several elements.
/// Elements list must finish with NULL.
ALogEntry *aLogEntry_new_from (LogEntry *e, ...);

/// Creates a new array from a C array. For example:
///   Arr *a = arr_new_c(3, (void *[]){"c", "d", "e"});
/// If 'size' is less than C array length, result is ok (only will be
/// used 'size' first elements); but if 'size' is greater, the result is
/// undetermined.
ALogEntry *aLogEntry_new_c (int size, LogEntry **es);

/// Returns a new array with elements of 'this'.
ALogEntry *aLogEntry_copy (ALogEntry *this);

///
int aLogEntry_size (ALogEntry *this);

/// Resturn the element at position ix.
LogEntry *aLogEntry_get (ALogEntry *this, int ix);

/// Adds an element at the end of 'this'. 'e' will be freed by 'this'.
void aLogEntry_push (ALogEntry *this, LogEntry *e);

/// Returns and removes the last element.
LogEntry *aLogEntry_pop (ALogEntry *this);

/// Returns the las element.
LogEntry *aLogEntry_peek (ALogEntry *this);

/// Sets the element at position ix.
void aLogEntry_set (ALogEntry *this, int ix, LogEntry *e);

/// Inserts an element at position ix.
void aLogEntry_insert (ALogEntry *this, int ix, LogEntry *e);

/// Removes an element at position ix. Buffer size of 'this' does not change.
void aLogEntry_remove (ALogEntry *this, int ix);

/// Adds pointer to elements of 'other' to 'this'.
void aLogEntry_cat (ALogEntry *this, ALogEntry *other);

/// Inserts pointer to elements of 'other' at 'ix'
void aLogEntry_insert_arr (ALogEntry *this, int ix, ALogEntry *other);

/// Removes elements between [begin-end). Buffer size of 'this' does not change.
void aLogEntry_remove_range (ALogEntry *this, int begin, int end);

/// Removes every element of 'this'.
void aLogEntry_clear (ALogEntry *this);

/// Reverses elements of 'this'.
void aLogEntry_reverse (ALogEntry *this);

/// Sorts 'this' ascendantly using the function 'greater' that returns '1'
/// if 'e1' > 'e2'.
void aLogEntry_sort (ALogEntry *this, int (*greater)(LogEntry *e1, LogEntry *e2));

/// aLogEntry_shuflle remix 'this' elements. It should be used after calling
/// rnd_init() or sys_init().
void aLogEntry_shuffle (ALogEntry *this);

/// Returns '1' if every element of 'this' yields '1' with 'pred'.
int aLogEntry_all (ALogEntry *this, int (*pred)(LogEntry *e));

/// Returns '1' if some element of 'this' yields '1' with 'pred'.
int aLogEntry_any (ALogEntry *this, int (*pred)(LogEntry *e));

/// Returns the index of the first elements which returns '1'
/// with 'pred', or -1 if such element does not exist.
int aLogEntry_index (ALogEntry *this, int (*pred)(LogEntry *e));

/// Returns the index of the last elements which returns '1'
/// with 'pred', or -1 if such element does not exist.
int aLogEntry_last_index (ALogEntry *this, int (*pred)(LogEntry *e));

/// Returns the first element which pruduces '1' with 'pred' or 'tp_none'.
OLogEntry *aLogEntry_find(ALogEntry *this, int (*pred)(LogEntry *e));

/// Returns the last element which pruduces '1' with 'pred' or 'tp_none'.
OLogEntry *aLogEntry_find_last(ALogEntry *this, int (*pred)(LogEntry *e));

/// aLogEntry_filter_in removes every element which returns '0' with 'pred'.
void aLogEntry_filter_in (ALogEntry *this, int (*pred)(LogEntry *e));

/// Returns a new Arr with the n first elements of this.
/// If this has less elements than n, returs a copy of this.
ALogEntry *aLogEntry_take (ALogEntry *this, int n);

/// Returns a new Arr with the first elements which return '1' with 'predicate'.
ALogEntry *aLogEntry_takef (ALogEntry *this, int (*predicate)(LogEntry *e));

/// Returns a new Arr with elements left after aLogEntry_take.
ALogEntry *aLogEntry_drop (ALogEntry *this, int n);

/// Returns a new Arr with elements left after aLogEntry_takef.
ALogEntry *aLogEntry_dropf (ALogEntry *this, int (*predicate)(LogEntry *e));

/// Returns a new Arr with every element which returns '1' with 'pred'.
ALogEntry *aLogEntry_filter_to (ALogEntry *this, int (*predicate)(LogEntry *e));

/// Returns a new Arr with elements generated by converter.
Arr *aLogEntry_map (ALogEntry *this, void *(*converter)(LogEntry *e));

/// Returns a new Arr whit the first element generated by conv1 and the rest
/// by conv2.
Arr *aLogEntry_map2 (ALogEntry *this, void *(*conv1)(LogEntry *e), void *(*conv2)(LogEntry *e));

/// Returns a new Arr mixing values of 'a1' and 'a2'. The size of the resultant
/// array is the less of 'a1' size and 'a2' size.
Arr *aLogEntry_zip (ALogEntry *a1, ALogEntry *a2, void *(*converter)(LogEntry *e1, LogEntry *e2));

/// Returns a new Arr mixing values of 'a1', 'a2' and 'a3'. The size of the
/// resultant array is the less of 'a1' size, 'a2' size and 'a3' size.
Arr *aLogEntry_zip3 (
  ALogEntry *a1, ALogEntry *a2, ALogEntry *a3,
  void *(*conveter)(LogEntry*e1, LogEntry*e2, LogEntry*e3)
);

/// Removes duplicates with function 'feq=1' and returns them in a new array.
/// It returns only the first duplicated element.
ALogEntry *aLogEntry_duplicates (ALogEntry *this, int (feq)(LogEntry *e1, LogEntry *e2));

/// Returns this JSONized.
///   this: Container.
///   to  : Converter of container element to JSON.
char *aLogEntry_to_js (ALogEntry *this, char *(*to)(LogEntry *e));

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
///   from: Converter from JSON to container element.
ALogEntry *aLogEntry_from_js (char *js, LogEntry *(*from)(char *ejs));


//--// Not remove

#endif