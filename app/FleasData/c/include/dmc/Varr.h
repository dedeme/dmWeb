// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Array structure.<p>
/// This structure is not owner of its elements and does not free them with
/// varr_free.

#ifndef DMC_VARR_H
  #define DMC_VARR_H

///
typedef struct varr_Varr Varr;

///
Varr *varr_new(void);

/// buffer must be > 0
Varr *varr_2_new(int buffer);

/// If ix is < 0 then is changed to 'varr_size - ix'
Varr *varr_left_new(Varr *this, int ix);

/// If ix is < 0 then is changed to 'varr_size - ix'
Varr *varr_right_new(Varr *this, int ix);

/// If begin or end are < 0 then is changed to 'varr_size - itsValue'
Varr *varr_sub_new(Varr *this, int begin, int end);

///
void varr_free(Varr *this);

///
int varr_size(Varr *this);

/// If ix is < 0 then is changed to 'varr_size - ix'
void *varr_get(Varr *this, int ix);

/// Return a pointer to the first element of 'this'
void **varr_start(Varr *this);

/// Returns a pointer to the next element after the last element of 'this'.
/// 'arr_end' does not point to a valid element.
void **varr_end(Varr *this);

/// Adds an element at the end of 'this'.
void varr_push(Varr *this, void *e);

/// Returns and removes the last element.
void *varr_pop(Varr *this);

/// Returns the las element.
void *varr_peek(Varr *this);

/// If ix is < 0 then is changed to 'varr_size - ix'
void varr_set(Varr *this, int ix, void *e);

/// If ix is < 0 then is changed to 'varr_size - ix'
void varr_insert(Varr *this, int ix, void *e);

/// If ix is < 0 then is changed to 'varr_size - ix'
void varr_remove(Varr *this, int ix);

/// Adds other to 'this'.
void varr_cat(Varr *this, Varr *other);

/// If ix is < 0 then is changed to 'varr_size - ix'
void varr_insert_arr(Varr *this, int ix, Varr *other);

/// If begin or end are < 0 then is changed to 'varr_size - itsValue'
void varr_remove_range(Varr *this, int begin, int end);

/// Reverses elements of 'this'
void varr_reverse(Varr *this);

/// Sorts 'this' using the function 'greater' that returns 'true' if
/// 'e1' > 'e2'
void varr_sort(Varr *this, int (*greater)(void *, void *));

/// varr_shuflle remix 'this' elements. It should be used after calling
/// rnd_init() or sys_init()
void varr_shuffle(Varr *this);

/// varr_index returns the index of the first elements which returns 'true'
/// with 'pred', or -1 if such element does not exist.
int varr_index(Varr *this, int (*pred)(void *e));

/// varr_filter removes every element which returns 'false' with 'pred'.
void varr_filter(Varr *this, int (*pred)(void *e));

#endif
