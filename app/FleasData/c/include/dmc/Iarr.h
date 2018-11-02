// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Array of ints

#ifndef DMC_IARR_H
  #define DMC_IARR_H

///
typedef struct iarr_Iarr Iarr;

///
Iarr *iarr_new(void);

/// buffer must be > 0
Iarr *iarr_2_new(int buffer);

/// If ix is < 0 then is changed to 'iarr_size - ix'
Iarr *iarr_left_new(Iarr *this, int ix);

/// If ix is < 0 then is changed to 'iarr_size - ix'
Iarr *iarr_right_new(Iarr *this, int ix);

/// If begin or end are < 0 then is changed to 'iarr_size - itsValue'
Iarr *iarr_sub_new(Iarr *this, int begin, int end);

///
void iarr_free(Iarr *this);

///
int iarr_size(Iarr *this);

/// If ix is < 0 then is changed to 'iarr_size - ix'
int iarr_get(Iarr *this, int ix);

///
int *iarr_start(Iarr *this);

///
int *iarr_end(Iarr *this);

///
void iarr_push(Iarr *this, int e);

/// If ix is < 0 then is changed to 'iarr_size - ix'
void iarr_set(Iarr *this, int ix, int e);

/// If ix is < 0 then is changed to 'iarr_size - ix'
void iarr_insert(Iarr *this, int ix, int e);

/// If ix is < 0 then is changed to 'iarr_size - ix'
void iarr_remove(Iarr *this, int ix);

///
void iarr_cat(Iarr *this, Iarr *other);

/// If ix is < 0 then is changed to 'iarr_size - ix'
void iarr_insert_arr(Iarr *this, int ix, Iarr *other);

/// If begin or end are < 0 then is changed to 'iarr_size - itsValue'
void iarr_remove_range(Iarr *this, int begin, int end);

///
void iarr_reverse(Iarr *this);

///
void iarr_sort(Iarr *this);

#endif
