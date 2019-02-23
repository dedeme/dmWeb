// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Array of doubles.

#ifndef DMC_DARR_H
  #define DMC_DARR_H

///
typedef struct darr_Darr Darr;

///
Darr *darr_new(void);

/// buffer must be > 0
Darr *darr_2_new(int buffer);

/// If ix is < 0 then is changed to 'darr_size - ix'
Darr *darr_left_new(Darr *this, int ix);

/// If ix is < 0 then is changed to 'darr_size - ix'
Darr *darr_right_new(Darr *this, int ix);

/// If begin or end are < 0 then is changed to 'darr_size - itsValue'
Darr *darr_sub_new(Darr *this, int begin, int end);

///
void darr_free(Darr *this);

///
int darr_size(Darr *this);

/// If ix is < 0 then is changed to 'darr_size - ix'
double darr_get(Darr *this, int ix);

///
double *darr_start(Darr *this);

///
double *darr_end(Darr *this);

///
void darr_push(Darr *this, double e);

/// If ix is < 0 then is changed to 'darr_size - ix'
void darr_set(Darr *this, int ix, double e);

/// If ix is < 0 then is changed to 'darr_size - ix'
void darr_insert(Darr *this, int ix, double e);

/// If ix is < 0 then is changed to 'darr_size - ix'
void darr_remove(Darr *this, int ix);

///
void darr_cat(Darr *this, Darr *other);

/// If ix is < 0 then is changed to 'darr_size - ix'
void darr_insert_arr(Darr *this, int ix, Darr *other);

/// If begin or end are < 0 then is changed to 'darr_size - itsValue'
void darr_remove_range(Darr *this, int begin, int end);

///
void darr_reverse(Darr *this);

///
void darr_sort(Darr *this);

#endif
