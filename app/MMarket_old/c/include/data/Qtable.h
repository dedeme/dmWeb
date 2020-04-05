// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Table of quotes.
///   - Rows are dates from before to after. Its number is 'HISTORIC_QUOTES'.
///     Each row is saved in a QtableRow.
///   - Columns are companies. Their number is equal to the number of nicks
///     selected. Their order is the order of 'qmatrix_nicks()'.

#ifndef DATA_QTABLE_H
  #define DATA_QTABLE_H

#include "dmc/async.h"

/// Array of doubles.
///   - Their number is equals to the number of nicks selected.
///   - Their order is the order of 'nicks_list()' filtered.
typedef double * QtableRow;

/*--*/

/// Record for manage opens and closes.
///   Arguments:
///     nicks: Arr-Nick
///     values: QtableRow
typedef struct Qtable_Qtable Qtable;

///
Qtable *qtable_new (Arr *nicks, QtableRow *values);

/// Arr[Nick]
Arr *qtable_nicks (Qtable *this);

/// Array of opens/closes ordered by dates(rows) x nicks(columns)
///   - Rows number is HISTORIC_QUOTES. Their are sorted from before to after.
///     Each row is saved in a QmatrixRow (doubles array).
///   - Columns number is 'arr_len(qtable_nicks())'
QtableRow *qtable_values (Qtable *this);

/*--*/

/// Adds 'row' to 'table' and remove the first row of 'table'
void qtable_add(QtableRow *table, QtableRow row);

/// Opt[double] Returns values of a nick, from before to after.
///   - If 'nick' is not found, returns 'opt_empty'
///   - Otherwise return a double* with HISTORIC_QUOTES values.
Opt *qtable_nick_values (Qtable *this, char *nick_name);

/// Returns Arr[double *] with the columns of 'this'
Arr *qtable_cos_values (Qtable *this);

/// Converts a double* to a QtableRow of length 1.
///   nick_quotes: its number must be HISTORIC_QUOTES, and they must be sorted
///                from before to after.
QtableRow *qtable_from_column (double *nick_quotes);

/// Returns the last value > 0.
///   nick_quotes: its number must be HISTORIC_QUOTES, and they must be sorted
///                from before to after.
double qtable_last_ok (double *nick_quotes);

/// Returns the last value > 0 of the column (company number) 'n_co'.
double qtable_last_row_ok (QtableRow *rows, int n_co);

#endif
