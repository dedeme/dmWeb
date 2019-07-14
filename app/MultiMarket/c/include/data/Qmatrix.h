// Copyright 11-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Quotes values (opens or closes) used by fleas.

#ifndef DATA_QMATRIX_H
  #define DATA_QMATRIX_H

#include "dmc/std.h"

/// Array of doubles
typedef double * QmatrixValues;

/*--*/

///
///   Arguments:
///     nicks: Arr-Nick
///     values: QmatrixValues
typedef struct Qmatrix_Qmatrix Qmatrix;

///
Qmatrix *qmatrix_new (Arr *nicks, QmatrixValues *values);

/// Arr[Nick]
Arr *qmatrix_nicks (Qmatrix *this);

/// Array of HISTORIC_QUOTES of QmatrixValues (one for each date). Every
/// QmatrixValues has 'nicks size' doubles.
QmatrixValues *qmatrix_values (Qmatrix *this);

/*--*/

#endif
