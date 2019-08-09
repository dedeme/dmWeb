// Copyright 11-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Qmatrix.h"
#include "DEFS.h"

/* .
Qmatrix
  # Arr[Nick]
  nicks: Arr - Nick
  # Array of HISTORIC_QUOTES of QmatrixValues (one for each date). Every
  # QmatrixValues has 'nicks size' doubles.
  values: QmatrixValues
*/

/*--*/

struct Qmatrix_Qmatrix {
  Arr *nicks;
  QmatrixValues *values;
};

Qmatrix *qmatrix_new (Arr *nicks, QmatrixValues *values) {
  Qmatrix *this = MALLOC(Qmatrix);
  this->nicks = nicks;
  this->values = values;
  return this;
}

Arr *qmatrix_nicks (Qmatrix *this) {
  return this->nicks;
}

QmatrixValues *qmatrix_values (Qmatrix *this) {
  return this->values;
}

/*--*/

void qmatrix_add(QmatrixValues *table, QmatrixValues row) {
  QmatrixValues *next = table;
  REPEAT(HISTORIC_QUOTES - 1) {
    *table++ = *(next++);
  }_REPEAT
  *table = row;
}
