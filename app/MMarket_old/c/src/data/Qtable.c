// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Qtable.h"
#include "data/Nick.h"
#include "DEFS.h"

/* .
# Record for manage opens and closes.
Qtable
  # Arr[Nick]
  nicks: Arr - Nick
  # Array of opens/closes ordered by dates(rows) x nicks(columns)
  #   - Rows number is HISTORIC_QUOTES. Their are sorted from before to after.
  #     Each row is saved in a QmatrixRow (doubles array).
  #   - Columns number is 'arr_len(qtable_nicks())'
  values: QtableRow
*/

/*--*/

struct Qtable_Qtable {
  Arr *nicks;
  QtableRow *values;
};

Qtable *qtable_new (Arr *nicks, QtableRow *values) {
  Qtable *this = MALLOC(Qtable);
  this->nicks = nicks;
  this->values = values;
  return this;
}

Arr *qtable_nicks (Qtable *this) {
  return this->nicks;
}

QtableRow *qtable_values (Qtable *this) {
  return this->values;
}

/*--*/

void qtable_add(QtableRow *table, QtableRow row) {
  QtableRow *next = table;
  REPEAT(HISTORIC_QUOTES - 1) {
    *table++ = *(next++);
  }_REPEAT
  *table = row;
}

Opt *qtable_nick_values (Qtable *this, char *nick) {
  int fn1 (Nick *nk) { return str_eq(nick_name(nk), nick); }
  int ix = arr_index(qtable_nicks(this), (FPRED)fn1);
  if (ix == -1) return opt_empty();
  double *r = ATOMIC(HISTORIC_QUOTES * sizeof(double));
  double *t = r;
  QtableRow *s = this->values;
  REPEAT(HISTORIC_QUOTES) {
    *t++ = (*s++)[ix];
  }_REPEAT
  return opt_new(r);
}

// Arr[double *]
Arr *qtable_cos_values (Qtable *this) {
  int n_cos = arr_size(this->nicks);
  // Arr[double *]
  Arr *r = arr_new();
  REPEAT(n_cos) {
    arr_push(r, ATOMIC(HISTORIC_QUOTES * sizeof(double)));
  }_REPEAT

  QtableRow *s = this->values;
  RANGE0(row, HISTORIC_QUOTES) {
    QtableRow qrow = *s++;
    RANGE0(col, n_cos) {
      ((double *)arr_get(r, col))[row] = *qrow++;
    }_RANGE
  }_RANGE

  return r;
}

QtableRow *qtable_from_column (double *nick_quotes) {
  QtableRow *table = GC_MALLOC(HISTORIC_QUOTES * sizeof(QtableRow));
  QtableRow *ptable = table;
  double *qs = nick_quotes;
  REPEAT(HISTORIC_QUOTES) {
    QtableRow row = ATOMIC(sizeof(double));
    *row = *qs++;
    *ptable++ = row;
  }_REPEAT
  return table;
}

double qtable_last_ok (double *nick_quotes) {
  double *qs = nick_quotes + HISTORIC_QUOTES - 1;
  if (*qs-- > 0) return *qs;

  while (qs > nick_quotes) {
    double q = *qs--;
    if (q > 0) return q;
  }
  if (*qs > 0) return *qs;
  return 1; // This point ought not to be reached.
}

double qtable_last_row_ok (QtableRow *rows, int n_co) {
  QtableRow *rs = rows + HISTORIC_QUOTES - 1;
  if ((*rs--)[n_co] > 0) return (*rs)[n_co];

  while (rs > rows) {
    double r = (*rs--)[n_co];
    if (r > 0) return r;
  }
  if ((*rs)[n_co] > 0) return (*rs)[n_co];
  return 1; // This point ought not to be reached.
}
