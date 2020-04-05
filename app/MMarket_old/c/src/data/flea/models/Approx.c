// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/flea/models/Approx.h"
#include "DEFS.h"

static void fcalc (
  int n_cos,
  QtableRow *closes,
  Darr *params,
  void (*fn) (QtableRow closes, QtableRow refs)
) {
  double *ps = darr_start(params);
  double start_to_buy = ps[0];
  double step_to_buy = ps[1];
  double start_to_sell = ps[2];
  double step_to_sell = ps[3];

  // Initialize refs.
  QtableRow refs = ATOMIC(HISTORIC_QUOTES * sizeof(double));
  RANGE0(i, n_cos) {
    QtableRow *pcls = closes;
    REPEAT(HISTORIC_QUOTES) {
      QtableRow row = *pcls++;
      double q = row[i];
      if (q > 0) {
        refs[i] = q * (1 - start_to_sell);
        break;
      }
    }_REPEAT
  }_RANGE

  int to_sells[n_cos];
  int *pto_sells = to_sells;
  REPEAT(n_cos) {
    *pto_sells++ = 1;
  } _REPEAT

  QtableRow *pcls = closes;
  REPEAT(HISTORIC_QUOTES) {
    QtableRow row = *pcls++;
    RANGE0(i, n_cos) {
      double q = row[i];
      double ref = refs[i];
      if (q > 0) {
        if (to_sells[i]) {
          ref = ref + (q - ref) * step_to_sell;
          if (q <= ref) {
            ref = q * (1 + start_to_buy);
            to_sells[i] = 0;
          }
        } else {
          ref = ref - (ref - q) * step_to_buy;
          if (q >= ref) {
            //double pond = (q - this->ref) / this->ref;
            ref = q * (1 - start_to_sell);
            to_sells[i] = 1;
          }
        }
        refs[i] = ref;
      }
    }_RANGE
    fn(row, refs);
  }_REPEAT

}

Fmodel *approx_model (void) {
  return fmodel_new(
    "APPR",
    "Approx",
    arr_new_c(4, (void * []) {"Inicio C", "Paso C", "Inicio V", "Paso V"}),
    darr_new_c(4, (double []) {0.01, 0.001, 0.01, 0.001}),
    darr_new_c(4, (double []) {0.3, 0.1, 0.3, 0.1}),
    arr_new_c(4, (void * []) {
      "new Dec(#N# * 100, 2).toIso() + '%'",
      "new Dec(#N# * 100, 2).toIso() + '%'",
      "new Dec(#N# * 100, 2).toIso() + '%'",
      "new Dec(#N# * 100, 2).toIso() + '%'"
    }),
    fcalc
  );
}

