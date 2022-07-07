// Copyright 16-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/models/MM.h"
#include <stddef.h>
#include "dmc/ADouble.h"
#include "dmc/AInt.h"
#include "data/modelDocs/mmDoc.h"

static void calc (
  AADouble *closes,
  double *params,
  void (*action)(ADouble *closes, ADouble *refs)
) {
  int days = params[0] + 0.5;
  double strip = params[1];
  int ncos = aDouble_size(*closes->es);

  ADouble *irefs = aDouble_new();
  for (int i = 0; i < ncos; ++i) {
    aDouble_push(irefs, -1);
  }

  ADouble **pcloses = closes->es;

  for (int d = 0; d < days; ++d) {
    ADouble *row = *pcloses++;
    action(row, irefs);
  }

  ADouble **pcloses0 = closes->es;
  ADouble *row = *pcloses;
  ADouble *row0 = *pcloses0;
  AInt *is_solds = aInt_new();
  ADouble *refs = aDouble_new();
  double *prow = row->es;
  double *prow0 = row0->es;
  while (prow < row->end) {
    double q = *prow++;
    double q0 = *prow0++;
    double is_sold = q < q0;
    aInt_push(is_solds, is_sold);
    aDouble_push(refs, q0 * (is_sold ? 1 + strip : 1 - strip));
  }

  while (pcloses < closes->end) {
    ADouble *row = *pcloses++;
    ADouble *row0 = *pcloses0++;
    ADouble *new_refs = aDouble_new();
    AInt *new_is_solds = aInt_new();

    double *prow = row->es;
    double *prow0 = row0->es;
    double *prefs = refs->es;
    int *pis_solds = is_solds->es;
    while (prow < row->end) {
      double q = *prow++;
      double q0 = *prow0++;
      double ref = *prefs++;
      int is_sold = *pis_solds++;

      if (is_sold) {
        if (q > ref) {
          aInt_push(new_is_solds, 0);
          double new_ref = q0 * (1 - strip);
          aDouble_push(new_refs, new_ref);
        } else {
          aInt_push(new_is_solds, 1);
          double new_ref = q0 * (1 + strip);
          aDouble_push(new_refs, new_ref < ref ? new_ref : ref);
        }
      } else {
        if (q < ref) {
          aInt_push(new_is_solds, 1);
          double new_ref = q0 * (1 + strip);
          aDouble_push(new_refs, new_ref);
        } else {
          aInt_push(new_is_solds, 0);
          double new_ref = q0 * (1 - strip);
          aDouble_push(new_refs, new_ref > ref ? new_ref : ref);
        }
      }
    }

    refs = new_refs;
    is_solds = new_is_solds;

    action(row, refs);
  }
}

Model *mm_new (void) {
  return model_new(
    "MX_MN",
    "Máximo - Mínimo",
    mmDoc_get(),
    achar_new_from("Dias", "Banda", NULL),
    aDouble_new_c(2, (double[]){15, 0.04})->es,
    aDouble_new_c(2, (double[]){5, 0.005})->es,
    aDouble_new_c(2, (double[]){1, 0.001})->es,
    calc
  );
}
