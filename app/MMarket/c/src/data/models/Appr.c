// Copyright 24-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/models/Appr.h"
#include <stddef.h>
#include "dmc/ADouble.h"
#include "dmc/AInt.h"
#include "data/modelDocs/apprDoc.h"

static void calc (
  AADouble *closes,
  double *params,
  void (*action)(ADouble *closes, ADouble *refs)
) {
  double start = params[0];
  double incr = params[1];

  ADouble **pcloses = closes->es;
  ADouble *refs = aDouble_new();
  AInt *is_solds = aInt_new();


  ADouble *row = *pcloses;
  double *prow = row->es;
  while (prow < row->end) {
    double q = *prow++;
    aDouble_push(refs, q * (1 - start));
    aInt_push(is_solds, 0);
  }

  while (pcloses < closes->end) {
    ADouble *row = *pcloses++;
    ADouble *new_refs = aDouble_new();
    AInt *new_is_solds = aInt_new();

    double *prow = row->es;
    double *prefs = refs->es;
    int *pis_solds = is_solds->es;
    while (prow < row->end) {
      double q = *prow++;
      double ref = *prefs++;
      int is_sold = *pis_solds++;

      if (is_sold) {
        if (q > ref) {
          aInt_push(new_is_solds, 0);
          aDouble_push(new_refs, q * (1 - start));
        } else {
          aInt_push(new_is_solds, 1);
          aDouble_push(new_refs, ref - (ref - q) * incr);
        }
      } else {
        if (q < ref) {
          aInt_push(new_is_solds, 1);
          aDouble_push(new_refs, q * (1 + start));
        } else {
          aInt_push(new_is_solds, 0);
          aDouble_push(new_refs, ref + (q - ref) * incr);
        }
      }
    }

    refs = new_refs;
    is_solds = new_is_solds;

    action(row, refs);
  }
}

Model *appr_new (void) {
  return model_new(
    "APRX",
    "Aproximaciones sucesivas",
    apprDoc_get(),
    achar_new_from("Inicio", "Aproximación", NULL),
    aDouble_new_c(2, (double[]){0.04, 0.002})->es,
    aDouble_new_c(2, (double[]){0.02, 0.002})->es,
    aDouble_new_c(2, (double[]){0.002, 0.0005})->es,
    calc
  );
}
