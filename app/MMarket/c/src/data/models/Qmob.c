// Copyright 24-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/models/Qmob.h"
#include <stddef.h>
#include "dmc/ADouble.h"
#include "dmc/AInt.h"
#include "data/modelDocs/qmobDoc.h"

static void calc (
  AADouble *closes,
  double *params,
  void (*action)(ADouble *closes, ADouble *refs)
) {
  double gap = params[0];

  ADouble **pcloses = closes->es;
  ADouble *refs = aDouble_new();
  AInt *is_solds = aInt_new();


  ADouble *row = *pcloses;
  double *prow = row->es;
  while (prow < row->end) {
    double q = *prow++;
    aDouble_push(refs, q * (1 - gap));
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
          aDouble_push(new_refs, q * (1 - gap));
        } else {
          aInt_push(new_is_solds, 1);
          double new_ref = q * (1 + gap);
          aDouble_push(new_refs, new_ref < ref ? new_ref : ref);
        }
      } else {
        if (q < ref) {
          aInt_push(new_is_solds, 1);
          aDouble_push(new_refs, q * (1 + gap));
        } else {
          aInt_push(new_is_solds, 0);
          double new_ref = q * (1 - gap);
          aDouble_push(new_refs, new_ref > ref ? new_ref : ref);
        }
      }
    }

    refs = new_refs;
    is_solds = new_is_solds;

    action(row, refs);
  }
}

Model *qmob_new (void) {
  return model_new(
    "QMOV",
    "Quantum móvil",
    qmobDoc_get(),
    achar_new_from("Intervalo", NULL),
    aDouble_new_c(1, (double[]){0.10})->es,
    aDouble_new_c(1, (double[]){0.005})->es,
    aDouble_new_c(1, (double[]){0.001})->es,
    calc
  );
}
