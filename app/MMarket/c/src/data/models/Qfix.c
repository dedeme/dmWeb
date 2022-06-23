// Copyright 16-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/models/Qfix.h"
#include <stddef.h>
#include <math.h>
#include "dmc/ADouble.h"
#include "data/modelDocs/qfixDoc.h"

static void calc (
  AADouble *closes,
  double *params,
  void (*action)(ADouble *closes, ADouble *refs)
) {
  double jmp = *params + 1;
  double lgJmp = log(jmp);

  /**/double down_gap (double q) { return pow(jmp, round(log(q) / lgJmp) - 1); }
  /**/double up_gap (double q) { return pow(jmp, round(log(q) / lgJmp) + 1); }
  /**/double down_gap2 (double q, double ref) {
  /**/  for (;;) {
  /**/    double ref2 = ref * jmp;
  /**/    if (ref2 * sqrt(jmp) >= q) return ref;
  /**/    ref = ref2;
  /**/  }
  /**/}
  /**/double up_gap2 (double q, double ref) {
  /**/  for (;;) {
  /**/    double ref2 = ref / jmp;
  /**/    if (ref2 / sqrt(jmp) <= q) return ref;
  /**/    ref = ref2;
  /**/  }
  /**/}

  ADouble *pvrow = aDouble_copy(*(closes->es));
  ADouble *refs = aDouble_new();
  double *ppvrow = pvrow->es;
  while (ppvrow < pvrow->end) aDouble_push(refs, down_gap(*ppvrow++) / jmp);

  ADouble **pcloses = closes->es;
  while (pcloses < closes->end) {
    ADouble *row = *pcloses++;
    ADouble *new_refs = aDouble_new();

    double *prow = row->es;
    double *ppvrow = pvrow->es;
    double *prefs = refs->es;
    while (prow < row->end) {
      double q0 = *ppvrow++;
      double q = *prow++;
      double ref = *prefs++;

      if (q0 < ref) {
        if (q < q0) {
          aDouble_push(new_refs, up_gap2(q, ref));
        } else if (q > ref) {
          aDouble_push(new_refs, down_gap(q));
        } else {
          aDouble_push(new_refs, ref);
        }
      } else {
        if (q > q0) {
          aDouble_push(new_refs, down_gap2(q, ref));
        } else if (q < ref) {
          aDouble_push(new_refs, up_gap(q));
        } else {
          aDouble_push(new_refs, ref);
        }
      }
    }

    pvrow = row;
    refs = new_refs;
    action(row, refs);
  }

}

Model *qfix_new (void) {
  return model_new(
    "QFIJO",
    "Quantum fijo",
    qfixDoc_get(),
    achar_new_from("Intervalo", NULL),
    aDouble_new_c(1, (double[]){0.14})->es,
    aDouble_new_c(1, (double[]){0.005})->es,
    aDouble_new_c(1, (double[]){0.001})->es,
    calc
  );
}
