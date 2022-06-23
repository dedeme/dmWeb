// Copyright 16-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/models/Ma.h"
#include <stddef.h>
#include "dmc/ADouble.h"
#include "dmc/AInt.h"
#include "data/modelDocs/maDoc.h"

static void calc (
  AADouble *closes,
  double *params,
  void (*action)(ADouble *closes, ADouble *refs)
) {
  int days = params[0] + 0.4;
  double strip = params[1];
  int ncos = aDouble_size(*closes->es);

  ADouble *irefs = aDouble_new();
  ADouble *sums = aDouble_new();
  for (int i = 0; i < ncos; ++i) {
    aDouble_push(irefs, -1);
    aDouble_push(sums, 0);
  }

  ADouble **pcloses = closes->es;

  for (int d = 0; d < days; ++d) {
    ADouble *row = *pcloses++;
    ADouble *new_sums = aDouble_new();

    double *prow = row->es;
    double *psums = sums->es;
    while (prow < row->end) {
      aDouble_push(new_sums, *psums + *prow);
      psums++;
      prow++;
    }
    sums = new_sums;

    action(row, irefs);
  }

  ADouble *row = *pcloses;
  AInt *is_solds = aInt_new();
  ADouble *avgs = aDouble_new();
  ADouble *refs = aDouble_new();
  double *prow = row->es;
  double *psums = sums->es;
  while (psums < sums->end) {
    double q = *prow++;
    double avg = *psums++ / days;
    double is_sold = q < avg;
    aDouble_push(avgs, avg);
    aInt_push(is_solds, is_sold);
    aDouble_push(refs, avg * (is_sold ? 1 + strip : 1 - strip));
  }

  ADouble **pcloses2 = closes->es;
  while (pcloses < closes->end) {
    ADouble *row = *pcloses++;
    ADouble *row2 = *pcloses2++;
    ADouble *new_avgs = aDouble_new();
    ADouble *new_refs = aDouble_new();
    AInt *new_is_solds = aInt_new();

    double *prow = row->es;
    double *prow2 = row2->es;
    double *pavgs = avgs->es;
    double *prefs = refs->es;
    int *pis_solds = is_solds->es;
    while (prow < row->end) {
      double q = *prow++;
      double q2 = *prow2++;
      double avg = *pavgs++;
      double ref = *prefs++;
      int is_sold = *pis_solds++;

      double new_avg = avg + (q - q2) / days;
      aDouble_push(new_avgs, new_avg);
      if (is_sold) {
        if (q > ref) {
          aInt_push(new_is_solds, 0);
          double new_ref = new_avg * (1 - strip);
          aDouble_push(new_refs, new_ref);
        } else {
          aInt_push(new_is_solds, 1);
          double new_ref = new_avg * (1 + strip);
          aDouble_push(new_refs, new_ref < ref ? new_ref : ref);
        }
      } else {
        if (q < ref) {
          aInt_push(new_is_solds, 1);
          double new_ref = new_avg * (1 + strip);
          aDouble_push(new_refs, new_ref);
        } else {
          aInt_push(new_is_solds, 0);
          double new_ref = new_avg * (1 - strip);
          aDouble_push(new_refs, new_ref > ref ? new_ref : ref);
        }
      }
    }

    avgs = new_avgs;
    refs = new_refs;
    is_solds = new_is_solds;

    action(row, refs);
  }
}

Model *ma_new (void) {
  return model_new(
    "MM",
    "Media móvil",
    maDoc_get(),
    achar_new_from("Dias", "Banda", NULL),
    aDouble_new_c(2, (double[]){15, 0.01})->es,
    aDouble_new_c(2, (double[]){5, 0.005})->es,
    aDouble_new_c(2, (double[]){1, 0.001})->es,
    calc
  );
}
