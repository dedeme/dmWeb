// Copyright 15-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "external/quotesReader.h"
#include <stdlib.h>
#include "dmc/DEFS.h"
#include "dmc/date.h"
#include "dmc/str.h"
#include "dmc/path.h"
#include "dmc/file.h"
#include "dmc/js.h"
#include "data/cts.h"
#include "data/dateFns.h"

// Resets values <= 0
static void normalize (AADouble *qs) {
  for (int d = 1; d < aADouble_size(qs); ++d) {
    ADouble *row = aADouble_get(qs, d);
    for (int c = 0; c < aDouble_size(row); ++c) {
      if (aDouble_get(row, c) <= 0)
        aDouble_set(row, c, aDouble_get(aADouble_get(qs, d - 1), c));
    }
  }

  for (int d = aADouble_size(qs) - 2; d >= 0; --d) {
    ADouble *row = aADouble_get(qs, d);
    for (int c = 0; c < aDouble_size(row); ++c) {
      if (aDouble_get(row, c) <= 0)
        aDouble_set(row, c, aDouble_get(aADouble_get(qs, d + 1), c));
    }
  }
}

Quotes *quotesReader_read (void) {
  Achar *cos = achar_new();
  char *nkstb = path_cat(cts_qmarket_data_dir(), "Nicks.tb", NULL);
  Achar *a = js_ra(file_read(nkstb));
  Achar *cos_js = js_ra(achar_get(a, 2));
  char **pcos_js = cos_js->es;
  while (pcos_js < cos_js->end) {
    char **co_js = js_ra(*pcos_js++)->es;
    if (js_rb(co_js[2])) {
      achar_push(cos, js_rs(co_js[1]));
    }
  }
  achar_sort(cos, str_greater);

  Achar *dates = achar_new();
  char *qstb = path_cat(
    cts_qmarket_data_dir(), "quotes",
    str_cat(achar_get(cos, 0), ".tb", NULL),
    NULL
  );
  Achar *txqs = str_csplit(file_read(qstb), '\n');
  achar_reverse(txqs);
  char **ptxqs = txqs->es;
  while (ptxqs < txqs->end) achar_push(dates, str_left(*ptxqs++, 8));

  AADouble *opens = aADouble_new();
  AADouble *closes = aADouble_new();
  for (int d = 0; d < achar_size(dates); ++d) {
    ADouble *ropens = aDouble_new();
    ADouble *rcloses = aDouble_new();
    for (int c = 0; c < achar_size(cos); ++c) {
      aDouble_push(ropens, -1);
      aDouble_push(rcloses, -1);
    }
    aADouble_push(opens, ropens);
    aADouble_push(closes, rcloses);
  }

  char **pcos = cos->es;
  int ico = 0;
  while (pcos < cos->end) {
    char *co = *pcos++;

    char *qstb = path_cat(
      cts_qmarket_data_dir(), "quotes",
      str_cat(co, ".tb", NULL),
      NULL
    );
    Achar *txqs = str_csplit(file_read(qstb), '\n');
    achar_reverse(txqs);

    char **ptxqs = txqs->es;
    int idate = 0;
    while (ptxqs < txqs->end) {
      Achar *fields = str_csplit(*ptxqs++, ':');
      aDouble_set(aADouble_get(opens, idate), ico, atof(achar_get(fields, 1)));
      aDouble_set(aADouble_get(closes, idate), ico, atof(achar_get(fields, 2)));

      ++idate;
    }

    ++ico;
  }

  normalize(opens);
  normalize(closes);
  return quotes_new(dateFns_last_sunday(), cos, dates, opens, closes);
}
