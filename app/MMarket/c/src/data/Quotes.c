// Copyright 15-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Quotes.h"
#include "dmc/DEFS.h"
#include "dmc/js.h"
#include "dmc/str.h"

Quotes *quotes_new (
  char *date, Achar *cos, Achar *dates, AADouble *opens, AADouble *closes
) {
  Quotes *this = MALLOC(Quotes);
  this->date = date;
  this->cos = cos;
  this->dates = dates;
  this->opens = opens;
  this->closes = closes;
  return this;
}

int quotes_ico(Quotes *this, char *co) {
  /**/int findex (char *c) { return str_eq(c, co); }
  return achar_index(this->cos, findex);
}

char *quotes_to_js(Quotes *this) {
  return js_wa(achar_new_from(
    js_ws(this->date),
    achar_to_js(this->cos),
    achar_to_js(this->dates),
    aADouble_to_js(this->opens),
    aADouble_to_js(this->closes),
    NULL
  ));
}

ADouble *quotes_closes(Quotes *this, int ico) {
  ADouble *r = aDouble_new();
  ADouble **pcloses = (this->closes)->es;
  while (pcloses < (this->closes)->end) {
    double *cls = (*pcloses++)->es;
    aDouble_push(r, cls[ico]);
  }
  return r;
}

Quotes *quotes_mk_single(Quotes *this, int ico) {
  char *date = this->date;
  Achar *cos = achar_new_from((this->cos)->es[ico], NULL);
  Achar *dates = this->dates;
  AADouble *opens = aADouble_new();
  ADouble **popens = (this->opens)->es;
  while (popens < (this->opens)->end) {
    ADouble *all_ops = *popens++;
    ADouble *ops = aDouble_new_c(1, (double[]){all_ops->es[ico]});
    aADouble_push(opens, ops);
  }
  AADouble *closes = aADouble_new();
  ADouble **pcloses = (this->closes)->es;
  while (pcloses < (this->closes)->end) {
    ADouble *all_cls = *pcloses++;
    ADouble *cls = aDouble_new_c(1, (double[]){all_cls->es[ico]});
    aADouble_push(closes, cls);
  }

  return quotes_new(date, cos, dates, opens, closes);
}

Quotes *quotes_from_js(char *js) {
  Achar *a = js_ra(js);
  return quotes_new(
    js_rs(achar_get(a, 0)),
    achar_from_js(achar_get(a, 1)),
    achar_from_js(achar_get(a, 2)),
    aADouble_from_js(achar_get(a, 3)),
    aADouble_from_js(achar_get(a, 4))
  );
}

