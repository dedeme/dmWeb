// Copyright 18-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "dmc/Dec.h"
#include <math.h>
#include "dmc/std.h"

struct dec_Dec {
  double n;
  int scale;
};

double _dec_round(double dbl) {
  double d = fabs(dbl);
  return
    d > 1000000000 ? 0.000001
  : d > 100000000 ? 0.0000001
  : d > 10000000 ? 0.00000001
  : d > 1000000 ? 0.000000001
  : d > 100000 ? 0.0000000001
  : d > 10000 ? 0.00000000001
  : d > 1000 ? 0.000000000001
  : d > 100 ? 0.0000000000001
  : d > 10 ? 0.00000000000001
  :         0.000000000000001;
}

Dec *dec_new(double n, int scale) {
  Dec *this = malloc(sizeof(Dec));
  this->n = n >= 0 ? n + _dec_round(n) : n - _dec_round(n);
  this->scale = scale < 0 ? 0 : scale > 10 ? 10 : scale;

  return this;
}

void dec_free(Dec *this) {
  free(this);
}

char *dec_to_str_new(Dec *this) {
  double n = this->n;
  char *scale = str_f_new("%d", this->scale);
  char *tpl = str_cat_new("%.", scale, "f", NULL);
  char *r = str_f_new(tpl, n);
  free(scale);
  free(tpl);
  if (*r == '-' && dec_eq(n, 0.0)) {
    str_right(&r, 1);
  }
  return r;
}

int dec_eq(double d1, double d2) {
  double df1 = _dec_round(d1);
  double df2 = _dec_round(d2);
  double df = df1 > df2 ? df1 : df2;
  return d1 < d2 + df && d1 > d2 - df;
}

int dec_eq_gap(double d1, double d2, double gap) {
  return d1 < d2 + gap && d1 > d2 - gap;
}

int dec_eqf_gap(float d1, float d2, float gap) {
  return d1 < d2 + gap && d1 > d2 - gap;
}

int dec_digits(const char *s) {
  char ch;
  ch = *s++;
  while (ch) {
    if (ch < '0' || ch > '9')
      return 0;
    ch = *s++;
  }
  return 1;
}

void dec_regularize_iso(char **s) {
  str_replace(s, ".", "");
  str_creplace(s, ',', '.');
}

void dec_regularize_us(char **s) {
  str_replace(s, ",", "");
}

int dec_number(const char *s) {
  char ch;
  ch = *s++;
  if (ch == '-')
    ch = *s++;

  while (ch) {
    if (ch == '.') {
      ch = *s++;
      while (ch) {
        if (ch < '0' || ch > '9')
          return 0;
        ch = *s++;
      }
      return 1;
    }
    if (ch < '0' || ch > '9')
      return 0;
    ch = *s++;
  }
  return 1;
}

Js *dec_to_js_new(Dec *this) {
  Arr *a = arr_new(free);
  arr_push(a, js_wd_new(this->n, this->scale));
  arr_push(a, js_wi_new(this->scale));
  Js *r = js_wa_new(a);
  arr_free(a);
  return r;
}

Dec *dec_from_js_new(Js *js) {
  Dec *this = malloc(sizeof(Dec));
  Arr *a = js_ra_new(js);
  this->n = js_rd(arr_get(a, 0));
  this->scale = js_ri(arr_get(a, 1));
  arr_free(a);
  return this;
}

