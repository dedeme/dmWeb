// Copyright 10-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Rconf.h"

/* .
-Rconf: SERIAL
  url: char *
  @sel: int
  is_date_eu: bool
  date_separator: char *
  is_iso_number: bool
  fields_type: char *
  table_start: char *
  table_end: char *
  row_start: char *
  row_end: char *
  cols_start: Arr - char *
  cols_end: Arr - char *
*/
/*--*/

struct Rconf_Rconf{
  char *url;
  int sel;
  int is_date_eu;
  char *date_separator;
  int is_iso_number;
  char *fields_type;
  char *table_start;
  char *table_end;
  char *row_start;
  char *row_end;
  Arr *cols_start;
  Arr *cols_end;
};

char *rconf_url(Rconf *this) {
  return this->url;
}

int rconf_sel(Rconf *this) {
  return this->sel;
}

void rconf_set_sel(Rconf *this, int value) {
  this->sel = value;
}

int rconf_is_date_eu(Rconf *this) {
  return this->is_date_eu;
}

char *rconf_date_separator(Rconf *this) {
  return this->date_separator;
}

int rconf_is_iso_number(Rconf *this) {
  return this->is_iso_number;
}

char *rconf_fields_type(Rconf *this) {
  return this->fields_type;
}

char *rconf_table_start(Rconf *this) {
  return this->table_start;
}

char *rconf_table_end(Rconf *this) {
  return this->table_end;
}

char *rconf_row_start(Rconf *this) {
  return this->row_start;
}

char *rconf_row_end(Rconf *this) {
  return this->row_end;
}

Arr *rconf_cols_start(Rconf *this) {
  return this->cols_start;
}

Arr *rconf_cols_end(Rconf *this) {
  return this->cols_end;
}

Js *rconf_to_js(Rconf *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->url));
  arr_push(js, js_wi((int)this->sel));
  arr_push(js, js_wb(this->is_date_eu));
  arr_push(js, js_ws(this->date_separator));
  arr_push(js, js_wb(this->is_iso_number));
  arr_push(js, js_ws(this->fields_type));
  arr_push(js, js_ws(this->table_start));
  arr_push(js, js_ws(this->table_end));
  arr_push(js, js_ws(this->row_start));
  arr_push(js, js_ws(this->row_end));
  arr_push(js, arr_to_js(this->cols_start, (FTO)js_ws));
  arr_push(js, arr_to_js(this->cols_end, (FTO)js_ws));
  return js_wa(js);
}

Rconf *rconf_from_js(Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  Rconf *this = MALLOC(Rconf);
  this->url = js_rs(*p++);
  this->sel = js_ri(*p++);
  this->is_date_eu = js_rb(*p++);
  this->date_separator = js_rs(*p++);
  this->is_iso_number = js_rb(*p++);
  this->fields_type = js_rs(*p++);
  this->table_start = js_rs(*p++);
  this->table_end = js_rs(*p++);
  this->row_start = js_rs(*p++);
  this->row_end = js_rs(*p++);
  this->cols_start = arr_from_js(*p++, (FFROM)js_rs);
  this->cols_end = arr_from_js(*p++, (FFROM)js_rs);
  return this;
}

/*--*/
