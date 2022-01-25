// Copyright 14-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Ann.h"
#include "dmc/err.h"
#include "dmc/str.h"
#include "dmc/sys.h"
#include "dmc/js.h"
#include "dmc/char/Achar.h"

Ann *ann_new (int id, int time_type, char *data, int text_type, char *text) {
  Ann *this = MALLOC(Ann);
  this->id = id;
  this->time_type = time_type;
  this->data = data;
  this->text_type = text_type;
  this->text = text;
  return this;
}

AInt *ann_days (Ann *a) {
  if (a->time_type != ann_PERIODIC)
    FAIL(str_f("time_type must be %d, but it is %d", ann_PERIODIC, a->time_type));
  Achar *arr = js_ra(a->data);
  return aInt_from_js(achar_get(arr, 1));
}

time_t ann_date (Ann *a) {
  if (a->time_type == ann_INIT || a->time_type == ann_NOTE)
    FAIL("time_type must not be ann_INIT or ann_NOTE");
  if (a->time_type == ann_FIX)
    return (time_t)js_rd(a->data);
  Achar *arr = js_ra(a->data);
  return (time_t)js_rd(achar_get(arr, 0));
}

char *ann_group (Ann *a) {
  if (a->time_type != ann_NOTE)
    FAIL(str_f("time_type must be %d, but it is %d", ann_NOTE, a->time_type));
  return js_rs(a->data);
}

void ann_set_id (Ann *this, int id) {
  this->id = id;
}

char *ann_run (Ann *this) {
  char *tx = this->text;
  if (this->text_type == ann_COMMAND) {
    char *rp = ochar_nsome(sys_cmd(str_f("%s 2>&1", tx)));
    if (rp) return str_f("[%s]\n%s", tx, rp);
    return str_f("[%s]\nFail.");
  } else {
    return tx;
  }
}

char *ann_to_js (Ann *this) {
  return js_wa(achar_new_from(
    js_wi(this->id),
    js_wi(this->time_type),
    this->data,
    js_wi(this->text_type),
    js_ws(this->text),
    NULL
  ));
}

Ann *ann_from_js (char *js) {
  char **p = js_ra(js)->es;
  Ann *this = MALLOC(Ann);
  this->id = js_ri(*p++);
  this->time_type = js_ri(*p++);
  this->data = *p++;
  this->text_type = js_ri(*p++);
  this->text = js_rs(*p++);
  return this;
}

