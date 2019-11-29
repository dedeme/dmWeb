// Copyright 28-Nov-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/RankEvalEntry.h"
#include "data/dfleas/dfleas__models.h"

/* .
RankEvalEntry: serial
  model_name: char *
  flea: Flea
  assets: double
  # avg moduled with variance
  profits: double
  days: double
  @points: double
*/
/*--*/

struct RankEvalEntry_RankEvalEntry {
  char *model_name;
  Flea *flea;
  double assets;
  double profits;
  double days;
  double points;
};

RankEvalEntry *rankEvalEntry_new (
  char *model_name,
  Flea *flea,
  double assets,
  double profits,
  double days,
  double points
) {
  RankEvalEntry *this = MALLOC(RankEvalEntry);
  this->model_name = model_name;
  this->flea = flea;
  this->assets = assets;
  this->profits = profits;
  this->days = days;
  this->points = points;
  return this;
}

char *rankEvalEntry_model_name (RankEvalEntry *this) {
  return this->model_name;
}

Flea *rankEvalEntry_flea (RankEvalEntry *this) {
  return this->flea;
}

double rankEvalEntry_assets (RankEvalEntry *this) {
  return this->assets;
}

double rankEvalEntry_profits (RankEvalEntry *this) {
  return this->profits;
}

double rankEvalEntry_days (RankEvalEntry *this) {
  return this->days;
}

double rankEvalEntry_points (RankEvalEntry *this) {
  return this->points;
}

void rankEvalEntry_set_points (RankEvalEntry *this, double value) {
  this->points = value;
}

Js *rankEvalEntry_to_js (RankEvalEntry *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->model_name));
  arr_push(js, flea_to_js(this->flea));
  arr_push(js, js_wd(this->assets));
  arr_push(js, js_wd(this->profits));
  arr_push(js, js_wd(this->days));
  arr_push(js, js_wd(this->points));
  return js_wa(js);
}

RankEvalEntry *rankEvalEntry_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  RankEvalEntry *this = MALLOC(RankEvalEntry);
  this->model_name = js_rs(*p++);
  this->flea = flea_from_js(*p++);
  this->assets = js_rd(*p++);
  this->profits = js_rd(*p++);
  this->days = js_rd(*p++);
  this->points = js_rd(*p++);
  return this;
}

/*--*/

// Returns Opt<Model>
Opt *rankEvalEntry_model (RankEvalEntry *this) {
  return dfleas__models_get(this->model_name);
}


