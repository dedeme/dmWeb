// Copyright 28-Nov-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/RankEntry.h"
#include "data/dfleas/dfleas__models.h"

/* .
RankEntry: serial
  model_name: char *
  flea: Flea
*/
/*--*/

struct RankEntry_RankEntry {
  char *model_name;
  Flea *flea;
};

RankEntry *rankEntry_new (char *model_name, Flea *flea) {
  RankEntry *this = MALLOC(RankEntry);
  this->model_name = model_name;
  this->flea = flea;
  return this;
}

char *rankEntry_model_name (RankEntry *this) {
  return this->model_name;
}

Flea *rankEntry_flea (RankEntry *this) {
  return this->flea;
}

Js *rankEntry_to_js (RankEntry *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->model_name));
  arr_push(js, flea_to_js(this->flea));
  return js_wa(js);
}

RankEntry *rankEntry_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  RankEntry *this = MALLOC(RankEntry);
  this->model_name = js_rs(*p++);
  this->flea = flea_from_js(*p++);
  return this;
}

/*--*/

// Returns Opt<Model>
Opt *rankEntry_model (RankEntry *this) {
  return dfleas__models_get(this->model_name);
}

