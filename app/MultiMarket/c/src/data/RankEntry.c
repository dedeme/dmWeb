// Copyright 28-Nov-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/RankEntry.h"
#include "data/dfleas/dfleas__models.h"

/* .
RankEntry: serial
  model_name: char *
  flea: Flea
  assets: int
  points: int
*/
/*--*/

struct RankEntry_RankEntry {
  char *model_name;
  Flea *flea;
  int assets;
  int points;
};

RankEntry *rankEntry_new (
  char *model_name,
  Flea *flea,
  int assets,
  int points
) {
  RankEntry *this = MALLOC(RankEntry);
  this->model_name = model_name;
  this->flea = flea;
  this->assets = assets;
  this->points = points;
  return this;
}

char *rankEntry_model_name (RankEntry *this) {
  return this->model_name;
}

Flea *rankEntry_flea (RankEntry *this) {
  return this->flea;
}

int rankEntry_assets (RankEntry *this) {
  return this->assets;
}

int rankEntry_points (RankEntry *this) {
  return this->points;
}

Js *rankEntry_to_js (RankEntry *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->model_name));
  arr_push(js, flea_to_js(this->flea));
  arr_push(js, js_wi((int)this->assets));
  arr_push(js, js_wi((int)this->points));
  return js_wa(js);
}

RankEntry *rankEntry_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  RankEntry *this = MALLOC(RankEntry);
  this->model_name = js_rs(*p++);
  this->flea = flea_from_js(*p++);
  this->assets = js_ri(*p++);
  this->points = js_ri(*p++);
  return this;
}

/*--*/

// Returns Opt<Model>
Opt *rankEntry_model (RankEntry *this) {
  return dfleas__models_get(this->model_name);
}

