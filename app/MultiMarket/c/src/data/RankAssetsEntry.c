// Copyright 28-Nov-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/RankAssetsEntry.h"
#include "data/dfleas/dfleas__models.h"

/* .
RankAssetsEntry: serial
  model_name: char *
  flea: Flea
  assets: int
  points: int
*/
/*--*/

struct RankAssetsEntry_RankAssetsEntry {
  char *model_name;
  Flea *flea;
  int assets;
  int points;
};

RankAssetsEntry *rankAssetsEntry_new (
  char *model_name,
  Flea *flea,
  int assets,
  int points
) {
  RankAssetsEntry *this = MALLOC(RankAssetsEntry);
  this->model_name = model_name;
  this->flea = flea;
  this->assets = assets;
  this->points = points;
  return this;
}

char *rankAssetsEntry_model_name (RankAssetsEntry *this) {
  return this->model_name;
}

Flea *rankAssetsEntry_flea (RankAssetsEntry *this) {
  return this->flea;
}

int rankAssetsEntry_assets (RankAssetsEntry *this) {
  return this->assets;
}

int rankAssetsEntry_points (RankAssetsEntry *this) {
  return this->points;
}

Js *rankAssetsEntry_to_js (RankAssetsEntry *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->model_name));
  arr_push(js, flea_to_js(this->flea));
  arr_push(js, js_wi((int)this->assets));
  arr_push(js, js_wi((int)this->points));
  return js_wa(js);
}

RankAssetsEntry *rankAssetsEntry_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  RankAssetsEntry *this = MALLOC(RankAssetsEntry);
  this->model_name = js_rs(*p++);
  this->flea = flea_from_js(*p++);
  this->assets = js_ri(*p++);
  this->points = js_ri(*p++);
  return this;
}

/*--*/

// Returns Opt<Model>
Opt *rankAssetsEntry_model (RankAssetsEntry *this) {
  return dfleas__models_get(this->model_name);
}


