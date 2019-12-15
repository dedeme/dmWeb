// Copyright 07-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Rank.h"
#include "dmc/Dec.h"

/* .
# Pair date-double to use with charts.
RankAssets: serial
  date: char *
  assets: double
===
# Pair date-int to use with charts.
RankPosition: serial
  date: char *
  position: int
===
# Data to send to client.
RankFlea: serial
  result: RsChampions
  # Arr[RankAssets]
  assets: Arr - RankAssets
  # Arr[RankPosition]
  positions: Arr - RankPosition
===
# Pair date-int to use with charts.
Rank: serial
  is_new: bool
  # If is_new == 1, variation == 0. Otherwise variation can be in range [-2 - 2]
  variation: int
  points: int
  assets: int
  model: char *
  flea: char *
*/

/*--*/

struct Rank_RankAssets {
  char *date;
  double assets;
};

RankAssets *rankAssets_new (char *date, double assets) {
  RankAssets *this = MALLOC(RankAssets);
  this->date = date;
  this->assets = assets;
  return this;
}

char *rankAssets_date (RankAssets *this) {
  return this->date;
}

double rankAssets_assets (RankAssets *this) {
  return this->assets;
}

Js *rankAssets_to_js (RankAssets *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->date));
  arr_push(js, js_wd(this->assets));
  return js_wa(js);
}

RankAssets *rankAssets_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  RankAssets *this = MALLOC(RankAssets);
  this->date = js_rs(*p++);
  this->assets = js_rd(*p++);
  return this;
}

struct Rank_RankPosition {
  char *date;
  int position;
};

RankPosition *rankPosition_new (char *date, int position) {
  RankPosition *this = MALLOC(RankPosition);
  this->date = date;
  this->position = position;
  return this;
}

char *rankPosition_date (RankPosition *this) {
  return this->date;
}

int rankPosition_position (RankPosition *this) {
  return this->position;
}

Js *rankPosition_to_js (RankPosition *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->date));
  arr_push(js, js_wi((int)this->position));
  return js_wa(js);
}

RankPosition *rankPosition_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  RankPosition *this = MALLOC(RankPosition);
  this->date = js_rs(*p++);
  this->position = js_ri(*p++);
  return this;
}

struct Rank_RankFlea {
  RsChampions *result;
  Arr *assets;
  Arr *positions;
};

RankFlea *rankFlea_new (RsChampions *result, Arr *assets, Arr *positions) {
  RankFlea *this = MALLOC(RankFlea);
  this->result = result;
  this->assets = assets;
  this->positions = positions;
  return this;
}

RsChampions *rankFlea_result (RankFlea *this) {
  return this->result;
}

Arr *rankFlea_assets (RankFlea *this) {
  return this->assets;
}

Arr *rankFlea_positions (RankFlea *this) {
  return this->positions;
}

Js *rankFlea_to_js (RankFlea *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, rsChampions_to_js(this->result));
  arr_push(js, arr_to_js(this->assets, (FTO)rankAssets_to_js));
  arr_push(js, arr_to_js(this->positions, (FTO)rankPosition_to_js));
  return js_wa(js);
}

RankFlea *rankFlea_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  RankFlea *this = MALLOC(RankFlea);
  this->result = rsChampions_from_js(*p++);
  this->assets = arr_from_js(*p++, (FFROM)rankAssets_from_js);
  this->positions = arr_from_js(*p++, (FFROM)rankPosition_from_js);
  return this;
}

struct Rank_Rank {
  int is_new;
  int variation;
  int points;
  int assets;
  char *model;
  char *flea;
};

Rank *rank_new (
  int is_new,
  int variation,
  int points,
  int assets,
  char *model,
  char *flea
) {
  Rank *this = MALLOC(Rank);
  this->is_new = is_new;
  this->variation = variation;
  this->points = points;
  this->assets = assets;
  this->model = model;
  this->flea = flea;
  return this;
}

int rank_is_new (Rank *this) {
  return this->is_new;
}

int rank_variation (Rank *this) {
  return this->variation;
}

int rank_points (Rank *this) {
  return this->points;
}

int rank_assets (Rank *this) {
  return this->assets;
}

char *rank_model (Rank *this) {
  return this->model;
}

char *rank_flea (Rank *this) {
  return this->flea;
}

Js *rank_to_js (Rank *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_wb(this->is_new));
  arr_push(js, js_wi((int)this->variation));
  arr_push(js, js_wi((int)this->points));
  arr_push(js, js_wi((int)this->assets));
  arr_push(js, js_ws(this->model));
  arr_push(js, js_ws(this->flea));
  return js_wa(js);
}

Rank *rank_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  Rank *this = MALLOC(Rank);
  this->is_new = js_rb(*p++);
  this->variation = js_ri(*p++);
  this->points = js_ri(*p++);
  this->assets = js_ri(*p++);
  this->model = js_rs(*p++);
  this->flea = js_rs(*p++);
  return this;
}

/*--*/

// Returns Arr[Arr[RankPosition]]
// assets are Arr[Arr[RankAssets]]
Arr *rank_mk_positions (Arr *assets) {
  int asz = arr_size(assets);
  if (!asz){
    return arr_new();
  }

  // Arr[Arr[RankPosition]]
  Arr *r = arr_new();
  REPEAT(arr_size(assets)) {
    arr_push(r, arr_new());
  }_REPEAT

  RANGE0(col, arr_size(arr_get(assets, 0))) {
    RANGE0(row, arr_size(assets)) {
      // Arr[RankAssets]
      Arr *fl_assets = arr_get(assets, row);
      if (arr_size(fl_assets)) {
        int pos = 0;
        RankAssets *rk_a = arr_get(fl_assets, col);
        EACH(assets, Arr, rk_row) {
          if (col < arr_size(rk_row)) {
            RankAssets *rk_a2 = arr_get(rk_row, col);
            if (rankAssets_assets(rk_a2) > rankAssets_assets(rk_a)) {
              ++pos;
            }
          }
        }_EACH
        arr_push(arr_get(r, row), rankPosition_new(
          rankAssets_date(rk_a), pos
        ));
      } else {
        EACH(arr_get(assets, 0), RankAssets, rkps) {
          arr_push(arr_get(r, row), rankPosition_new(
            rankAssets_date(rkps), asz
          ));
        }_EACH
      }
    }_RANGE
  }_RANGE

  return r;
}

// Returns Arr[Rank]
// rk: Arr[RankAssetsEntry]
// prev_rk: Arr[RankAssetsEntry]
Arr *rank_mk_ranking (Arr *rk, Arr *prev_rk) {
  int calc_df (int current, int previous) {
    int dif = previous - current;
    return dif > 3 ? 2
      : dif > 0 ? 1
        : dif < -3 ? -2
          : dif < 0 ? -1
            : 0
    ;
  }

  // Arr[Rank]
  Arr *r = arr_new();

  EACH_IX(rk, RankAssetsEntry, e, ix) {
    char *fname = flea_name(rankAssetsEntry_flea(e));
    char *mname = rankAssetsEntry_model_name(e);
    int fn (RankAssetsEntry *pre) {
      return str_eq(fname, flea_name(rankAssetsEntry_flea(pre))) &&
        str_eq(mname, rankAssetsEntry_model_name(e))
      ;
    }
    int prix = arr_index(prev_rk, (FPRED)fn);
    arr_push(r, rank_new(
      prix == -1 ? 1 : 0,
      prix == -1 ? 0 : calc_df(ix, prix),
      rankAssetsEntry_points(e),
      rankAssetsEntry_assets(e),
      mname,
      fname
    ));
  }_EACH

  return r;
}
