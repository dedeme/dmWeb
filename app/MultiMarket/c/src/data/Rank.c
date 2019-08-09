// Copyright 07-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Rank.h"

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
  model: char *
  flea: char *
  is_new: bool
  # If is_new == 1, variation == 0
  variation: int
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
  char *model;
  char *flea;
  int is_new;
  int variation;
};

Rank *rank_new (
  char *model,
  char *flea,
  int is_new,
  int variation
) {
  Rank *this = MALLOC(Rank);
  this->model = model;
  this->flea = flea;
  this->is_new = is_new;
  this->variation = variation;
  return this;
}

char *rank_model (Rank *this) {
  return this->model;
}

char *rank_flea (Rank *this) {
  return this->flea;
}

int rank_is_new (Rank *this) {
  return this->is_new;
}

int rank_variation (Rank *this) {
  return this->variation;
}

Js *rank_to_js (Rank *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->model));
  arr_push(js, js_ws(this->flea));
  arr_push(js, js_wb(this->is_new));
  arr_push(js, js_wi((int)this->variation));
  return js_wa(js);
}

Rank *rank_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  Rank *this = MALLOC(Rank);
  this->model = js_rs(*p++);
  this->flea = js_rs(*p++);
  this->is_new = js_rb(*p++);
  this->variation = js_ri(*p++);
  return this;
}

/*--*/

// Returns Arr[Arr[RankPosition]]
// assets are Arr[Arr[RankAssets]]
Arr *rank_mk_positions (Arr *assets) {
  if (!arr_size(assets)){
    return arr_new();
  }

  // Arr[Arr[RankPosition]]
  Arr *r = arr_new();
  REPEAT(arr_size(assets)) {
    arr_push(r, arr_new());
  }_REPEAT

  RANGE0(col, arr_size(arr_get(assets, 0))) {
    RANGE0(row, arr_size(assets)) {
      RankAssets *rk_a = arr_get(arr_get(assets, row), col);
      int pos = 0;
      EACH(assets, Arr, rk_row) {
        RankAssets *rk_a2 = arr_get(rk_row, col);
        if (rankAssets_assets(rk_a2) > rankAssets_assets(rk_a)) {
          ++pos;
        }
      }_EACH
      arr_push(arr_get(r, row), rankPosition_new(
        rankAssets_date(rk_a), pos
      ));
    }_RANGE
  }_RANGE

  return r;
}

// Returns Arr[Rank]
// rss: Arr[RsChampions]
// positions: Arr[Arr[RankPosition]]
Arr *rank_mk_ranking (Arr *rss, Arr *positions) {
  // Arr[Rank]
  Arr *r = arr_new();

  RsChampions **prs = (RsChampions **)arr_start(rss);
  Arr **poss = (Arr **)arr_start(positions);
  char *last_date = "00000000";
  RANGE0(i, arr_size(rss)) {
    RsChampions *rs = *prs++;
    // Arr[RankPosition]
    Arr *apos = *poss++;
    RankPosition *last_pos = arr_get(apos, arr_size(apos) - 1);

    int dif = rankPosition_position(arr_get(apos, arr_size(apos) - 2)) -
      rankPosition_position(last_pos);

    char *fname = flea_name(rs_flea(rsWeb_result(rsChampions_result(rs))));
    char *date = str_left(fname, str_cindex(fname, '-'));
    if (str_greater(date, last_date)) {
      last_date = date;
    }
    arr_push(r, rank_new(
      rsChampions_model(rs),
      date,
      0,
      dif > 3 ? 2
        : dif > 0 ? 1
          : dif < -3 ? -2
            : dif < 0 ? -1
              : 0
    ));
  }_RANGE

  EACH(r, Rank, rk) {
    if (str_eq(rk->flea, last_date)) {
      rk->is_new = 1;
      rk->variation = 0;
    }
  }_EACH

  return r;
}
