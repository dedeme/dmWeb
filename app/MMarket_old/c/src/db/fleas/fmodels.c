// Copyright 20-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db/fleas/fmodels.h"
#include "data/flea/fmodels.h"
#include "scheduler/fleas.h"
#include "DEFS.h"

static char *path (char *model) {
  return path_cat(sys_home(), FLEAS_PATH, model, NULL);
}

static char *poolPath (char *model) {
  return path_cat(path(model), "Pool.db", NULL);
}

static char *bestsPath (char *model) {
  return path_cat(path(model), "Bests.db", NULL);
}

static char *rankingPath (char *model) {
  return path_cat(path(model), "Ranking.db", NULL);
}

void fmodels_init (void) {
  EACH(fmodels_list(), Fmodel, model) {
    char *id = fmodel_id(model);
    char *p = path(id);
    if (!file_exists(p)) {
      file_mkdir(p);
      file_write(poolPath(id), "[]");
      file_write(bestsPath(id), "[]");
      file_write(rankingPath(id), "[]");
    }
  }_EACH
}

// Returns Arr[FleasEval]
Arr *fmodels_read_pool (Fmodel *model) {
  char *p = poolPath(fmodel_id(model));
  if (file_exists(p))
    return arr_from_js((Js*)file_read(p), (FFROM)fleasEval_from_js);
  return arr_new();
}

void fmodels_write_pool (
  Fmodel *model,
  Arr *pool // Arr[FleasEval]
) {
  char *p = poolPath(fmodel_id(model));
  if (file_exists(p))
    file_write(p, (char *)arr_to_js(pool, (FTO)fleasEval_to_js));
}

// Returns Arr[FleasEval]
Arr *fmodels_read_bests (Fmodel *model) {
  char *p = bestsPath(fmodel_id(model));
  if (file_exists(p))
    return arr_from_js((Js*)file_read(p), (FFROM)fleasEval_from_js);
  return arr_new();
}

void fmodels_write_bests (
  Fmodel *model,
  Arr *bests // Arr[FleasEval]
) {
  char *p = bestsPath(fmodel_id(model));
  if (file_exists(p))
    file_write(p, (char *)arr_to_js(bests, (FTO)fleasEval_to_js));
}

// Returns Arr[Arr[FleasEval]](Array 10 (days) of 40 (fleas)).
Arr *fmodels_read_ranking (Fmodel *model) {
  char *p = rankingPath(fmodel_id(model));
  if (file_exists(p)) {
    // Arr[js]
    Arr *ajs = js_ra((Js*)file_read(p));
    // Arr[FleasEval]
    Arr *fn(Js *js) { return arr_from_js(js, (FFROM)fleasEval_from_js); }
    return arr_map(ajs, (FCOPY)fn);
  }
  return arr_new();
}

void fmodels_write_ranking (
  Fmodel *model,
  Arr *ranking // Arr[Arr[FleasEval]] (Array 10 (days) of 40 (fleas)).
) {
  char *p = rankingPath(fmodel_id(model));
  if (file_exists(p)) {
    // fleas is Arr[FleasEval]
    Js *fn(Arr *efleas) { return arr_to_js(efleas, (FTO)fleasEval_to_js); }
    // Arr[js]
    Arr *ajs = arr_map(ranking, (FCOPY) fn);
    file_write(p, (char *)js_wa(ajs));
  }
}
