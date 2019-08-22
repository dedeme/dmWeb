// Copyright 07-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/ranking.h"
#include "dmc/cgi.h"
#include "io/conf.h"
#include "io/sbox.h"
#include "io/dailydb.h"
#include "io/fleasdb.h"
#include "data/Server.h"
#include "data/Model.h"
#include "data/dfleas/dfleas__models.h"

static void rp_data (Map *rp, int sel) {
  // Arr[RsChampions]
  Arr *ranking = fleasdb_ranking();
  // Arr[Arr[RankAssets]]
  Arr *assets = fleasdb_ranking_assets(ranking);
  // Arr[Arr[RankPositions]]
  Arr *positions = rank_mk_positions(assets);
  RsChampions *rs = arr_get(ranking, sel);
  RankFlea *rank_flea = rankFlea_new(
    rs,
    arr_get(assets, sel),
    arr_get(positions, sel)
  );
  map_put(rp, "selData", rankFlea_to_js(rank_flea));
  Model *md = opt_eget(
    dfleas__models_get(rsChampions_model(rs)),
    str_f("Model '%s' is unknown", rsChampions_model(rs))
  );
  map_put(rp, "selParamNames", arr_to_js(
    it_to(it_map(it_from(model_param_cf(md)), (FCOPY)modelMxMn_name)),
    (FTO)js_ws
  ));
  map_put(rp, "selParamFmts", model_param_jss(md));
  map_put(rp, "list", arr_to_js(
    rank_mk_ranking(ranking, positions), (FTO)rank_to_js
  ));
}

// mrq is Map[Js]
char *ranking_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq)
  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "idata")) {
    void fn () {
      int sel = conf_ranking_selected();
      map_put(rp, "sel", js_wi(sel));
      rp_data(rp, sel);
    }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "newSel")) {
    CGI_GET_INT(sel, mrq)
    void fn () {
      conf_set_ranking_selected(sel);
      rp_data(rp, sel);
   }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "idata | newSel", rq)
  return NULL; // Unreachable
}

