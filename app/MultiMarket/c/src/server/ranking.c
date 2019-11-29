// Copyright 07-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/ranking.h"
#include "dmc/cgi.h"
#include "io/conf.h"
#include "io/sbox.h"
#include "io/dailydb.h"
#include "io/fleasdb.h"
#include "io/rank.h"
#include "io/quotes.h"
#include "data/Server.h"
#include "data/Model.h"
#include "data/dfleas/dfleas__models.h"

static void rp_data (Map *rp, char *date, int sel) {
  // Arr<char>
  Arr *dates = rank_dates();
  int fany (char *d) {
    return str_eq(date, d);
  }
  if (!arr_any(dates, (FPRED)fany)) date = arr_peek(dates);
  map_put(rp, "dates", arr_to_js(dates, (FTO)js_ws));
  map_put(rp, "date", js_ws(date));

  // Arr[RankAssetsEntry]
  Arr *ranking = rank_fleas(date);
  // Arr<RankAssetsEntry>
  Arr *previous_ranking = rank_fleas_previous(date);

  // Arr[Arr[RankAssets]]
  Arr *assets = fleasdb_ranking_assets(ranking);
  // Arr[Arr[RankPositions]]
  Arr *positions = rank_mk_positions(assets);
  RankAssetsEntry *e = arr_get(ranking, sel);

  RsChampions *rs = opt_nget(
    fleasdb_rsChampions(rankAssetsEntry_model_name(e), rankAssetsEntry_flea(e))
  );
  if (rs) {
    RankFlea *rank_flea = rankFlea_new(
      rs,
      arr_get(assets, sel),
      arr_get(positions, sel)
    );
    map_put(rp, "selData", rankFlea_to_js(rank_flea));
  } else {
    map_put(rp, "selData", js_wn());
  }

  Model *md = opt_eget(
    rankAssetsEntry_model(e),
    str_f("Model '%s' is unknown", rankAssetsEntry_model_name(e))
  );
  map_put(rp, "selParamNames", arr_to_js(
    it_to(it_map(it_from(model_param_cf(md)), (FCOPY)modelMxMn_name)),
    (FTO)js_ws
  ));
  map_put(rp, "selParamFmts", model_param_jss(md));
  map_put(rp, "list", arr_to_js(
    rank_mk_ranking(ranking, previous_ranking), (FTO)rank_to_js
  ));
}

// mrq is Map[Js]
char *ranking_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq)
  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "idata")) {
    void fn () {
      // Arr<char>
      Arr *dates = rank_dates();
      rp_data(rp, arr_peek(dates), 0);
    }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "newSel")) {
    CGI_GET_STR(date, mrq)
    CGI_GET_INT(sel, mrq)

    void fn () {
      rp_data(rp, date, sel);
   }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "dataUpdate")) {
    void fn () {
      rank_update();
    }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "idata | newSel", rq)
  return NULL; // Unreachable
}

