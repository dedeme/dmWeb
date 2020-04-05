// Copyright 19-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/fleas/ftests/orders.h"
#include "dmc/cgi.h"
#include "data/flea/fmodels.h"
#include "data/flea/Fmodel.h"
#include "data/Nick.h"
#include "db/quotes.h"
#include "DEFS.h"

char *orders_process (AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq);

  if (str_eq(rq, "ordersData")) {
    CGI_GET_STR(modelId, mrq)
    CGI_GET(Darr *, params, darr_from_js, mrq)
    // Map[Js]
    Map *rp = map_new();

    char * error = "";
    void fn () {
      // Arr[char]
      Arr *dates = quotes_dates();
      if (!arr_size(dates)) { error = "Dates can not be read"; return; }

      Qtable *opens = opt_nget(quotes_opens());
      if (!opens) { error = "Quotes data base can not be read"; return; }

      Qtable *closes = opt_nget(quotes_closes());
      if (!closes) { error = "Quotes data base can not be read"; return; }

      int fn2 (Fmodel *m) { return str_eq(fmodel_id(m), modelId); }
      Fmodel *model = opt_nget(
        it_find(arr_to_it(fmodels_list()), (FPRED)fn2)
      );
      if (!model) { error = "Model not found"; return; }

      char *fn3 (Nick *nk) { return nick_name(nk); }
      // Arr[char *]
      Arr *nk_names = arr_map(qtable_nicks(opens), (FCOPY)fn3);
      Darr *last_closes = darr_new_c(
        arr_size(qtable_nicks(closes)),
        qtable_values(closes)[HISTORIC_QUOTES - 1]
      );

      map_put(rp, "nicks", arr_to_js(nk_names, (FTO)js_ws));
      map_put(rp, "lastCloses", darr_to_js(last_closes));
      map_put(rp, "orders", arr_to_js(
        fmodel_orders(model, dates, opens, closes, params),
        (FTO)fmodelOrder_to_js
      ));
      map_put(rp, "assets", rs_to_js(
        fmodel_assets(model, opens, closes, params)
      ));
    }
    asyncActor_wait(ac, fn);

    if (*error)
      return cgi_error(error);
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "chartData", rq)
  return NULL; // Unreachable
}


