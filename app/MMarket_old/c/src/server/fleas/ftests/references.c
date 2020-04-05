// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/fleas/ftests/references.h"
#include "dmc/cgi.h"
#include "data/flea/fmodels.h"
#include "data/flea/Fmodel.h"
#include "data/Nick.h"
#include "db/quotes.h"
#include "db/nicks.h"
#include "DEFS.h"

char *references_process (AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq);

  if (str_eq(rq, "nickList")) {
    char *getName (Nick *nick) { return nick_name(nick); }
    // Map[Js]
    Map *rp = map_new();
    map_put(rp, "nickList", arr_to_js(
      arr_map(nicks_selected_list(), (FCOPY)getName),
      (FTO)js_ws
    ));
    return cgi_ok(rp);
  }
  if (str_eq(rq, "chartData")) {
    CGI_GET_STR(modelId, mrq)
    CGI_GET_STR(nickName, mrq)
    CGI_GET(Darr *, params, darr_from_js, mrq)
    // Map[Js]
    Map *rp = map_new();

    char * error = "";
    void fn () {
      Qtable *opens = opt_nget(quotes_opens());
      if (!opens) { error = "Quotes data base can not be read"; return; }
      double *nick_opens = opt_nget(qtable_nick_values(opens, nickName));
      if (!nick_opens) { error = "Nick not found"; return; }

      Qtable *closes = opt_nget(quotes_closes());
      if (!closes) { error = "Quotes data base can not be read"; return; }
      double *nick_closes = opt_nget(qtable_nick_values(closes, nickName));
      if (!nick_closes) { error = "Nick not found"; return; }

      int fn2 (Fmodel *m) { return str_eq(fmodel_id(m), modelId); }
      Fmodel *model = opt_nget(
        it_find(arr_to_it(fmodels_list()), (FPRED)fn2)
      );
      if (!model) { error = "Model not found"; return; }

      map_put(rp, "dates", arr_to_js(quotes_dates(), (FTO)js_ws));
      map_put(rp, "opens", darr_to_js(
        darr_new_c(HISTORIC_QUOTES, nick_opens)
      ));
      map_put(rp, "closes", darr_to_js(
        darr_new_c(HISTORIC_QUOTES, nick_closes)
      ));
      map_put(rp, "refs", darr_to_js(
        darr_new_c(HISTORIC_QUOTES, fmodel_refs(model, nick_closes, params))
      ));
      map_put(rp, "profits", js_wd(
        fmodel_profits(model, nick_opens, nick_closes, params)
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

