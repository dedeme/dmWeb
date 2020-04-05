// Copyright 23-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/fleas/charts/summary.h"
#include "dmc/cgi.h"
#include "data/flea/Fmodel.h"
#include "data/flea/fmodels.h"
#include "db/quotes.h"
#include "chart/summary.h"

char *summary_process (AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq);

  if (str_eq(rq, "idata")) {
    CGI_GET_STR(modelId, mrq)
    CGI_GET(Flea *, flea, flea_from_js, mrq)
    // Map[Js]
    Map *rp = map_new();
    char *error = "";
    void fn () {
      // Arr[char]
      Arr *dates = quotes_dates();
      if (!arr_size(dates)) {
        error = "Fail reading quote dates";
        return;
      }
      Qtable *opens = opt_nget(quotes_opens());
      if (!opens) {
        error = "Fail reading quote opens";
        return;
      }
      Qtable *closes = opt_nget(quotes_opens());
      if (!closes) {
        error = "Fail reading quote closess";
        return;
      }
      Fmodel *model = opt_nget(fmodels_get(modelId));
      if (!model) {
        error = "Fail reading model";
        return;
      }
      // Arr[SummaryData]
      Arr *historic = summary_historic(
        model, dates, opens, closes, flea_gen(flea)
      );
      map_put(rp, "historic", arr_to_js(historic, (FTO)summaryData_to_js));
    }
    asyncActor_wait(ac, fn);

    map_put(rp, "error", js_ws(error));

    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "idata", rq)
  return NULL; // Unreachable
}


