// Copyright 23-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/fleas/charts/main.h"
#include "dmc/cgi.h"
#include "data/flea/fmodels.h"
#include "data/flea/Fmodel.h"
#include "db/fleas/fmodels.h"
#include "scheduler/fleas.h"
#include "DEFS.h"

char *mainCharts_process (AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq);

  if (str_eq(rq, "bestRanking")) {
    CGI_GET_STR(modelId, mrq);

    // Map[Js]
    Map *rp = map_new();
    void fn () {
      Fmodel *model = opt_nget(fmodels_get(modelId));
      if (!model) {
        map_put(rp, "eflea", opt_to_js(opt_empty(), (FTO)fleasEval_to_js));
        return;
      }
      //Arr[Arr[Investor]
      Arr *rank = fmodels_read_ranking(model);
      if (!arr_size(rank) || !arr_size(arr_get(rank, 0))) {
        map_put(rp, "eflea", opt_to_js(opt_empty(), (FTO)fleasEval_to_js));
        return;
      }
      FleasEval *eflea = arr_get(arr_get(rank, arr_size(rank) - 1), 0);
      map_put(rp, "eflea", opt_to_js(opt_new(eflea), (FTO)fleasEval_to_js));
    }
    asyncActor_wait(ac, fn);

    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "bestRanking", rq)
  return NULL; // Unreachable
}



