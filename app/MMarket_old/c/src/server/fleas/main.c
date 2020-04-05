// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/fleas/main.h"
#include "dmc/cgi.h"
#include "server/fleas/ftests/references.h"
#include "server/fleas/ftests/orders.h"
#include "server/fleas/ftests/selection.h"
#include "server/fleas/charts/main.h"
#include "server/fleas/charts/summary.h"
#include "server/fleas/charts/companies.h"
#include "data/flea/fmodels.h"
#include "data/flea/Fmodel.h"

char *mainFleas_process (AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(source, mrq);

  if (str_eq(source, "References")) return references_process(ac, mrq);
  if (str_eq(source, "Orders")) return orders_process(ac, mrq);
  if (str_eq(source, "Selection")) return selection_process(ac, mrq);
  if (str_eq(source, "MainCharts")) return mainCharts_process(ac, mrq);
  if (str_eq(source, "Summary")) { return summary_process(ac, mrq); }
  if (str_eq(source, "Companies")) { return companies_process(ac, mrq); }

  CGI_GET_STR(rq, mrq);
  if (str_eq(rq, "models")) {
    // Map[Js]
    Map *rp = map_new();
    map_put(rp, "models", arr_to_js(fmodels_list(), (FTO)fmodel_to_js));
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT(
    "source",
    "References | Orders | Selection | MainCharts | Summary | Companies | "
    "models",
    source
  )
  return NULL; // Unreachable
}

