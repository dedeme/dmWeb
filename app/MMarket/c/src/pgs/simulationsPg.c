// Copyright 29-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pgs/simulationsPg.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "dmc/err.h"
#include "dmc/js.h"
#include "data/models.h"
#include "db/simProfitsDb.h"

char *simulationsPg_process (Mchar *mrq) {
  char *rq = cgi_rq_string(mrq, "rq");
  if (str_eq(rq, "idata")) {
    char *model_id = cgi_rq_string(mrq, "modelId");

    Model *md = models_get(model_id);
    ASimProfitsRow *profits = simProfitsDb_read(model_id)->rows;
    Mchar *rp = mchar_new();
    mchar_put(rp, "model", model_to_js(md));
    mchar_put(rp, "profits", aSimProfitsRow_to_js(profits));
    return cgi_rp(rp);
  } else return FAIL(str_f("Unexpected value for 'rq': %s", rq));
}
