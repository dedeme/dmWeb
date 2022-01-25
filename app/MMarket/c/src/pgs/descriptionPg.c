// Copyright 15-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pgs/descriptionPg.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "dmc/err.h"
#include "dmc/js.h"
#include "data/models.h"

char *descriptionPg_process (Mchar *mrq) {
  char *rq = cgi_rq_string(mrq, "rq");
  if (str_eq(rq, "idata")) {
    char *model_id = cgi_rq_string(mrq, "modelId");
    Model *md = models_get(model_id);
    Mchar *rp = mchar_new();
    mchar_put(rp, "model", model_to_js(md));
    return cgi_rp(rp);
  } else return FAIL(str_f("Unexpected value for 'rq': %s", rq));
}
