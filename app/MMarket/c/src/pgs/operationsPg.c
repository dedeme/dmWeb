// Copyright 21-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pgs/operationsPg.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "dmc/err.h"
#include "dmc/js.h"
#include "data/models.h"
#include "db/evalsDb.h"
#include "db/quotesTb.h"

char *operationsPg_process (Mchar *mrq) {
  char *rq = cgi_rq_string(mrq, "rq");
  if (str_eq(rq, "idata")) {
    char *model_id = cgi_rq_string(mrq, "modelId");
    char *params_js = ochar_some(mchar_get(mrq, "params"));

    Model *md = models_get(model_id);
    AModelEval *evs = evalsDb_read(model_id)->evals;

    ADouble *params = NULL;
    ModelEval *eval = NULL;
    if (!js_is_null(params_js)) {
      params = aDouble_from_js(params_js);
      /**/int ffind (ModelEval *e) {
      /**/  return aDouble_eq(params, e->params, 0.000001);
      /**/}
      eval = oModelEval_nsome(aModelEval_find(evs, ffind));
      if (!eval) params_js = js_wn(); // Enter in next 'if'
    }
    if (js_is_null(params_js)) { // Warning: Not use 'else'
      /**/int fgreater (ModelEval *e1, ModelEval *e2) {
      /**/  return e1->value < e2->value;
      /**/}
      aModelEval_sort(evs, fgreater);
      eval = *(evs->es);
      params = eval->params;
    }

    Quotes *qs = quotesTb_read();
    Result *result = model_result(md, qs, params);
    Achar *nicks = qs->cos;
    ADouble *last_closes = aADouble_get(
      qs->closes, aADouble_size(qs->closes) - 1
    );


    Mchar *rp = mchar_new();
    mchar_put(rp, "model", model_to_js(md));
    mchar_put(rp, "result", result_to_js(result));
    mchar_put(rp, "eval", modelEval_to_js(eval));
    mchar_put(rp, "nicks", achar_to_js(nicks));
    mchar_put(rp, "lastCloses", aDouble_to_js(last_closes));
    mchar_put(rp, "orders", aOrder_to_js(model_orders(md, qs, params)));
    return cgi_rp(rp);
  } else return FAIL(str_f("Unexpected value for 'rq': %s", rq));
}
