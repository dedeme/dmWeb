// Copyright 19-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pgs/cosPg.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "dmc/err.h"
#include "dmc/js.h"
#include "data/models.h"
#include "db/evalsDb.h"
#include "db/quotesTb.h"

char *cosPg_process (Mchar *mrq) {
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
      /**/  return e1->hvalue < e2->hvalue;
      /**/}
      aModelEval_sort(evs, fgreater);
      eval = *(evs->es);
      params = eval->params;
    }

    Quotes *qs = quotesTb_read();
    Result *result = model_result(md, qs, params);

    Mchar *rp = mchar_new();
    mchar_put(rp, "model", model_to_js(md));
    mchar_put(rp, "result", result_to_js(result));
    mchar_put(rp, "eval", modelEval_to_js(eval));
    mchar_put(rp, "cos", achar_to_js(qs->cos));
    return cgi_rp(rp);
  } else if (str_eq(rq, "co")) {
    char *model_id = cgi_rq_string(mrq, "modelId");
    ADouble *params = aDouble_from_js(ochar_some(mchar_get(mrq, "params")));
    char *co = cgi_rq_string(mrq, "co");
    Model *md = models_get(model_id);
    Quotes *all_qs = quotesTb_read();
    int ico = quotes_ico(all_qs, co);
    if (ico == -1) FAIL (str_f("Company %s not found", co));
    ADouble *qs = quotes_closes(all_qs, ico);
    Quotes *co_qs = quotes_mk_single(all_qs, ico);
    Result *result = model_result(md, co_qs, params);
    ADouble *refs = model_refs(md, co_qs, params);

    Mchar *rp = mchar_new();
    mchar_put(rp, "result", result_to_js(result));
    mchar_put(rp, "dates", achar_to_js(all_qs->dates));
    mchar_put(rp, "qs", aDouble_to_js(qs));
    mchar_put(rp, "refs", aDouble_to_js(refs));
    return cgi_rp(rp);

  } else return FAIL(str_f("Unexpected value for 'rq': %s", rq));
}
