// Copyright 12-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys/sys__models.h"
#include "dmc/cgi.h"
#include "data/dfleas/dfleas__models.h"
#include "data/Manager.h"
#include "io/managerdb.h"
#include "io/nicks.h"
#include "scheduler/management.h"

// mrq is Map[Js]
char *sys__models_process(AsyncActor *ac, Map *mrq) {
  // Map[Js]
  Map *rp = map_new();
  CGI_GET_STR(rq, mrq, "rq")

  if (str_eq(rq, "idata")) {
    void fn (void *null) {
      // Map[ManagerEntry2]
      Map *es = map_new();
      EACH(nicks_list(), Nick, nk)
        if (nick_is_sel(nk)) {
          char *nick = nick_name(nk);
          ModelParams *mps = managerdb_nick(nick);
          Model *md = modelParams_model(mps);
          map_put(es, nick, managerEntry2_new(
            model_name(md), modelParams_params(mps), model_param_cf(md),
            arr_from_js(model_param_jss(md), (FFROM) managerFormat_from_js)
          ));
        }
      _EACH

      ModelParams *mps = managerdb_default();
      Model *md = modelParams_model(mps);
      map_put(rp, "manager", manager2_to_js(manager2_new(
        managerEntry2_new(
          model_name(md), modelParams_params(mps), model_param_cf(md),
          arr_from_js(model_param_jss(md), (FFROM) managerFormat_from_js)
        ),
        es
      )));

      // Arr[ManagerEntry2]
      Arr *models = arr_new();
      EACH(dfleas__models(), Model, md)
        Darr *ps = darr_new();
        // Arr[ModelMxMn]
        Arr *mmm  = model_param_cf(md);
        EACH(mmm, ModelMxMn, m)
          darr_push(ps, (modelMxMn_max(m) + modelMxMn_min(m)) / 2);
        _EACH
        arr_push(models, managerEntry2_new(
          model_name(md), ps, mmm,
          arr_from_js(model_param_jss(md), (FFROM) managerFormat_from_js)
        ));
      _EACH
      map_put(rp, "models", arr_to_js(models, (FTO)managerEntry2_to_js));
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "update")) {
    CGI_GET_STR(nick, mrq, "nick");
    CGI_GET_STR(model, mrq, "model");
    CGI_GET(Darr *, params, darr_from_js, mrq, "params");
    void fn (void *null) {
      map_put(rp, "ok", js_wb(!managerdb_set_nick (nick, model, params)));
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "regularize")) {
    management_update(ac);
    return cgi_empty();
  }

  EXC_ILLEGAL_ARGUMENT(
    "rq",
    "idata | update | regularize",
    rq
  )
  return NULL; // Unreachable
}
