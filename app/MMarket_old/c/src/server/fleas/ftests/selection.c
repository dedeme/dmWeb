// Copyright 19-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/fleas/ftests/selection.h"
#include "dmc/cgi.h"
#include "data/flea/fmodels.h"
#include "data/flea/Fmodel.h"
#include "db/fleas/flog.h"
#include "db/quotes.h"
#include "scheduler/fleas.h"
#include "DEFS.h"

struct Selection_Params {
  AsyncActor *ac;
  Fmodel *model;
  Opt *log_id; // Opt[char]
};
typedef struct Selection_Params Params;
static Params *params_new (
  AsyncActor *ac,
  Fmodel *model,
  Opt *log_id // Opt[char]
) {
  Params *this = MALLOC(Params);
  this->ac = ac;
  this->model = model;
  this->log_id = log_id;
  return this;
}

static void selection (Params *ps) {
  Qtable *opens = NULL;
  Qtable *closes = NULL;
  void fn1 () {
    opens = opt_nget(quotes_opens());
    closes = opt_nget(quotes_closes());
  }
  asyncActor_wait(ps->ac, fn1);

  fleas_update(ps->ac, opens, closes, ps->model, ps->log_id);
}

char *selection_process (AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq);

  if (str_eq(rq, "logId")) {
    // Map[Js]
    Map *rp = map_new();
    void fn () { map_put(rp, "logId", js_ws(flog_new())); }
    asyncActor_wait(ac, fn);

    return cgi_ok(rp);
  }
  if (str_eq(rq, "start")) {
    CGI_GET_STR(modelId, mrq);
    CGI_GET_STR(logId, mrq);

    // Map[Js]
    Map *rp = map_new();

    Fmodel *model = opt_nget(fmodels_get(modelId));
    if (model) {
      async_thread_detached(
        (FPROC)selection,
        params_new(ac, model, opt_new(logId))
      );
      flog_info(logId, "Starting...");
      map_put(rp, "error", js_ws(""));
      return cgi_ok(rp);
    }

    CGI_GET_STR(error1, mrq)
    map_put(rp, "error", js_ws(error1));
    return cgi_ok(rp);
  }
  if (str_eq(rq, "continue")) {
    CGI_GET_STR(logId, mrq);

    // Map[Js]
    Map *rp = map_new();
    map_put(rp, "log", flog_read(logId));
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "logId", rq)
  return NULL; // Unreachable
}



