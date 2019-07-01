// Copyright 01-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys/sys__annotations.h"
#include "dmc/cgi.h"
#include "io.h"
#include "io/conf.h"
#include "io/accdb.h"
#include "io/log.h"
#include "data/Acc.h"

// mrq is Map[Js]
char *sys__annotations_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq, "rq")
  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "idata")) {
    void fn (void *null) {
      // Arr[AccEntry]
      Arr *annotations = accdb_diary_read();
      AccLedPf *rs = accLedPf_new(annotations);
      EACH(accLedPf_errors(rs), char, e)
        log_error(e);
      _EACH
      map_put(rp, "errors", js_wi(arr_size(accLedPf_errors(rs))));
      map_put(rp, "cash", js_wd(accLedger_cash(accLedPf_ledger(rs))));
      map_put(rp, "annotations", accdb_diary_read_js());
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_ok(rp);
  }
  if (str_eq(rq, "new")) {
    CGI_GET(AccEntry *, ann, accEntry_from_js, mrq, "ann")
    void fn (void *null) {
      accdb_diary_add(ann);
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_empty();
  }
  if (str_eq(rq, "remove")) {
    CGI_GET_INT(id, mrq, "id");
    CGI_GET_STR(date, mrq, "date");
    char *year = str_left(date, 4);
    void fn (void *null) {
      accdb_diary_remove(year, id);
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_empty();
  }

  EXC_ILLEGAL_ARGUMENT("rq", "idata | new | remove", rq)
  return NULL; // Unreachable
}



