// Copyright 27-Feb-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pages/results.h"
#include "dmc/cgi.h"
#include "io.h"

void results_process(Map *mrq) {
  CGI_GET_STR(rq, mrq, "rq")

  if (str_eq(rq, "getDates")) {
    CGI_GET_STR(fmodel, mrq, "fmodel")
    Map *m = map_new(free);
    map_put(m, "dates", io_dates_new(fmodel));
    cgi_ok(m);
    map_free(m);
    free(fmodel);
  } else if (str_eq(rq, "data")) {
    CGI_GET_STR(fmodel, mrq, "fmodel")
    CGI_GET_STR(date, mrq, "date")
    Map *m = map_new(free);
    map_put(m, "params", io_params_new(fmodel));
    map_put(m, "table", io_results_new(fmodel, date));
    cgi_ok(m);
    free(fmodel);
    free(date);
    map_free(m);
  } else {
    FAIL(str_f_new("'%s': Unknown 'rq' in settings", rq))
  }

  free(rq);
}
