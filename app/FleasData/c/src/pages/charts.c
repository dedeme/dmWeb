// Copyright 28-Feb-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pages/charts.h"
#include "dmc/cgi.h"
#include "conf.h"
#include "io.h"

void charts_process(Map *mrq) {
  CGI_GET_STR(rq, mrq, "rq")

  if (str_eq(rq, "getModel")) {
    Map *m = map_new(free);
    map_put(m, "model", conf_get_charts_model_new());
    cgi_ok(m);
    map_free(m);
  } else if (str_eq(rq, "setModel")) {
    CGI_GET_STR(model, mrq, "model")
    conf_set_charts_model(model);
    cgi_empty();
    free(model);
  } else if (str_eq(rq, "list")) {
    CGI_GET_STR(fmodel, mrq, "fmodel")
    Map *m = map_new(free);
    map_put(m, "list", io_charts_list_new(fmodel));
    cgi_ok(m);
    map_free(m);
    free(fmodel);
  } else if (str_eq(rq, "data")) {
    CGI_GET_STR(fmodel, mrq, "fmodel")
    CGI_GET_STR(nick, mrq, "nick")
    Map *m = map_new(free);
    map_put(m, "data", io_charts_data_new(fmodel, nick));
    cgi_ok(m);
    free(fmodel);
    free(nick);
    map_free(m);
  } else {
    FAIL(str_f_new("'%s': Unknown 'rq' in settings", rq))
  }

  free(rq);
}

