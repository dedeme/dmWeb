// Copyright 18-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pages/main.h"
#include "dmc/cgi.h"
#include "io/confdb.h"
#include "io/dpathsdb.h"

// mrq is Map[Js]
char *main_process (Map *mrq) {
  CGI_GET_STR(rq, mrq)

  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "idata")) {
    map_put(rp, "conf", conf_to_js(confdb_load()));
    map_put(rp, "paths", arr_to_js(dpathsdb_load(), (FTO)dpath_to_js));
    return cgi_ok(rp);
  }

  if (str_eq(rq, "setPath")) {
    CGI_GET_STR(option, mrq)
    Conf *cf = confdb_load();
    confdb_save(conf_new(option, conf_lang(cf), conf_show_all(cf)));
    return cgi_empty();
  }

  EXC_ILLEGAL_ARGUMENT("rq", "idata | setPath", rq)
  return NULL;  // Unreachable

}
