// Copyright 19-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pages/paths.h"
#include "dmc/cgi.h"
#include "io/confdb.h"
#include "io/dpathsdb.h"

// mrq is Map[Js]
char *paths_process (Map *mrq) {
  CGI_GET_STR(rq, mrq)

  if (str_eq(rq, "new")) {
    CGI_GET_STR(id, mrq)
    CGI_GET_STR(path, mrq)
    /// Arr[Dpath]
    Arr *paths = dpathsdb_load();
    arr_push(paths, dpath_new(id, path, 1, 1));
    dpathsdb_save(paths);
    return cgi_empty();
  }

  if (str_eq(rq, "del")) {
    CGI_GET_STR(id, mrq)
    /// Arr[Dpath]
    Arr *paths = dpathsdb_load();
    int ffilter (Dpath *p) { return !str_eq(dpath_id(p), id); }
    dpathsdb_save(arr_filter_to(paths, (FPRED)ffilter));
    return cgi_empty();
  }

  if (str_eq(rq, "showPath")) {
    CGI_GET_STR(id, mrq)
    /// Arr[Dpath]
    Arr *paths = dpathsdb_load();
    EACH(paths, Dpath, p) {
      if (str_eq(dpath_id(p), id))
        dpath_set_show(p, !dpath_show(p));
    }_EACH
    dpathsdb_save(paths);
    return cgi_empty();
  }

  if (str_eq(rq, "modify")) {
    CGI_GET_STR(id, mrq)
    CGI_GET_STR(newId, mrq)
    CGI_GET_STR(dpath, mrq)
    /// Arr[Dpath]
    Arr *paths = dpathsdb_load();
    int i = -1;
    Dpath *np = NULL;
    EACH_IX(paths, Dpath, p, ix) {
      if (str_eq(dpath_id(p), id)) {
        i = ix;
        np = dpath_new(newId, dpath, dpath_show(p), 1);
        break;
      }
    }_EACH

    if (np) {
      arr_remove(paths, i);
      arr_push(paths, np);
      dpathsdb_save(paths);
    }

    return cgi_empty();
  }

  if (str_eq(rq, "showAll")) {
    Conf *cf = confdb_load();
    confdb_save(conf_new(
      conf_path(cf), conf_lang(cf), conf_show_all(cf) ? 0 : 1
    ));
    return cgi_empty();
  }

  if (str_eq(rq, "lang")) {
    Conf *cf = confdb_load();
    confdb_save(conf_new(
      conf_path(cf),
      str_eq(conf_lang(cf), "en") ? "es" : "en",
      conf_show_all(cf)
    ));
    return cgi_empty();
  }

  EXC_ILLEGAL_ARGUMENT(
    "rq", "new | del | showPath | modify | showAll | lang", rq
  )
  return NULL;  // Unreachable

}

