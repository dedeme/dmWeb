// Copyright 20-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pages/module.h"
#include "dmc/cgi.h"
#include "io/dpathsdb.h"
#include "io/io.h"

// mrq is Map[Js]
char *module_process (Map *mrq) {
  CGI_GET_STR(id, mrq)
  CGI_GET_STR(path, mrq)

  // Map[Js]
  Map *rp = map_new();
  map_put(rp, "doc", js_wn());
  Dpath *apath = opt_nget(dpathdb_get(id));
  if (apath) {
    Doc *doc = opt_nget(io_module(
      path_cat(dpath_path(apath), "include", str_f("%s.h", path), NULL)
    ));
    if (doc)
      map_put(rp, "doc", doc_to_js(doc));
  }
  return cgi_ok(rp);
}
