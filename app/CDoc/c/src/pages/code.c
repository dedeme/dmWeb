// Copyright 22-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pages/code.h"
#include "dmc/cgi.h"
#include "io/dpathsdb.h"
#include "io/io.h"

// mrq is Map[Js]
char *code_process (Map *mrq) {
  CGI_GET_STR(type, mrq)
  CGI_GET_STR(id, mrq)
  CGI_GET_STR(path, mrq)

  // Map[Js]
  Map *rp = map_new();
  map_put(rp, "code", js_ws(""));
  Dpath *apath = opt_nget(dpathdb_get(id));
  if (apath) {
    char *code = io_code(
      str_eq(type, "h")
       ? path_cat(dpath_path(apath), "include", str_f("%s.h", path), NULL)
       : path_cat(dpath_path(apath), "src", str_f("%s.c", path), NULL)
    );
    map_put(rp, "code", js_ws(code));
  }
  return cgi_ok(rp);
}

