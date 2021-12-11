// Copyright 11-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pgs/codePg.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "dmc/Js.h"
#include "dmc/path.h"
#include "dmc/file.h"
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "db/dpaths.h"
#include "data/Dpath/ADpath.h"

char *codePg_process(Mchar *mrq) {
  char *rq = cgi_rq_string(mrq, "rq");
  if (str_eq(rq, "code")) {
    char *module = cgi_rq_string(mrq, "module");
    char *path = cgi_rq_string(mrq, "path");
    int is_header = cgi_rq_bool(mrq, "isHeader");

    char *code = NULL;
    /**/int find_module(Dpath *p) { return str_eq(p->id, module); }
    Dpath *lb = oDpath_nsome(aDpath_find(dpaths_read(), find_module));
    if (lb) {
      char *spath = is_header
        ? path_cat(lb->path, "include", str_f("%s.h", path), NULL)
        : path_cat(lb->path, "src", str_f("%s.c", path), NULL);
      if (file_exists(spath)) {
        code = file_read(spath);
      }
    }

    Mchar *rp = mchar_new();
    mchar_put(rp, "code", code ? js_ws(code) : js_wn());
    return cgi_rp(rp);
  } else {
    return FAIL(str_f("Unexpected value for 'rq': %s", rq));
  }
}
