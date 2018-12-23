// Copyright 31-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "bests.h"
#include "dmc/cgi.h"
#include "DEFS.h"

static Js *fgroups_new() {
  char *dir = path_cat_new(FLEAS_DIR, "bests", NULL);
  // Arr[char]
  Arr *fs = file_dir_new(dir);
  arr_sort(fs, (FGREATER)str_greater);
  arr_reverse(fs);
  // Arr[Js]
  Arr *a = arr_new(free);
  EACH(fs, char, f)
    char *f2 = str_new(f);
    str_left(&f2, strlen(f2) - 3);
    arr_push(a, js_ws_new(f2));
    free(f2);
  _EACH
  Js *r = js_wa_new(a);
  free(dir);
  arr_free(fs);
  arr_free(a);
  return r;
}

void bests_process(Map *rqm) {
  CGI_GET_STR(rq, rqm, "rq")

  // ----------------------------------------------------------- dates
  if (str_eq(rq, "groups")) {
    // Map[Js]
    Map *m = map_new(free);
    map_put(m, "fgrs", fgroups_new());
    cgi_ok(m);
    map_free(m);

  // --------------------------------------------------------- results
  } else if (str_eq(rq, "results")) {
    CGI_GET_STR(fgroup, rqm, "fgroup")
    str_cat(&fgroup, ".db");
    char *path = path_cat_new(FLEAS_DIR, "bests", fgroup, NULL);
    // Map[Js]
    Map *m = map_new(free);
    map_put(m, "results", file_read_new(path));
    cgi_ok(m);
    free(fgroup);
    map_free(m);

  // ---------------------------------------------------------- error!
  } else FAIL(str_f_new("Unknown request '%s'", rq))

  free(rq);
}
