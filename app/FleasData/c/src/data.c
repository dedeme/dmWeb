// Copyright 31-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data.h"
#include "dmc/cgi.h"
#include "DEFS.h"

static Js *dates_new(char *fgroup) {
  char *dir = path_cat_new(FLEAS_DIR, fgroup, NULL);
  // Arr[char]
  Arr *fs = file_dir_new(dir);
  arr_sort(fs, (FGREATER)str_greater);
  arr_reverse(fs);
  // Arr[Js]
  Arr *a = arr_new(free);
  EACH(fs, char, f)
    arr_push(a, js_ws_new(f));
  _EACH
  Js *r = js_wa_new(a);
  free(dir);
  arr_free(fs);
  arr_free(a);
  return r;
}

static Js *results_new(char *fgroup, char *date, int chunk) {
  chunk *= 200;
  char *f = path_cat_new(FLEAS_DIR, fgroup, date, NULL);
  // Arr[Js]
  Varr *achunk = varr_new();
  Js *results = (Js *)file_read_new(f);
  // Arr[Js]
  Arr *aresults = js_ra_new(results);
  int end = arr_size(aresults);
  end = end > chunk + 200 ? chunk + 200 : end;
  RANGE(i, chunk, end)
    varr_push(achunk, arr_get(aresults, i));
  _RANGE
  Js *r = js_wa_new((Arr *)achunk);
  free(f);
  varr_free(achunk);
  free(results);
  arr_free(aresults);
  return r;
}

void data_process(Map *rqm) {
  CGI_GET_STR(rq, rqm, "rq")

  // ----------------------------------------------------------- dates
  if (str_eq(rq, "dates")) {
    CGI_GET_STR(fgroup, rqm, "fgroup")
    // Map[Js]
    Map *m = map_new(free);
    map_put(m, "dates", dates_new(fgroup));
    cgi_ok(m);
    free(fgroup);
    map_free(m);

  // ----------------------------------------------------------- dates
  } else if (str_eq(rq, "results")) {
    CGI_GET_STR(fgroup, rqm, "fgroup")
    CGI_GET_STR(date, rqm, "date")
    CGI_GET_INT(chunk, rqm, "chunk")
    // Map[Js]
    Map *m = map_new(free);
    map_put(m, "results", results_new(fgroup, date, chunk));
    cgi_ok(m);
    free(fgroup);
    free(date);
    map_free(m);

  // ---------------------------------------------------------- error!
  } else FAIL(str_f_new("Unknown request '%s'", rq))

  free(rq);
}
