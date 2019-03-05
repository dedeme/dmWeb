// Copyright 26-Feb-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io.h"

static char *fpath = "/home/deme/.dmCApp/fleas";

Js *io_fmodels_new(void) {
  // Arr[char]
  Arr *fs = file_dir_new(fpath);
  // Arr [Js]
  Arr *jss = arr_new(free);
  EACH(fs, char, f)
    if (!str_eq(f, "_bests") && !str_eq(f, "_charts")) {
      arr_push(jss, js_ws_new(f));
    }
  _EACH

  Js *r = js_wa_new(jss);

  arr_free(fs);
  arr_free(jss);
  return r;
}

Js *io_params_new(char *model) {
  char *path = str_f_new("%s/%s/conf.db", fpath, model);
  Js *r = (Js *)file_read_new(path);
  free(path);
  return r;
}

Js *io_bests_new(char *model) {
  char *path = str_f_new("%s/_bests/%s.db", fpath, model);
  if (!file_exists(path)) {
    return (Js *) str_new("[]");
  }
  Js *r = (Js *)file_read_new(path);
  free(path);
  return r;
}

Js *io_dates_new(char *model) {
  char *path = str_f_new("%s/%s", fpath, model);
  // Arr[char]
  Arr *dir = file_dir_new(path);
  int ffilter(char *f) {
    return !str_eq(f, "conf.db");
  }
  arr_filter(dir, (FPRED)ffilter);
  Js *r = arr_to_js_new(dir, (FTO)js_ws_new);
  free(path);
  arr_free(dir);
  return r;
}

Js *io_results_new(char *model, char *date) {
  char *path = str_f_new("%s/%s/%s", fpath, model, date);
  if (!file_exists(path)) {
    return (Js *) str_new("[]");
  }
  Js *r = (Js *)file_read_new(path);
  free(path);
  return r;
}

Js *io_charts_list_new(char *model) {
  char *path = str_f_new("%s/_charts/%s.db", fpath, model);
  if (!file_exists(path)) {
    return (Js *) str_new("[]");
  }
  Js *data = (Js *)file_read_new(path);
  // Arr[Js]
  Arr *data_a = js_ra_new(data);
  // Arr[Js]
  Arr *rjs = arr_new(free);
  EACH(data_a, Js, js)
    // Arr[Js]
    Arr *ajs = js_ra_new(js);
    arr_push(rjs, (Js*)str_new((char *)arr_get(ajs, 0)));
    arr_free(ajs);
  _EACH

  Js *r = js_wa_new(rjs);

  arr_free(rjs);
  arr_free(data_a);
  free(data);
  free(path);

  return r;
}

Js *io_charts_data_new(char *model, char *nick) {
  char *path = str_f_new("%s/_charts/%s.db", fpath, model);
  if (!file_exists(path)) {
    return (Js *) str_new("[]");
  }
  Js *data = (Js *)file_read_new(path);
  // Arr[Js]
  Arr *data_a = js_ra_new(data);

  Js *r = NULL;
  EACH(data_a, Js, js)
    // Arr[Js]
    Arr *ajs = js_ra_new(js);
    char *nk = js_rs_new(arr_get(ajs, 0));
    if (str_eq(nick, nk)) {
      r = (Js *)str_new((char *)js);
      arr_free(ajs);
      free(nk);
      break;
    }
    free(nk);
    arr_free(ajs);
  _EACH

  if (!r) {
    r = (Js *) str_new("[]");
  }

  free(path);
  free(data);
  arr_free(data_a);

  return r;
}
