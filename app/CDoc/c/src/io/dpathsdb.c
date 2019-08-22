// Copyright 18-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/dpathsdb.h"
#include "dmc/cgi.h"

static char *dbpath (void) {
  return path_cat(cgi_home(), "dpaths.db", NULL);
}

// Arr[Dpath]
Arr *dpathsdb_load (void) {
  char *path = dbpath();
  if (file_exists(path)) {
    // Arr[Path]
    Arr *r = arr_from_js((Js *)file_read(path), (FFROM)dpath_from_js);
    EACH(r, Dpath, p) {
      dpath_set_valid(p, file_exists(dpath_path(p)));
      if (!dpath_valid(p)) dpath_set_show(p, 0);
    }_EACH
    return r;
  }
  return arr_new();
}

// paths is Arr[Dpath]
void dpathsdb_save (Arr *paths) {
  file_write(dbpath(), (char *)arr_to_js(paths, (FTO)dpath_to_js));
}

// Opt[Dpath]
Opt *dpathdb_get (char *id) {
  int ffind (Dpath *p) { return str_eq(dpath_id(p), id); }
  return it_find(it_from(dpathsdb_load()), (FPRED)ffind);
}
