// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db/dpaths.h"
#include "dmc/path.h"
#include "dmc/file.h"
#include "dmc/Js.h"

static char *dpaths_path = NULL;

void dpaths_init (char *parent) {
  dpaths_path = path_cat(parent, "paths.tb", NULL);
  if (!file_exists(dpaths_path)) {
    dpaths_write(aDpath_new());
  }
}

ADpath *dpaths_read (void) {
  ADpath *a = (ADpath *)achar_map(
    js_ra(file_read(dpaths_path)),
    (void *(*)(char *))dpath_from_js
  );
  Dpath **p = a->es;
  while (p < a->end) {
    Dpath *dpath = *p++;
    dpath_set_valid(dpath, file_is_directory(dpath->path));
  }
  return a;
}

void dpaths_write (ADpath *paths) {
  file_write(dpaths_path, js_wa((Achar*)aDpath_map(
    paths,
    (void *(*)(Dpath *))dpath_to_js
  )));
}
