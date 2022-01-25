// Copyright 24-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db/results.h"
#include "dmc/path.h"
#include "dmc/file.h"
#include "dmc/js.h"
#include "dmc/char/Mchar.h"
#include "data/cts.h"

static char *dir = NULL;

static char *fpath (char *year) {
  return path_cat(dir, year, "results.tb", NULL);
}

void results_init (char *parent) {
  dir = parent;
  char *ydir = path_cat(dir, cts_year(), NULL);
  char *path = fpath(cts_year());
  if (!file_exists(path)) {
    file_mkdir(ydir);
    results_write(aAOResult_new_nones());
  }
}

AAOResult *results_read (char *year) {
  char *path = fpath(year);
  return aAOResult_from_js(file_read(path));
}

void results_write (AAOResult *results) {
  char *path = fpath(cts_year());
  file_write(path, aAOResult_to_js(results));
}
