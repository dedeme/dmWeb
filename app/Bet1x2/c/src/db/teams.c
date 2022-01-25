// Copyright 24-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db/teams.h"
#include "dmc/path.h"
#include "dmc/file.h"
#include "dmc/js.h"
#include "dmc/str.h"
#include "dmc/char/Mchar.h"
#include "data/cts.h"

static char *dir = NULL;

static char *fpath (char *year) {
  return path_cat(dir, year, "teams.tb", NULL);
}

void teams_init (char *parent) {
  dir = parent;
  char *ydir = path_cat(dir, cts_year(), NULL);
  char *path = fpath(cts_year());
  if (!file_exists(path)) {
    file_mkdir(ydir);
    file_write(path, aKchar_to_js(mchar_to_array(cts_teams())));
  }
}

AKchar *teams_read (char *year) {
  char *path = fpath(year);
  return aKchar_from_js(file_read(path));
}

Achar *teams_years (void) {
  /**/int ffilter (char *e) { return str_eq(path_extension(e), ""); }
  return achar_filter_to(file_dir(dir), ffilter);
}
