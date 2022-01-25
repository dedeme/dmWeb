// Copyright 24-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db/bets.h"
#include "dmc/path.h"
#include "dmc/file.h"
#include "dmc/js.h"
#include "dmc/char/Mchar.h"
#include "data/cts.h"

static char *dir = NULL;

static char *fpath (char *year) {
  return path_cat(dir, year, "bets.tb", NULL);
}

void bets_init (char *parent) {
  dir = parent;
  char *ydir = path_cat(dir, cts_year(), NULL);
  char *path = fpath(cts_year());
  if (!file_exists(path)) {
    file_mkdir(ydir);
    bets_write(aAOBet_new_nones());
  }
}

AAOBet *bets_read (char *year) {
  char *path = fpath(year);
  return aAOBet_from_js(file_read(path));
}

void bets_write (AAOBet *bets) {
  char *path = fpath(cts_year());
  file_write(path, aAOBet_to_js(bets));
}
