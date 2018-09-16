// Copyright 10-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/db.h"

char *dir = NULL;

static char *quotes_dir() {
  return path_cat(db_dir(), "quotes", NULL);
}

void db_create(char *path) {
  dir = path;
  file_mkdir(quotes_dir());
}

void db_init(char *path) {
  dir = path;
}

/// Return the directory 'data'
char *db_dir(void) {
  if (!dir) {
    exc_illegal_state("db_dir has not been intialized");
  }
  return dir;
}
