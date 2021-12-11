// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db.h"
#include "dmc/path.h"
#include "dmc/file.h"
#include "db/conftb.h"
#include "db/dpaths.h"

void db_init (char *home) {
  char *dir = path_cat(home, "data", NULL);
  if (!file_exists(dir)) {
    file_mkdir(dir);
  }

  conftb_init(dir);
  dpaths_init(dir);
}
