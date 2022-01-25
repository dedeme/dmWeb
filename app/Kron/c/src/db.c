// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db.h"
#include "dmc/path.h"
#include "dmc/file.h"
#include "data/cts.h"
#include "db/conftb.h"
#include "db/log.h"
#include "db/anns.h"
#include "db/exes.h"

void db_init (char *home) {
  char *dir = path_cat(home, cts_data_dir(), NULL);
  if (!file_exists(dir)) {
    file_mkdir(dir);
  }

  conftb_init(dir);
  log_init(dir);
  anns_init(dir);
  exes_init(dir);
}
