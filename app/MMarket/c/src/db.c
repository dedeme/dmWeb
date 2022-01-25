// Copyright 15-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db.h"
#include "dmc/file.h"
#include "data/cts.h"
#include "db/quotesTb.h"
#include "db/evalsDb.h"

void db_init (void) {
  char *dir = cts_data_dir();
  if (!file_exists(dir)) {
    file_mkdir(dir);
  }

  quotesTb_init(dir);
  evalsDb_init(dir);
}
