// Copyright 24-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db.h"
#include "dmc/path.h"
#include "dmc/file.h"
#include "dmc/err.h"
#include "dmc/str.h"
#include "data/cts.h"
#include "db/teams.h"
#include "db/results.h"
#include "db/points.h"
#include "db/bets.h"

void db_init (char *home) {
  char *dir = path_cat(home, cts_data_dir(), NULL);
  char *fversion = path_cat(dir, "version.txt", NULL);
  if (!file_exists(dir)) {
    file_mkdir(dir);
    file_write(fversion, cts_data_version());
  }

  if (!str_eq(file_read(fversion), cts_data_version())) {
    FAIL(str_f(
      "Expected data version '%s'\n found '%s'.",
      cts_data_version(), file_read(fversion)
    ));
  }

  teams_init(dir);
  results_init(dir);
  points_init(dir);
  bets_init(dir);
}
