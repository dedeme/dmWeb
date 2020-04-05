// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db/init.h"
#include "db/calendar.h"
#include "db/dailyqs.h"
#include "db/global.h"
#include "db/log.h"
#include "db/nicks.h"
#include "db/quotes.h"
#include "db/sbox.h"
#include "db/servers.h"
#include "db/fleas/flog.h"
#include "db/fleas/fmodels.h"
#include "db/fleas/ranking.h"
#include "DEFS.h"

static char *path () {
  return path_cat(sys_home(), DATA_PATH, NULL);
}

static char *fleas_path () {
  return path_cat(sys_home(), FLEAS_PATH, NULL);
}

static char *ranking_path () {
  return path_cat(sys_home(), RANKING_PATH, NULL);
}

static char *acc_path () {
  return path_cat(sys_home(), ACC_PATH, NULL);
}

void initDb_run (void) {
  if (!file_exists(path())) {
    file_mkdir(path());
  }
  if (!file_exists(fleas_path())) {
    file_mkdir(fleas_path());
  }
  if (!file_exists(ranking_path())) {
    file_mkdir(ranking_path());
  }
  if (!file_exists(acc_path())) {
    file_mkdir(acc_path());
  }

  calendar_init();
  dailyqs_init();
  global_init();
  log_init();
  nicks_init();
  quotes_init();
  sbox_init();
  servers_init();
  flog_init();
  fmodels_init();
  ranking_init();
}
