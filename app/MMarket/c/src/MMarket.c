// Copyright 15-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "MMarket.h"
#include <stdio.h>
#include <unistd.h>
#include <pwd.h>
#include "dmc/str.h"
#include "dmc/path.h"
#include "dmc/sys.h"
#include "dmc/cgi.h"
#include "db.h"
#include "pgs.h"
#include "update.h"
#include "data/cts.h"

static char *help = "Usage: MMarket [update | test | help | <request>]";

static void test (void) {/*
  #include "data/Model.h"
  #include "data/models.h"
  #include "external/quotesReader.h"
  AModelEval *evs = model_range_new_evaluation(models_get("GA"), quotesReader_read(),
    aModelEval_new());
  ModelEval **pevs = evs->es;
  while (pevs < evs->es) {
    ModelEval *ev = *pevs++;
    puts(modelEval_to_js(ev));
  }
*/}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    puts(help);
    return(1);
  }
  char *arg = argv[1];

  db_init();

  uid_t uid = getuid();
  struct passwd *udata = getpwuid(uid);
  if (str_eq(udata->pw_name, "deme")) {
    sys_init(cts_app_name());
    if (str_eq(arg, "update")) update_run();
    else if (str_eq(arg, "test")) test();
    else puts(help);
  } else {
    cgi_init(path_cat("dmcgi", cts_app_name(), NULL), cts_expiration());
    puts(pgs_process(arg));
  }

  return(0);
}
