// Copyright 24-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "Bet1x2.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pwd.h>
#include "dmc/sys.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "dmc/err.h"
#include "dmc/path.h"
#include "dmc/char/Ochar.h"
#include "data/cts.h"
#include "db.h"
#include "update.h"
#include "tests.h"
#include "web.h"

static char *help = "Usage: FbBet [update | test | help | <request>]";

void FbBet_err (char *err, char **stack, int len) {
  for (int i = 0; i < len; i++) {
    err = str_f("%s\n  %s", err, *stack);
    ++stack;
  }

  puts(err);
  exit(1);
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    puts(help);
    return(1);
  }
  char *arg = argv[1];

  err_fn(FbBet_err);

  uid_t uid = getuid();
  struct passwd *udata = getpwuid(uid);
  char *uname = str_new(udata->pw_name);
  if (str_eq(uname, "www-data")) {
    cgi_init(cts_web_app_dir(), cts_expiration());
    db_init(path_cat("/home/deme/.dmCApp", cts_app_name(), NULL));
    printf("%s", web_process(arg));
  } else {
    sys_init(cts_app_name());
    db_init(sys_home());

    if (str_eq(arg, "update")) {
      update_run();
    } else if (str_eq(arg, "test")) {
      tests_run();
    } else if (str_eq(arg, "help")) {
      puts(help);
    }  else {
      FAIL(help);
    }
  }

  return(0);
}
