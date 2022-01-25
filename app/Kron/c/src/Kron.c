// Copyright 12-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "Kron.h"
#include "dmc/async.h"
#include <stdio.h>
#include <stdlib.h>
#include "dmc/sys.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "dmc/err.h"
#include "dmc/char/Ochar.h"
#include "data/cts.h"
#include "db.h"
#include "db/log.h"
#include "scheduler.h"
#include "server.h"

static char *help = "Usage: Kron [start | stop | test | help]";

void Kron_err (char *err, char **stack, int len) {
  for (int i = 0; i < len; i++) {
    err = str_f("%s\n  %s", *stack);
    ++stack;
  }
  err = str_f("%s\n", err);

  ALogEntry *log = log_read();
  aLogEntry_push(log, logEntry_new("F", err));
  log_write(log);

  exit(1);
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    puts(help);
    return(1);
  }
  char *arg = argv[1];

  sys_init(cts_app_name());
  err_fn(Kron_err);
  cgi_init(sys_home(), cts_expiration());
  db_init(sys_home());

  if (str_eq(arg, "start")) {
    char *rp = ochar_nsome(sys_cmd(str_cat(
      "printf \"test\" | nc -q 120 localhost ", cts_port(), " 2>&1", NULL
    )));
    if (rp && str_eq(rp, cts_server_ok_msg())) {
      puts("Kron already is running");
      return 0;
    }

    /**/void fn1 (void) { log_info("Kron started"); }
    async_run(fn1);

    pthread_t *sch = async_thread(scheduler_run);
    pthread_t *sv = async_thread(server_run);
    async_join(sv);
    async_join(sch);

    /**/void fn2 (void) { log_info("Kron stopped"); }
    async_run(fn2);
    return(0);
  } else if (str_eq(arg, "stop")) {
    if (!ochar_nsome(sys_cmd(str_cat(
      "printf \"", cts_server_stop_msg(), "\" | nc -q 120 localhost "
      , cts_port(), " 2>&1", NULL
    ))))
      FAIL ("Test command failed");
  } else if (str_eq(arg, "test")) {
    char *rp = ochar_nsome(sys_cmd(str_cat(
      "printf \"test\" | nc -q 120 localhost ", cts_port(), " 2>&1", NULL
    )));
    if (rp) {
      if (!str_eq(rp, cts_server_ok_msg())) {
        puts("Server is not running.");
      } else {
        puts("Server is running.");
      }
    } else {
      FAIL ("Test command failed");
    }
  } else if (str_eq(arg, "help")) {
    puts(help);
  } else {
    puts(help);
    return(1);
  }

  return(0);
}
