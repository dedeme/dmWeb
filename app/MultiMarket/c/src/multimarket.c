// Copyright 03-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "multimarket.h"
#include "dmc/cgi.h"
#include "dmc/Iserver.h"
#include "io.h"
#include "DEFS.h"
#include "server.h"

#include "io/log.h"
#include "io/conf.h"
#include "io/backups.h"
#include "io/trash.h"

#include "io/nicks.h"
#include "io/quotes.h"
#include "io/servers.h"

static void help (void) {
  puts("Usage: MultiMarket [start | stop | test | help]");
}

static void init (void) {
  sys_init(APP_NAME);
  cgi_init(sys_home(), EXPIRATION);
  io_init();
  log_init();
  conf_init();
  backups_init();
  trash_init();
  //---------
  nicks_init();
  quotes_init();
  servers_init();
}

static char *send (char *msg) {
  // Opt[char]
  Opt *r = sys_cmd(str_f("printf \"%s\" | nc -q 15 localhost %d", msg, PORT));
  if (opt_is_empty(r))
    EXC_IO(str_f("Fail sending initial message to %s", PORT))
  return opt_get(r);
}

static int test (void) {
  return str_eq(send("test"), "ok");
}


int main (int argc, char *argv[]) {
  init();

  if (argc != 2) {
    help ();
  } else if (str_eq(argv[1], "start")) {
    Iserver *sv = iserver_new(PORT);

    pthread_t *server_th = async_thread((FPROC)server_run, sv);

    async_join(server_th);

    iserver_close(sv);
  } else if (str_eq(argv[1], "stop")) {
    send("end");
  } else if (str_eq(argv[1], "test")) {
    if (test()) {
      puts("MultiMarket is active");
    } else {
      puts("MultiMarket is stopped");
    }
  } else {
    help();
  }

  io_end();
}
