// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "mmarket.h"
#include "dmc/cgi.h"
#include "dmc/Iserver.h"
#include "db/init.h"
#include "DEFS.h"
#include "server.h"
#include "scheduler.h"

//#include "io/log.h"
//#include "io/conf.h"
//#include "io/backups.h"
//#include "io/trash.h"

//#include "io/nicks.h"
//#include "io/quotes.h"
//#include "io/servers.h"
//#include "io/calendar.h"
//#include "io/fleasdb.h"
//#include "io/accdb.h"
//#include "io/sbox.h"
//#include "io/dailydb.h"
//#include "io/rank.h"
//#include "io/managerdb.h"

//#include "data/dfleas/dfleas__models.h"

//#include "scheduler/fleas.h"

static void help (void) {
  puts("Usage: MultiMarket [start | stop | test | help]");
}

static void init (void) {
  sys_init(APP_NAME);
  cgi_init(sys_home(), EXPIRATION);
  initDb_run();
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

    AsyncActor *ac = asyncActor_new(ACTOR_SLEEP);
    void fn1 () { server_run(ac, sv); }
    pthread_t *server_th = async_thread(fn1);
    void fn2 () { scheduler_run(ac); }
    pthread_t *scheduler_th = async_thread(fn2);

    async_join(server_th);
    async_join(scheduler_th);
    asyncActor_end(ac);
    asyncActor_join(ac);

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
}
