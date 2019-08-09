// Copyright 04-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server.h"
#include "dmc/date.h"
#include "dmc/ext.h"
#include "io/io.h"
#include "io/log.h"
#include "DEFS.h"
#include "server/hub.h"

static int is_local (IserverRq *rq, char *msg, char *test) {
  return str_eq(msg, test) && str_eq(iserverRq_host(rq), "127.0.0.1");
}

// ac_rq is Tp[AsyncActor, IServerRq]
static void request (Tp *ac_rq) {
  AsyncActor *ac = tp_e1(ac_rq);
  IserverRq *rq = tp_e2(ac_rq);
  TRY
    DateTm *now = date_tm_now();
    char *msg = hub_rp(ac, opt_get(iserverRq_msg(rq)));
    if (date_tm_df(date_tm_now(), now) < 14000) {
      char *e = iserverRq_write(rq, msg);
      if (*e) THROW(exc_io_t) e _THROW
    } else {
      log_error("Time out waiting for a server response");
    }
  CATCH (ex)
    log_exception(ex);
    iserverRq_write(rq, str_f(
      "%s\n  %s", exc_msg(ex), str_join(exc_stack(ex), "\n  ")
    ));
  _TRY
}

void server_run (AsyncActor *ac, Iserver *server) {
  int error_counter = 0;
  while (io_active()) {
    IserverRq *rq = iserver_read(server);

    if (*iserverRq_error(rq)) {
      TRY
        EXC_IO(iserverRq_error(rq))
      CATCH(ex)
        if (error_counter == 5) {
          void fn () { log_exception(ex); }
          asyncActor_wait(ac, fn);
        }
        ++error_counter;
      _TRY
    } else {
      if (opt_is_full(iserverRq_msg(rq))){
        char *msg = opt_get(iserverRq_msg(rq));

        if (is_local(rq, msg, "end")) {
          io_set_active(0);
          error_counter = 0;
        } else if (is_local(rq, msg, "test")) {
          char *e = iserverRq_write(rq, "ok");
          if (*e) {
            TRY
              EXC_IO(e)
            CATCH (ex)
              if (error_counter == 5) {
                void fn () { log_exception(ex); }
                asyncActor_wait(ac, fn);
              }
              ++error_counter;
            _TRY
          } else {
            error_counter = 0;
          }
        } else {
          async_thread_detached((FPROC)request, tp_new(ac, rq));
          error_counter = 0;
        }
      }
    }

    if (error_counter > 100) {
      void fn () {
        log_error(str_f(
          "server_run: More than 100 tries to read Iserver '%s'",
          iserverRq_host(rq)
        ));
      }
      asyncActor_wait(ac, fn);
      error_counter = 0;
    }

    sys_sleep(SERVER_SLEEP);
  }
}
