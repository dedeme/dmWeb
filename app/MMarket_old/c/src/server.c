// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server.h"
#include "data/activity.h"
#include "db/log.h"
#include "server/hub.h"
#include "DEFS.h"

static int is_local (IserverRq *rq, char *msg, char *test) {
  return str_eq(msg, test) && str_eq(iserverRq_host(rq), "127.0.0.1");
}

// ac_rq is Tp[AsyncActor, IServerRq]
static void request (Tp *ac_rq) {
  AsyncActor *ac = tp_e1(ac_rq);
  IserverRq *rq = tp_e2(ac_rq);
  TRY
    char *msg = hub_rp(ac, opt_get(iserverRq_msg(rq)));
    char *e = iserverRq_write(rq, msg);
    if (*e) THROW(exc_io_t) e _THROW
  CATCH (ex)
    log_exception(ex);
    iserverRq_write(rq, str_f(
      "%s\n  %s", exc_msg(ex), str_join(exc_stack(ex), "\n  ")
    ));
  _TRY
}

void server_run (AsyncActor *ac, Iserver *server) {
  int error_counter = 0;
  while (activity_active()) {
    IserverRq *rq = iserver_read(server);

    if (*iserverRq_error(rq)) {
      TRY
        EXC_IO(iserverRq_error(rq))
      CATCH(ex)
        if (error_counter == 5) {
          void fn () { log_info(exc_msg(ex)); }
          asyncActor_wait(ac, fn);
        }
        ++error_counter;
      _TRY
    } else {
      if (opt_is_full(iserverRq_msg(rq))){
        char *msg = opt_get(iserverRq_msg(rq));

        if (is_local(rq, msg, "end")) {
          activity_off();
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
      sys_sleep(SERVER_ERROR_SLEEP);
      error_counter = 0;
    }

    sys_sleep(SERVER_SLEEP);
  }
}
