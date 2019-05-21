// Copyright 04-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server.h"
#include "io.h"
#include "io/log.h"
#include "DEFS.h"
#include "server/hub.h"

static int is_local (IserverRq *rq, char *msg, char *test) {
  return str_eq(msg, test) && str_eq(iserverRq_host(rq), "127.0.0.1");
}

static void request (IserverRq *rq) {
  TRY
    char *e = iserverRq_write(rq, hub_rp(opt_get(iserverRq_msg(rq))));
    if (*e) {
      THROW(exc_io_t) e _THROW
    }
  CATCH (ex)
    char *msg = exc_msg(ex);
    // Arr[char]
    Arr *stack = exc_stack(ex);

    log_exception(msg, stack);
    iserverRq_write(rq, str_f("%s\n  %s", msg, str_join(stack, "\n  ")));
  _TRY
}

void server_run(Iserver *server) {
  while (io_active()) {
    IserverRq *rq = iserver_read(server);

    if (*iserverRq_error(rq)) {
      TRY
        EXC_IO(iserverRq_error(rq))
      CATCH(ex)
        char *msg = exc_msg(ex);
        // Arr[char]
        Arr *stack = exc_stack(ex);

        log_exception(msg, stack);
        iserverRq_write(rq, str_f("%s\n  %s", msg, str_join(stack, "\n  ")));
      _TRY
    } else {
      if (opt_is_full(iserverRq_msg(rq))){
        char *msg = opt_get(iserverRq_msg(rq));

        if (is_local(rq, msg, "end")) {
          io_set_active(0);
        } else if (is_local(rq, msg, "test")) {
          char *e = iserverRq_write(rq, str_f("ok", msg));
          if (*e) {
            TRY
              EXC_IO(e)
            CATCH (ex)
              char *msg = exc_msg(ex);
              // Arr[char]
              Arr *stack = exc_stack(ex);

              log_exception(msg, stack);
              iserverRq_write(rq, str_f(
                "%s\n  %s", msg, str_join(stack, "\n  ")
              ));
            _TRY
          }
        } else {
          async_thread((FPROC)request, rq);
        }
      }
    }

    sys_sleep(SERVER_SLEEP);
  }
}
