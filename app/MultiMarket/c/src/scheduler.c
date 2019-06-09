// Copyright 04-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "scheduler.h"
#include "dmc/date.h"
#include "io.h"
#include "io/conf.h"
#include "DEFS.h"
#include "scheduler/historic.h"
#include "scheduler/fleas.h"
#include "scheduler/acc.h"

static void sleeping1 (AsyncActor *ac) {
  int hour = atoi(date_f(date_now(), "%H"));
  if (hour > 4) {
    void fn (void *null) {
      conf_set_fleas_running(0);
      conf_set_fleas_finished(0);
    }
    asyncActor_wait(ac, fn, NULL);

    if (io_active ()) historic_read(ac);
    if (io_active ()) fleas_set_quotes(ac);
    if (io_active ()) acc_operations_from_historic(ac);
    if (io_active ()) fleas_run(ac);
    if (io_active ()) {
      void fn (void *null) { conf_set_activity(ACT_SLEEPING2); }
      asyncActor_wait(ac, fn, NULL);
    }
  }
}

time_t sleeping2_time = 0;
static void sleeping2 (AsyncActor *ac) {
  time_t now = date_now();
  if (now - sleeping2_time < 7 * 60) {
    return;
  }
  sleeping2_time = now;
  puts("Sleeping2 without implementation");
  return;
  if (io_active ()) {
    void fn (void *null) { conf_set_activity(ACT_ACTIVATING); }
    asyncActor_wait(ac, fn, NULL);
  }
}

static void activating (AsyncActor *ac) {
  puts("Activating without implementation");
  if (io_active ()) {
    void fn (void *null) { conf_set_activity(ACT_ACTIVE); }
    asyncActor_wait(ac, fn, NULL);
  }
}

static void active (AsyncActor *ac) {
  puts("Active without implementation");
  if (io_active ()) {
    void fn (void *null) { conf_set_activity(ACT_DEACTIVATING); }
    asyncActor_wait(ac, fn, NULL);
  }
}

static void deactivating (AsyncActor *ac) {
  puts("Deactivating without implementation");
  if (io_active ()) {
    void fn (void *null) { conf_set_activity(ACT_SLEEPING1); }
    asyncActor_wait(ac, fn, NULL);
  }
}

void scheduler_run (AsyncActor *ac) {
  while (io_active ()) {
    char *activity = NULL;
    void fn (void *null) { activity = conf_activity(); }
    asyncActor_wait(ac, fn, NULL);

    if (str_eq(activity, ACT_SLEEPING1)) {
      sleeping1(ac);
    } else if (str_eq(activity, ACT_SLEEPING2)) {
      sleeping2(ac);
    } else if (str_eq(activity, ACT_ACTIVATING)) {
      activating(ac);
    } else if (str_eq(activity, ACT_ACTIVE)) {
      active(ac);
    } else if (str_eq(activity, ACT_DEACTIVATING)) {
      deactivating(ac);
    } else
      EXC_ILLEGAL_STATE(str_f("Activity '%s' is unknown", activity))

    sys_sleep(SCHEDULER_SLEEP);
  }
}
