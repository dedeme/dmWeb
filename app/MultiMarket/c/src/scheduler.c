// Copyright 04-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "scheduler.h"
#include "dmc/date.h"
#include "net.h"
#include "io.h"
#include "io/conf.h"
#include "io/calendar.h"
#include "io/quotes.h"
#include "io/sbox.h"
#include "io/dailydb.h"
#include "io/log.h"
#include "DEFS.h"
#include "scheduler/historic.h"
#include "scheduler/fleas.h"
#include "scheduler/acc.h"

static void sleeping1 (AsyncActor *ac) {
  asyncActor_wait(ac, (FPROC)log_info, "In Sleeping (1)");
  int hour = atoi(date_f(date_now(), "%H"));
  if (hour > ACT_HISTORIC_START && hour < ACT_HISTORIC_END) {
    if (io_active ()) {
      void fn (void *null) {
        conf_set_activity(ACT_HISTORIC);
        log_info("To Historic");
      }
      asyncActor_wait(ac, fn, NULL);
    }
  }
}

static void historic (AsyncActor *ac) {
  if (io_active ()) historic_update(ac);
  if (io_active ()) net_update_daily(ac);
  if (io_active ()) acc_historic_profits(ac);
  if (io_active ()) fleas_run(ac);
  if (io_active ()) {
    void fn (void *null) {
      conf_set_activity(ACT_SLEEPING2);
      log_info("To Sleeping (2)");
    }
    asyncActor_wait(ac, fn, NULL);
  }
}

time_t sleeping2_time = 0;
static void sleeping2 (AsyncActor *ac) {
  time_t now = date_now();
  if (now - sleeping2_time < 7 * 60) {
    return;
  }
  sleeping2_time = now;
  if (io_active () && calendar_is_open(now)) {
    void fn (void *null) {
      conf_set_activity(ACT_ACTIVATING);
      log_info("To Activating");
    }
    asyncActor_wait(ac, fn, NULL);
  }
}

static void activating (AsyncActor *ac) {
  if (io_active ()) {
    void fn1 (void *null) {
      dailydb_reset();
      sbox_next();
    }
    asyncActor_wait(ac, fn1, NULL);
    net_update_daily(ac);
    void fn2 (void *null) {
      conf_set_activity(ACT_ACTIVE);
      log_info("To Active");
    }
    asyncActor_wait(ac, fn2, NULL);
  }
}

time_t active_time = 0;
static void active (AsyncActor *ac) {
  time_t now = date_now();
  if (now - active_time < 2 * 60) {
    return;
  }
  active_time = now;

  net_update_daily(ac);
  void fn (void *null) { dailydb_update(); }
  asyncActor_wait(ac, fn, NULL);

  if (io_active () && !calendar_is_open(now)) {
    void fn (void *null) {
      conf_set_activity(ACT_DEACTIVATING);
      log_info("To Deactivating");
    }
    asyncActor_wait(ac, fn, NULL);
  }
}

static void deactivating (AsyncActor *ac) {
  net_update_daily(ac);
  if (io_active ()) {
    void fn (void *null) {
      conf_set_activity(ACT_SLEEPING1);
      log_info("To Sleeping (1)");
    }
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
    } else if (str_eq(activity, ACT_HISTORIC)) {
      historic(ac);
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
