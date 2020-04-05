// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "scheduler.h"
#include "dmc/date.h"
#include "data/activity.h"
#include "net/net.h"
//#include "io/io.h"
//#include "io/conf.h"
//#include "io/quotes.h"
//#include "io/sbox.h"
//#include "io/dailydb.h"
#include "db/calendar.h"
#include "db/log.h"
#include "db/global.h"
#include "DEFS.h"
//#include "scheduler/historic.h"
#include "scheduler/fleas.h"
//#include "scheduler/acc.h"
//#include "scheduler/management.h"

static void sleeping1 (AsyncActor *ac) {
  int hour = atoi(date_f(date_now(), "%H"));
  if (hour > ACT_HISTORIC_START && hour < ACT_HISTORIC_END) {
    if (activity_active ()) {
      void fn () {
        global_set_activity(ACT_HISTORIC);
        log_info("To Historic");
      }
      asyncActor_wait(ac, fn);
    }
  }
}

static void historic (AsyncActor *ac) {
  if (activity_active ()) log_info("historic_update(ac)");
  if (activity_active ()) log_info("net_update_daily(ac)");
  if (activity_active ()) log_info("acc_historic_profits(ac)");
  if (activity_active ()) log_info("management_update(ac)");
  if (activity_active ()) {
    log_info("Fleas running.");
    fleas_run(ac);
  }
  if (activity_active ()) {
    void fn () {
      global_set_activity(ACT_SLEEPING2);
      log_info("To Sleeping (2)");
    }
    asyncActor_wait(ac, fn);
  }
}

time_t sleeping2_time = 0;
static void sleeping2 (AsyncActor *ac) {
  time_t now = date_now();
  if (now - sleeping2_time < 7 * 60) {
    return;
  }
  sleeping2_time = now;
  if (activity_active () && calendar_is_open(now)) {
    void fn () {
      global_set_activity(ACT_ACTIVATING);
      log_info("To Activating");
    }
    asyncActor_wait(ac, fn);
  }
}

static void activating (AsyncActor *ac) {
  if (activity_active ()) {
    void fn1 () {
      log_info("dailydb_reset()");
      log_info("sbox_next()");
    }
    asyncActor_wait(ac, fn1);
    log_info("net_update_daily(ac)");
    void fn2 () {
      global_set_activity(ACT_ACTIVE);
      log_info("To Active");
    }
    asyncActor_wait(ac, fn2);
  }
}

time_t active_time = 0;
static void active (AsyncActor *ac) {
  time_t now = date_now();
  if (now - active_time < 2 * 60) {
    return;
  }
  active_time = now;

  log_info("net_update_daily(ac)");
  void fn () { log_info("dailydb_update()"); }
  asyncActor_wait(ac, fn);

  if (activity_active () && !calendar_is_open(now)) {
    void fn () {
      global_set_activity(ACT_DEACTIVATING);
      log_info("To Deactivating");
    }
    asyncActor_wait(ac, fn);
  }
}

static void deactivating (AsyncActor *ac) {
  log_info("net_update_daily(ac)");
  if (activity_active ()) {
    void fn () {
      global_set_activity(ACT_SLEEPING1);
      log_info("To Sleeping (1)");
    }
    asyncActor_wait(ac, fn);
  }
}

void scheduler_run (AsyncActor *ac) {
  while (activity_active ()) {
    char *activity = NULL;
    void fn () { activity = global_activity(); }
    asyncActor_wait(ac, fn);

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
