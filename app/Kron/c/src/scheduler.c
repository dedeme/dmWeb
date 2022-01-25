// Copyright 13-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "dmc/async.h"
#include "scheduler.h"
#include "dmc/sys.h"
#include "data/cts.h"
#include "procs/prinit.h"
#include "procs/prfix.h"
#include "procs/prperiodic.h"

static int scheduler_active = 1;

static void normal_process (void *v) {
  prfix_process();
  prperiodic_process();
}

void scheduler_run (void) {
  /**/ void init (void *v) { prinit_process(); }
  async_thread_detached(init, NULL);

  int times = 0;
  while (scheduler_active) {
    sys_sleep(cts_scheduler_sleep());
    ++times;
    if (times >= cts_scheduler_times()) {
      async_thread_detached(normal_process, NULL);
      times = 0;
    }
  }
}

void scheduler_stop(void) {
  scheduler_active = 0;
}
