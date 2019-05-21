// Copyright 04-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "scheduler.h"
#include "io.h"
#include "DEFS.h"


void scheduler_run (void *null) {
  while (io_active ()) {
    sys_sleep(SCHEDULER_SLEEP);
  }
}
