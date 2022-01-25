// Copyright 22-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "procs/prfix.h"
#include "dmc/async.h"
#include "dmc/date.h"
#include "data/Ann/AAnn.h"
#include "db/anns.h"
#include "db/log.h"

#include <stdio.h>

void prfix_process (void) {
  AAnn *to_run = aAnn_new();
  AAnn *rest = aAnn_new();

  time_t now = date_now();
  /**/void filter (void) {
  /**/  AAnn *anns = anns_read();
  /**/  Ann **p = anns->es;
  /**/  while (p < anns->end) {
  /**/    Ann *a = *p++;
  /**/    if (a->time_type == ann_FIX && ann_date(a) <= now)
  /**/      aAnn_push(to_run, a);
  /**/    else aAnn_push(rest, a);
  /**/  }
  /**/  anns_write(rest);
  /**/}
  async_run(filter);

  Ann **p = to_run->es;
  while (p < to_run->end) {
    Ann *a = *p++;
    if (a->text_type == ann_COMMAND) log_info(ann_run(a));
    else log_msg(a->text);
  }
}
