// Copyright 22-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "procs/prinit.h"
#include "dmc/async.h"
#include "data/Ann/AAnn.h"
#include "db/anns.h"
#include "db/log.h"

void prinit_process (void) {
  AAnn *anns;

  /**/void read (void) {
  /**//**/int filter (Ann *a) { return a->time_type == ann_INIT; }
  /**/  anns = aAnn_filter_to(anns_read(), filter);
  /**/}
  async_run(read);

  Ann **p = anns->es;
  while (p < anns->end) log_info(ann_run(*p++));
}
