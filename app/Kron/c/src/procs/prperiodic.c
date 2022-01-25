// Copyright 22-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "procs/prperiodic.h"
#include <stdlib.h>
#include <string.h>
#include "dmc/async.h"
#include "dmc/date.h"
#include "data/Ann/AAnn.h"
#include "db/anns.h"
#include "db/log.h"
#include "db/exes.h"

static int days_contains(AInt *days, int day) {
  int *p = days->es;
  while (p < days->end) {
    if (*p++ == day) return 1;
  }
  return 0;
}

static time_t make_atime (time_t now, Ann *a) {
  time_t atime = ann_date(a);
  struct tm t;
  memset(&t, 0, sizeof(struct tm));
  t.tm_year = date_year(now) - 1900;
  t.tm_mon = date_month(now) - 1;
  t.tm_mday = date_day(now);
  t.tm_hour = localtime(&atime)->tm_hour;
  t.tm_min = localtime(&atime)->tm_min;
  t.tm_sec = 0;

  return (time_t) mktime(&t);
}

static int execute (char *today, Ann *a) {
  AExe *exes = exes_read();
  /**/int filterf (Exe *e) { return strcmp(e->day, today) >= 0; }
  aExe_filter_in(exes, filterf);
  /**/int indexf (Exe *e) { return e->id == a->id; }
  if (aExe_index(exes, indexf) == -1) {
    aExe_push(exes, exe_new(a->id, today));
    exes_write(exes);
    return 1;
  } else {
    return 0;
  }
}

static AAnn *prperiodic_select (void) {
  AAnn *to_run = aAnn_new();
  time_t now = date_now();
  char *today = date_to_str(now);
  int week_day = atoi(date_f(now, "%u")) - 1;

  AAnn *anns = anns_read();
  Ann **p = anns->es;
  while (p < anns->end) {
    Ann *a = *p++;
    if (a->time_type == ann_PERIODIC && days_contains(ann_days(a), week_day)) {
      time_t atime = make_atime(now, a);
      if (atime <= now && execute(today, a)) {
        aAnn_push(to_run, a);
      }
    }
  }

  return to_run;
}

void prperiodic_process (void) {
  AAnn *to_run = NULL;

  /**/void select (void) { to_run = prperiodic_select(); }
  async_run(select);

  Ann **p = to_run->es;
  while (p < to_run->end) {
    Ann *a = *p++;
    if (a->text_type == ann_COMMAND) log_info(ann_run(a));
    else log_msg(a->text);
  }
}
