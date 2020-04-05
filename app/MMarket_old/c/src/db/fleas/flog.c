// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db/fleas/flog.h"
#include "dmc/date.h"
#include "data/LogEntry.h"
#include "DEFS.h"

static char *fid = "";

static char *path () {
  return path_cat(sys_home(), FLEAS_PATH, "Flog.db", NULL);
}

static char *now () {
  return date_f(date_now(), "%d/%m/%Y(%H:%M:%S)");
}

static void add_entry (LogEntry *e) {
  // Arr[js]
  Arr *es = js_ra((Js *)file_read(path()));
  arr_insert(es, 0, logEntry_to_js(e));
  file_write(path(), (char *)js_wa(arr_take(es, LOG_MAX_ENTRIES)));
}

void flog_init (void) {
  if (!file_exists(path())) {
    file_write(path(), (char *)js_wa(arr_new()));
  }
}

char *flog_new (void) {
  DateTm *tm = dateTm_now();
  fid = str_f("%d%d", tm->tv_sec, tm->tv_usec);
  file_write(path(), (char *)js_wa(arr_new()));
  return fid;
}

void flog_stop (char *id) {
  fid = "";
}

void flog_info (char *id, char *msg) {
  if (str_eq(id, fid)) add_entry(logEntry_new(0, now(), msg));
}

Js *flog_read (char *id) {
  void *fto (void *e) { return e; }
  return *id && str_eq(id, fid)
    ? opt_to_js(opt_new(file_read(path())), (FTO)fto)
    : opt_to_js(opt_empty(), (FTO)fto)
  ;
}

