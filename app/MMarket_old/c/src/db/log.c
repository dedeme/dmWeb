// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db/log.h"
#include "dmc/date.h"
#include "data/LogEntry.h"
#include "DEFS.h"

static char *path () {
  return path_cat(sys_home(), DATA_PATH, "Log.db", NULL);
}

static char *now () {
  return date_f(date_now(), "%d/%m/%Y(%H:%M:%S)");
}

static void add_entry (LogEntry *e) {
  // Arr[js]
  Arr *es = js_ra(log_read());
  arr_insert(es, 0, logEntry_to_js(e));
  file_write(path(), (char *)js_wa(arr_take(es, LOG_MAX_ENTRIES)));
}

void log_init (void) {
  if (!file_exists(path())) {
    file_write(path(), (char *)js_wa(arr_new()));
  }
}

void log_reset (void) {
  file_write(path(), (char *)js_wa(arr_new()));
}

void log_error (char *msg) {
  add_entry(logEntry_new(1, now(), msg));
}

void log_info (char *msg) {
  add_entry(logEntry_new(0, now(), msg));
}

void log_exception (Exc *ex) {
  log_error(str_f("%s\n  %s", exc_msg(ex), str_join(exc_stack(ex), "\n  ")));
}

Js *log_read (void) {
  return (Js *)file_read(path());
}
