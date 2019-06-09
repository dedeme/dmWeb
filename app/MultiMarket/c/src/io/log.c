// Copyright 03-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/log.h"
#include "dmc/date.h"
#include "io.h"
#include "DEFS.h"

static char *log = NULL;

static char *date (void) {
  return date_f(date_now(), "%d/%m/%Y(%H:%M:%S)");
}

void log_init (void) {
  log = path_cat(io_data_dir(), "log.txt", NULL);
  if (!file_exists(log)) {
    file_write(log, "[]");
  }
}

static void write (char *msg) {
  if (!log) EXC_ILLEGAL_STATE("'log.txt' was not intiliazed")

  // Arr[char]
  Arr *msgs = arr_from_js((Js *)file_read(log), (FFROM)js_rs);
  while (arr_size(msgs) > LOG_MAX_ENTRIES) {
    arr_remove(msgs, LOG_MAX_ENTRIES);
  }
  arr_insert(msgs, 0, msg);
  file_write(log, (char *)arr_to_js(msgs, (FTO)js_ws));
}

void log_error (char *msg) {
  write(str_f("E%s = %s", date(), msg));
}

void log_info (char *msg) {
  write(str_f("W%s - %s", date(), msg));
}

// stack is Arr[char]
void log_exception (Exc *ex) {
  log_error(str_f("%s\n  %s", exc_msg(ex), str_join(exc_stack(ex), "\n  ")));
}

Js *log_to_js (void) {
  if (!log) EXC_ILLEGAL_STATE("'log.txt' was not intiliazed")

  return (Js *)file_read(log);
}
