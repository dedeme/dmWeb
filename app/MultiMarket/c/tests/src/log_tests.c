// Copyright 03-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "log_tests.h"
#include "assert.h"
#include "io/log.h"
#include "DEFS.h"

// returns Arr[char]
static Arr *get_log (void) {
  return arr_from_js(log_to_js(), (FFROM)js_rs);
}

static char *text (char *e) {
  return arr_get(str_csplit_trim(e, *e == 'W' ? '-' : '='), 1);
}

void log_tests() {
  puts("Log tests:");

  // Arr[char]
  Arr *l = get_log();

  REPEAT(1200)
    log_error("Test");
  _REPEAT

  l = get_log();

  assert(arr_size(l) == LOG_MAX_ENTRIES + 1);
  char *msg = arr_get(l, 0);
  assert(*msg);

  log_error("Err 1");
  l = get_log();
  assert(arr_size(l) == LOG_MAX_ENTRIES + 1);
  msg = arr_get(l, 0);
  assert(*msg == 'E');
  assert(str_eq(text(msg), "Err 1"));

  log_info("Warn 1");
  l = get_log();
  assert(arr_size(l) == LOG_MAX_ENTRIES + 1);
  msg = arr_get(l, 0);
  assert(*msg == 'W');
  assert(str_eq(text(msg), "Warn 1"));
  msg = arr_get(l, 1);
  assert(*msg == 'E');
  assert(str_eq(text(msg), "Err 1"));

  puts("    Finished");
}
