// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db/global.h"
#include "dmc/date.h"
#include "DEFS.h"

static char *path () {
  return path_cat(sys_home(), DATA_PATH, "Global.db", NULL);
}

static char *rs (char *key, char *default_value) {
  // Map[Js]
  Map *m = js_ro((Js *)file_read(path()));
  Js *v = opt_nget(map_get(m, key));
  if (v) return js_rs(v);
  return default_value;
}

static void ws (char *key, char *value) {
  // Map[Js]
  Map *m = js_ro((Js *)file_read(path()));
  map_put(m, key, js_ws(value));
  file_write(path(), (char *)js_wo(m));
}

void global_init (void) {
  if (!file_exists(path())) {
    file_write(path(), "{}");
  }
}

char *global_lang (void) {
  return rs("lang", "es");
}

void global_set_lang (char *lang) {
  ws("lang", lang);
}

char *global_activity (void) {
  return rs("activity", ACT_SLEEPING1);
}

void global_set_activity (char *activity) {
  ws("activity", activity);
}

char *global_time_stamp (void) {
  return rs("timestamp", "0");
}

/// Sets time stamp with the current time.
char *global_set_time_stamp (void) {
  DateTm *tm = dateTm_now();
  char *tmst = str_f("%d%d", tm->tv_sec, tm->tv_usec);
  ws("timestamp", tmst);
  return tmst;
}

/// Check if current time stamp is equals to 'ts'
int global_check_time_stamp (char *ts) {
  return str_eq(global_time_stamp(), ts);
}


