// Copyright 08-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/calendar.h"
#include "io.h"
#include "data/MarketDay.h"

static char *calendar = NULL;

void calendar_init (void) {
  calendar = path_cat(io_data_dir(), "calendar.db", NULL);
  if (!file_exists(calendar)) {
    file_write(calendar, "{}");
  }
}

// returns Map[Js]
static Map *read (void) {
  if (!calendar) EXC_ILLEGAL_STATE("'calendar.db' was not intiliazed")

  return js_ro((Js *)file_read(calendar));
}

// kv is Kv[Js]
static void write (char *key, Js *value) {
  // Map[Js]
  Map *m = read();
  map_put(m, key, value);
  file_write(calendar, (char *)js_wo(m));
}

Timetable *calendar_general (void) {
  return timetable_from_js(opt_oget(
    map_get(read(), "general"), timetable_to_js(timetable_new())
  ));
}

void calendar_set_general (Timetable *time_table) {
  write("general", timetable_to_js(time_table));
}

Arr *calendar_holidays (void) {
  return arr_from_js(
    opt_oget(map_get(read(), "holidays"), "[]"),
    (FFROM)timetable_from_js
  );
}

void calendar_set_holidays (Arr *holidays) {
  write("holidays", arr_to_js(holidays, (FTO)timetable_to_js));
}

Arr *calendar_special_days (void) {
  return arr_from_js(
    opt_oget(map_get(read(), "specialDays"), "[]"),
    (FFROM)marketDay_from_js
  );
}

void calendar_set_special_days (Arr *special_days) {
  write("specialDays", arr_to_js(special_days, (FTO)marketDay_to_js));
}
