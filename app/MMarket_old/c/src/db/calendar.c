// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db/calendar.h"
#include "dmc/date.h"
//#include "io/io.h"
#include "data/MarketDay.h"
//#include "DEFS.h"

static char *path () {
  return path_cat(sys_home(), DATA_PATH, "Calendar.db", NULL);
}


void calendar_init (void) {
  if (!file_exists(path())) {
    file_write(path(), "{}");
  }
}

// returns Map[Js]
static Map *read (void) {
  return js_ro((Js *)file_read(path()));
}

// kv is Kv[Js]
static void write (char *key, Js *value) {
  // Map[Js]
  Map *m = read();
  map_put(m, key, value);
  file_write(path(), (char *)js_wo(m));
}

Timetable *calendar_general (void) {
  return timetable_from_js(opt_oget(
    map_get(read(), "general"), timetable_to_js(timetable_new())
  ));
}

void calendar_set_general (Timetable *time_table) {
  write("general", timetable_to_js(time_table));
}

// Returns Arr[char]
Arr *calendar_holidays (void) {
  return arr_from_js(
    opt_oget(map_get(read(), "holidays"), "[]"),
    (FFROM)js_rs
  );
}

// 'holidays' is Arr[char]
void calendar_set_holidays (Arr *holidays) {
  write("holidays", arr_to_js(holidays, (FTO)js_ws));
}

// Returns Arr[MarketDay]
Arr *calendar_special_days (void) {
  return arr_from_js(
    opt_oget(map_get(read(), "specialDays"), "[]"),
    (FFROM)marketDay_from_js
  );
}

// 'special_days' is Arr[MarketDay]
void calendar_set_special_days (Arr *special_days) {
  write("specialDays", arr_to_js(special_days, (FTO)marketDay_to_js));
}

int calendar_is_open (time_t date) {
  date = date - SERVERS_DELAY;

  if (str_cindex("12345", *date_f(date, "%u")) != -1) {
    char *d = date_to_str(date);
    int fcontains (char *dt) { return str_eq(dt, d); }
    if (it_contains(arr_to_it(calendar_holidays()), (FPRED)fcontains)) {
      return 0;
    }

    int h = atoi(date_f(date, "%H"));
    int m = atoi(date_f(date, "%M"));
    Timetable *tt = calendar_general();
    int hopen = timetable_hopen(tt);
    int mopen = timetable_mopen(tt);
    int hclose = timetable_hclose(tt);
    int mclose = timetable_mclose(tt);

    int ffind (MarketDay *md) { return str_eq(marketDay_date(md), d); }
    // Opt[MarkeDay]
    Opt *special_day = it_find(
      arr_to_it(calendar_special_days()), (FPRED)ffind
    );
    if (opt_is_full(special_day)) {
      MarketDay *md = opt_get(special_day);
      hopen = marketDay_hopen(md);
      mopen = marketDay_mopen(md);
      hclose = marketDay_hclose(md);
      mclose = marketDay_mclose(md);
    }

    if (
      (h > hopen || (h == hopen && m > mopen)) &&
      (h < hclose || (h == hclose && m < mclose))
    ) {
      return 1;
    }
    return 0;
  }
  return 0;
}
