// Copyright 08-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys/sys__schedule.h"
#include "dmc/cgi.h"
#include "io.h"
#include "io/calendar.h"
#include "data/Timetable.h"
#include "data/MarketDay.h"

// mrq is Map[Js]
char *sys__schedule_process(AsyncActor *ac, Map *mrq) {
  // Map[Js]
  Map *rp = map_new();
  CGI_GET_STR(rq, mrq, "rq")

  if (str_eq(rq, "general")) {
    void (fn)(void *null) {
      map_put(rp, "general", timetable_to_js(calendar_general()));
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "setGeneral")) {
    CGI_GET(Timetable *, timetable, timetable_from_js, mrq, "timetable")
    void (fn)(void *null) { calendar_set_general(timetable); }
    asyncActor_wait(ac, fn, NULL);
    return cgi_empty();
  }

  if (str_eq(rq, "holidays")) {
    void (fn)(void *null) {
      map_put(rp, "holidays", arr_to_js(calendar_holidays(), (FTO)js_ws));
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "setHolidays")) {
    // Arr[char]
    Arr *fget(Js *js) { return arr_from_js(js, (FFROM)js_rs); }
    CGI_GET(Arr *, holidays, fget, mrq, "holidays")
    void (fn)(void *null) { calendar_set_holidays(holidays); }
    asyncActor_wait(ac, fn, NULL);
    return cgi_empty();
  }

  if (str_eq(rq, "specialDays")) {
    void (fn)(void *null) {
      map_put(rp, "specialDays",
        arr_to_js(calendar_special_days(), (FTO)marketDay_to_js));
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "setSpecialDays")) {
    // Arr[char]
    Arr *fget(Js *js) { return arr_from_js(js, (FFROM)marketDay_from_js); }
    CGI_GET(Arr *, special_days, fget, mrq, "specialDays")
    void (fn)(void *null) { calendar_set_special_days(special_days); }
    asyncActor_wait(ac, fn, NULL);
    return cgi_empty();
  }

  EXC_ILLEGAL_ARGUMENT(
    "rq",
    "general | setGeneral | holidays | setHolidays | "
    "specialDays | setSpecialDays",
    rq
  )
  return NULL; // Unreachable
}

