// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/settings/calendar.h"
#include "dmc/cgi.h"
#include "data/MarketDay.h"
#include "db/global.h"
#include "db/calendar.h"

char *calendar_process (AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq);
  if (str_eq(rq, "idata")) {
    // Map[Js]
    Map *rp = map_new();
    map_put(rp, "timeStamp", js_ws(global_time_stamp()));
    map_put(rp, "general", timetable_to_js(calendar_general()));
    map_put(rp, "holidays", arr_to_js(calendar_holidays(), (FTO)js_ws));
    map_put(
      rp,
      "specialDays",
      arr_to_js(calendar_special_days(), (FTO)marketDay_to_js)
    );
    return cgi_ok(rp);
  };
  if (str_eq(rq, "setGeneral")) {
    CGI_GET_STR(timeStamp, mrq);
    CGI_GET(Timetable *, timetable, timetable_from_js, mrq);

    // Map[Js]
    Map *rp = map_new();
    if (global_check_time_stamp(timeStamp)) {
      char *tmst;
      void fn () {
        calendar_set_general(timetable);
        tmst = global_set_time_stamp();
      }
      asyncActor_wait(ac, fn);
      map_put(rp, "timeStamp", js_ws(tmst));
      map_put(rp, "ok", js_wb(1));
    } else {
      map_put(rp, "ok", js_wb(0));
    }
    return cgi_ok(rp);
  }

  if (str_eq(rq, "setSpecialDays")) {
    CGI_GET_STR(timeStamp, mrq);
    CGI_GET_ARR(specialDays, mrq);

    // Map[Js]
    Map *rp = map_new();
    if (global_check_time_stamp(timeStamp)) {
      char *tmst;
      void fn () {
        calendar_set_special_days(
          arr_map(specialDays, (FCOPY)marketDay_from_js)
        );
        tmst = global_set_time_stamp();
      }
      asyncActor_wait(ac, fn);
      map_put(rp, "timeStamp", js_ws(tmst));
      map_put(rp, "ok", js_wb(1));
    } else {
      map_put(rp, "ok", js_wb(0));
    }
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT(
    "rq",
    "idata | setGeneral | setHolidays | setSpecialDays",
    rq
  )
  return NULL; // Unreachable
}
