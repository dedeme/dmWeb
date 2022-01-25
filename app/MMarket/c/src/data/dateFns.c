// Copyright 18-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dateFns.h"
#include "dmc/date.h"

char *dateFns_last_sunday (void) {
  time_t today = date_now();
  int wd = date_week_day(today);
  return date_to_str(date_add(today, -wd));
}
