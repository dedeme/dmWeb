// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Daily.h"

/* .
# Record for daily data read from server.
DailyEntry:
  code: char *
  close: double
*/

/*--*/

struct Daily_DailyEntry {
  char *code;
  double close;
};

DailyEntry *dailyEntry_new (char *code, double close) {
  DailyEntry *this = MALLOC(DailyEntry);
  this->code = code;
  this->close = close;
  return this;
}

char *dailyEntry_code (DailyEntry *this) {
  return this->code;
}

double dailyEntry_close (DailyEntry *this) {
  return this->close;
}

/*--*/
