// Copyright 24-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/DailyEntry.h"

/* .
DailyEntry:
  code: char *
  close: double
*/
/*--*/

struct DailyEntry_DailyEntry {
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
