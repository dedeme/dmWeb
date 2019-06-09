// Copyright 24-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/HistoricEntry.h"

/* .
HistoricEntry:
  date: char *
  open: double
  close: double
  max: double
  min: double
  vol: int
*/
/*--*/

struct HistoricEntry_HistoricEntry{
  char *date;
  double open;
  double close;
  double max;
  double min;
  int vol;
};

HistoricEntry *historicEntry_new(
  char *date,
  double open,
  double close,
  double max,
  double min,
  int vol
) {
  HistoricEntry *this = MALLOC(HistoricEntry);
  this->date = date;
  this->open = open;
  this->close = close;
  this->max = max;
  this->min = min;
  this->vol = vol;
  return this;
}

char *historicEntry_date(HistoricEntry *this) {
  return this->date;
}

double historicEntry_open(HistoricEntry *this) {
  return this->open;
}

double historicEntry_close(HistoricEntry *this) {
  return this->close;
}

double historicEntry_max(HistoricEntry *this) {
  return this->max;
}

double historicEntry_min(HistoricEntry *this) {
  return this->min;
}

int historicEntry_vol(HistoricEntry *this) {
  return this->vol;
}

/*--*/

Quote *historicEntry_to_quote (HistoricEntry *this) {
  return quote_new(
    this->date, this->open, this->close, this->max, this->min, this->vol, 0
  );
}
