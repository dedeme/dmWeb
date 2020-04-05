// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Entry record for historic data read from server

#ifndef DATA_HISTORIC_H
  #define DATA_HISTORIC_H

#include "dmc/async.h"
#include "Quote.h"

/*--*/

/// Record for historic data read from server
///   Arguments:
///     date: char*
///     open: double
///     close: double
///     max: double
///     min: double
///     vol: int
typedef struct Historic_HistoricEntry HistoricEntry;

///
HistoricEntry *historicEntry_new (
  char *date,
  double open,
  double close,
  double max,
  double min,
  int vol
);

///
char *historicEntry_date (HistoricEntry *this);

///
double historicEntry_open (HistoricEntry *this);

///
double historicEntry_close (HistoricEntry *this);

///
double historicEntry_max (HistoricEntry *this);

///
double historicEntry_min (HistoricEntry *this);

///
int historicEntry_vol (HistoricEntry *this);

/*--*/

Quote *historicEntry_to_quote (HistoricEntry *this);

#endif
