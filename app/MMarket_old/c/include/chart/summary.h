// Copyright 24-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Data for summary charts

#ifndef CHART_SUMMARY_H
  #define CHART_SUMMARY_H

#include "dmc/async.h"
#include "data/flea/Fmodel.h"
#include "data/flea/Flea.h"
#include "data/Qtable.h"

/*--*/

/// Record for historic data.
///   Arguments:
///     date: char*
///     assets: double
///     profits: double
///     market: double
typedef struct summary_SummaryData SummaryData;

///
char *summaryData_date (SummaryData *this);

///
double summaryData_assets (SummaryData *this);

///
double summaryData_profits (SummaryData *this);

///
double summaryData_market (SummaryData *this);

///
Js *summaryData_to_js (SummaryData *this);

/*--*/

/// Returns Arr[SumaryData] Historic data.
///   model: The model of flea.
///   dates: Dates of opens and closes.
///   opens: opens table.
///   closes: closes table.
///   params: flea parameters.
Arr *summary_historic(
  Fmodel *model,
  Arr *dates, // Arr[char]
  Qtable *opens,
  Qtable *closes,
  Darr *params
);

/// Returns positions in ranking. Postions number can be 0 (that is 'Iarr' is
/// empty.
///   model  : The model of 'flea'
///   flea   : The flea to get data.
///   ranking: Ranking data.
Iarr *summary_ranking(
  Fmodel *model,
  Flea *flea,
  Arr *ranking // Arr[Arr[Investor]]
);

#endif
