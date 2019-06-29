// Copyright 12-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Fleas data base.

#ifndef IO_FLEASDB_H
  #define IO_FLEASDB_H

#include "dmc/std.h"
#include "data/Rs.h"

/// Initilizes fleas data base.
void fleasdb_init();

/// Returns Arr[char]
Arr *fleasdb_model_dates (char * model);

/// Returns Js -> Arr[RsWeb]
Js *fleasdb_model_read_js (char *model, char *date);

/// 'rs' is Arr[RsWeb].<p>
/// 'params' has the following structure:
///   [
///     (0) params0: [
///         (0) prefix: String -- Some text before number or ""
///         (1) multiplicator: Double -- (normaly '1', for % '100'
///         (2) decimal: Int -- Decimal positions
///         (3) postfix: String -- Some text after number or ""
///       ]
///     (-) params-: [...]
///     --- So many as flea parameters
///   ]
void fleasdb_model_write (char *model, Js *params, char *date, Arr *rs);

/// Returns Arr[RsBests]
Arr *fleasdb_bests_read (char *model);

/// Returns Js -> Arr[RsBests]
Js *fleasdb_bests_read_js (char *model);

///
void fleasdb_bests_add (char *model, RsBests *rs);

/// Returns Js -> Arr[char]
Js *fleasdb_charts_read_nicks_js (char *model);

/// Returns Js -> Opt[RsChart]
Js *fleasdb_charts_read_js (char *model, char *nick);

///
void fleasdb_charts_write (char *model, RsCharts *rs);

/// Returns Arr[RsChampions]
Arr *fleasdb_champions_read (int nparams);

/// Returns Js -> Arr[RsChampions]
Js *fleasdb_champions_read_js (int nparams);

/// If 'rs' is duplicate, sets value. If not adds it.
void fleasdb_champions_add (RsChampions *rs);

/// 'rss' is Arr[RsChampions]
void fleasdb_champions_write (int nparams, Arr *rss);

/// Writes a fleas log entry. 'msg' can not finish in '\n'
void fleasdb_flog_write (char *msg);

/// Returns Js->Arr[char]. Reads fleas log.
Js *fleasdb_flog_to_js (void);

/// Clears fleas log
void fleasdb_flog_clear (void);

#endif
