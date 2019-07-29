// Copyright 18-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// All data structures

#ifndef DATA_H
  #define DATA_H

#include "dmc/std.h"

/*--*/

///
///   Arguments:
///     first: Arr-char*
///     second: Arr-char*
///     third: Arr-char*
typedef struct Data_DataPreviousGroup DataPreviousGroup;

///
DataPreviousGroup *dataPreviousGroup_new (Arr *first, Arr *second, Arr *third);

/// Arr[char]
Arr *dataPreviousGroup_first (DataPreviousGroup *this);

/// Arr[char]
Arr *dataPreviousGroup_second (DataPreviousGroup *this);

/// Arr[char]
Arr *dataPreviousGroup_third (DataPreviousGroup *this);

///
Js *dataPreviousGroup_to_js (DataPreviousGroup *this);

///
DataPreviousGroup *dataPreviousGroup_from_js (Js *js);

///
///   Arguments:
///     daily_g: DataPreviousGroup
///     short_g: DataPreviousGroup
///     medium_g: DataPreviousGroup
///     long_g: DataPreviousGroup
typedef struct Data_DataPrevious DataPrevious;

///
DataPrevious *dataPrevious_new (
  DataPreviousGroup *daily_g,
  DataPreviousGroup *short_g,
  DataPreviousGroup *medium_g,
  DataPreviousGroup *long_g
);

///
DataPreviousGroup *dataPrevious_daily_g (DataPrevious *this);

///
DataPreviousGroup *dataPrevious_short_g (DataPrevious *this);

///
DataPreviousGroup *dataPrevious_medium_g (DataPrevious *this);

///
DataPreviousGroup *dataPrevious_long_g (DataPrevious *this);

///
Js *dataPrevious_to_js (DataPrevious *this);

///
DataPrevious *dataPrevious_from_js (Js *js);

///
///   Arguments:
///     dif: double
///     result: int
typedef struct Data_DataMatchResult DataMatchResult;

///
DataMatchResult *dataMatchResult_new (double dif, int result);

/// Positive -> up wins. Negative down wins.
double dataMatchResult_dif (DataMatchResult *this);

/// 1 (up wins), 0 (draw), 2 (down wins) or '-1' (Resting)
int dataMatchResult_result (DataMatchResult *this);

///
Js *dataMatchResult_to_js (DataMatchResult *this);

///
DataMatchResult *dataMatchResult_from_js (Js *js);

///
///   Arguments:
///     results: Arr-DataMatchResult
typedef struct Data_DataRoundResult DataRoundResult;

///
DataRoundResult *dataRoundResult_new (Arr *results);

///
Arr *dataRoundResult_results (DataRoundResult *this);

///
Js *dataRoundResult_to_js (DataRoundResult *this);

///
DataRoundResult *dataRoundResult_from_js (Js *js);

///
///   Arguments:
///     nicks: Arr-char*
///     results: Arr-DataRoundResult
typedef struct Data_DataLeague DataLeague;

///
DataLeague *dataLeague_new (Arr *nicks, Arr *results);

/// Arr[char]
Arr *dataLeague_nicks (DataLeague *this);

///
Arr *dataLeague_results (DataLeague *this);

///
Js *dataLeague_to_js (DataLeague *this);

///
DataLeague *dataLeague_from_js (Js *js);

///
///   Arguments:
///     code: char*
///     first: DataLeague
///     second: DataLeague
///     third: DataLeague
typedef struct Data_DataLeagueGroup DataLeagueGroup;

///
DataLeagueGroup *dataLeagueGroup_new (
  char *code,
  DataLeague *first,
  DataLeague *second,
  DataLeague *third
);

/// 'code' is sum of quotes for daily group and last date for the rest
char *dataLeagueGroup_code (DataLeagueGroup *this);

/// Arr[char]
DataLeague *dataLeagueGroup_first (DataLeagueGroup *this);

/// Arr[char]
DataLeague *dataLeagueGroup_second (DataLeagueGroup *this);

/// Arr[char]
DataLeague *dataLeagueGroup_third (DataLeagueGroup *this);

///
Js *dataLeagueGroup_to_js (DataLeagueGroup *this);

///
DataLeagueGroup *dataLeagueGroup_from_js (Js *js);

///
///   Arguments:
///     daily_g: DataLeagueGroup
///     short_g: DataLeagueGroup
///     medium_g: DataLeagueGroup
///     long_g: DataLeagueGroup
typedef struct Data_DataCurrent DataCurrent;

///
DataCurrent *dataCurrent_new (
  DataLeagueGroup *daily_g,
  DataLeagueGroup *short_g,
  DataLeagueGroup *medium_g,
  DataLeagueGroup *long_g
);

///
DataLeagueGroup *dataCurrent_daily_g (DataCurrent *this);

///
DataLeagueGroup *dataCurrent_short_g (DataCurrent *this);

///
DataLeagueGroup *dataCurrent_medium_g (DataCurrent *this);

///
DataLeagueGroup *dataCurrent_long_g (DataCurrent *this);

///
Js *dataCurrent_to_js (DataCurrent *this);

///
DataCurrent *dataCurrent_from_js (Js *js);

///
///   Arguments:
///     previous: DataPrevious
///     current: DataCurrent
typedef struct Data_DataAll DataAll;

///
DataAll *dataAll_new (DataPrevious *previous, DataCurrent *current);

///
DataPrevious *dataAll_previous (DataAll *this);

///
DataCurrent *dataAll_current (DataAll *this);

///
Js *dataAll_to_js (DataAll *this);

///
DataAll *dataAll_from_js (Js *js);

/*--*/

#endif
