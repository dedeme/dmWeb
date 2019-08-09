// Copyright 07-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Data to manage rankings

#ifndef DATA_RANK_H
  #define DATA_RANK_H

#include "dmc/async.h"
#include "Rs.h"

/*--*/

/// Pair date-double to use with charts.
///   Arguments:
///     date: char*
///     assets: double
typedef struct Rank_RankAssets RankAssets;

///
RankAssets *rankAssets_new (char *date, double assets);

///
char *rankAssets_date (RankAssets *this);

///
double rankAssets_assets (RankAssets *this);

///
Js *rankAssets_to_js (RankAssets *this);

///
RankAssets *rankAssets_from_js (Js *js);

/// Pair date-int to use with charts.
///   Arguments:
///     date: char*
///     position: int
typedef struct Rank_RankPosition RankPosition;

///
RankPosition *rankPosition_new (char *date, int position);

///
char *rankPosition_date (RankPosition *this);

///
int rankPosition_position (RankPosition *this);

///
Js *rankPosition_to_js (RankPosition *this);

///
RankPosition *rankPosition_from_js (Js *js);

/// Data to send to client.
///   Arguments:
///     result: RsChampions
///     assets: Arr-RankAssets
///     positions: Arr-RankPosition
typedef struct Rank_RankFlea RankFlea;

///
RankFlea *rankFlea_new (RsChampions *result, Arr *assets, Arr *positions);

///
RsChampions *rankFlea_result (RankFlea *this);

/// Arr[RankAssets]
Arr *rankFlea_assets (RankFlea *this);

/// Arr[RankPosition]
Arr *rankFlea_positions (RankFlea *this);

///
Js *rankFlea_to_js (RankFlea *this);

///
RankFlea *rankFlea_from_js (Js *js);

/// Pair date-int to use with charts.
///   Arguments:
///     model: char*
///     flea: char*
///     is_new: bool
///     variation: int
typedef struct Rank_Rank Rank;

///
Rank *rank_new (
  char *model,
  char *flea,
  int is_new,
  int variation
);

///
char *rank_model (Rank *this);

///
char *rank_flea (Rank *this);

///
int rank_is_new (Rank *this);

/// If is_new == 1, variation == 0
int rank_variation (Rank *this);

///
Js *rank_to_js (Rank *this);

///
Rank *rank_from_js (Js *js);

/*--*/

/// Returns Arr[Arr[RankPosition]] Calculate historic positions in ranking.
///   assets: (Arr[Arr[RankAssets]]) Table of assetes. Rows are fleas
///     values and fields are assets for each date
///   return: (Arr[Arr[RankPosition]]) Table of positions. Rows are fleas
///     values and fields are positions for each date
Arr *rank_mk_positions (Arr *assets);

/// Returns Arr[Rank]
///   rss: Arr[RsChampions]
///   positions: Arr[Arr[RankPosition]]
Arr *rank_mk_ranking (Arr *rss, Arr *positions);

#endif
