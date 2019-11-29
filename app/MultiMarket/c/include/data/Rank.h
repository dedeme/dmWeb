// Copyright 07-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Data to manage rankings

#ifndef DATA_RANK_H
  #define DATA_RANK_H

#include "dmc/async.h"
#include "Rs.h"
#include "RankAssetsEntry.h"

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
///     is_new: bool
///     variation: int
///     points: int
///     assets: int
///     model: char*
///     flea: char*
typedef struct Rank_Rank Rank;

///
Rank *rank_new (
  int is_new,
  int variation,
  int points,
  int assets,
  char *model,
  char *flea
);

///
int rank_is_new (Rank *this);

/// If is_new == 1, variation == 0. Otherwise variation can be in range [-2 - 2]
int rank_variation (Rank *this);

///
int rank_points (Rank *this);

///
int rank_assets (Rank *this);

///
char *rank_model (Rank *this);

///
char *rank_flea (Rank *this);

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
///   rk     : Arr[RankAssetsEntry] Current ranking
///   prev_rk: Arr[RankAssetsEntry] Previous ranking
Arr *rank_mk_ranking (Arr *rk, Arr *prev_rk);

#endif
