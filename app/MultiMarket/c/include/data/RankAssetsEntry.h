// Copyright 28-Nov-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_RANKASSETSENTRY_H
  #define DATA_RANKASSETSENTRY_H

#include "dmc/async.h"
#include "Flea.h"
#include "Model.h"

/*--*/

///
///   Arguments:
///     model_name: char*
///     flea: Flea
///     assets: int
///     points: int
typedef struct RankAssetsEntry_RankAssetsEntry RankAssetsEntry;

///
RankAssetsEntry *rankAssetsEntry_new (
  char *model_name,
  Flea *flea,
  int assets,
  int points
);

///
char *rankAssetsEntry_model_name (RankAssetsEntry *this);

///
Flea *rankAssetsEntry_flea (RankAssetsEntry *this);

///
int rankAssetsEntry_assets (RankAssetsEntry *this);

///
int rankAssetsEntry_points (RankAssetsEntry *this);

///
Js *rankAssetsEntry_to_js (RankAssetsEntry *this);

///
RankAssetsEntry *rankAssetsEntry_from_js (Js *js);

/*--*/

/// Returns Opt<Model>
Opt *rankAssetsEntry_model (RankAssetsEntry *this);

#endif
