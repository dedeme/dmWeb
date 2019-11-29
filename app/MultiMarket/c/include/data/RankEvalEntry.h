// Copyright 28-Nov-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_RANKEVALENTRY_H
  #define DATA_RANKEVALENTRY_H

#include "dmc/async.h"
#include "Flea.h"
#include "Model.h"

/*--*/

///
///   Arguments:
///     model_name: char*
///     flea: Flea
///     assets: double
///     profits: double
///     days: double
///     points: double
typedef struct RankEvalEntry_RankEvalEntry RankEvalEntry;

///
RankEvalEntry *rankEvalEntry_new (
  char *model_name,
  Flea *flea,
  double assets,
  double profits,
  double days,
  double points
);

///
char *rankEvalEntry_model_name (RankEvalEntry *this);

///
Flea *rankEvalEntry_flea (RankEvalEntry *this);

///
double rankEvalEntry_assets (RankEvalEntry *this);

/// avg moduled with variance
double rankEvalEntry_profits (RankEvalEntry *this);

///
double rankEvalEntry_days (RankEvalEntry *this);

///
double rankEvalEntry_points (RankEvalEntry *this);

///
void rankEvalEntry_set_points (RankEvalEntry *this, double value);

///
Js *rankEvalEntry_to_js (RankEvalEntry *this);

///
RankEvalEntry *rankEvalEntry_from_js (Js *js);

/*--*/

/// Returns Opt<Model>
Opt *rankEvalEntry_model (RankEvalEntry *this);

#endif
