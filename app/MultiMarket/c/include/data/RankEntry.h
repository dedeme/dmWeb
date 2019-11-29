// Copyright 28-Nov-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_RANKENTRY_H
  #define DATA_RANKENTRY_H

#include "dmc/async.h"
#include "Flea.h"
#include "Model.h"

/*--*/

///
///   Arguments:
///     model_name: char*
///     flea: Flea
typedef struct RankEntry_RankEntry RankEntry;

///
RankEntry *rankEntry_new (char *model_name, Flea *flea);

///
char *rankEntry_model_name (RankEntry *this);

///
Flea *rankEntry_flea (RankEntry *this);

///
Js *rankEntry_to_js (RankEntry *this);

///
RankEntry *rankEntry_from_js (Js *js);

/*--*/

/// Returns Opt<Model>
Opt *rankEntry_model (RankEntry *this);

#endif
