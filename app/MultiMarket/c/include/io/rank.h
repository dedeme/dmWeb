// Copyright 28-Nov-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef IO_RANK_H
  #define IO_RANK_H

#include "dmc/async.h"
#include "data/RankEntry.h"
#include "data/RankAssetsEntry.h"
#include "data/RankEvalEntry.h"

///
void rank_init (void);

/// Undate ranking database
void rank_update (void);

#endif
