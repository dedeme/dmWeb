// Copyright 19-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Selection page of Fleas Ftests.

#ifndef SERVER_FLEAS_FTESTS_SELECTION_H
  #define SERVER_FLEAS_FTESTS_SELECTION_H

#include "dmc/async.h"

/// Entry point to process requests.
char *selection_process (AsyncActor *ac, Map *mrq);

#endif
