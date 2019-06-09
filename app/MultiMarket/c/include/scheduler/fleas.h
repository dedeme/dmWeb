// Copyright 29-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Fleas main.

#ifndef SCHEDULER_FLEAS_H
  #define SCHEDULER_FLEAS_H

#include "dmc/async.h"

/// Load historic quotes. This function must be called before 'fleas_run()'
void fleas_set_quotes (AsyncActor *ac);

/// Running fleas in thread apart
void fleas_run(AsyncActor *ac);

#endif
