// Copyright 23-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Companies page of Fleas charts.

#ifndef SERVER_FLEAS_CHARTS_COMPANIES_H
  #define SERVER_FLEAS_CHARTS_COMPANIES_H

#include "dmc/async.h"

/// Entry point to process requests.
char *companies_process (AsyncActor *ac, Map *mrq);

#endif
