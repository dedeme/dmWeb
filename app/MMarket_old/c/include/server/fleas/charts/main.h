// Copyright 23-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Main page of Fleas charts.

#ifndef SERVER_FLEAS_CHARTS_MAIN_H
  #define SERVER_FLEAS_CHARTS_MAIN_H

#include "dmc/async.h"

/// Entry point to process requests.
char *mainCharts_process (AsyncActor *ac, Map *mrq);

#endif
