// Copyright 30-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from fleas->wgs->Wcharts

#ifndef SERVER_FLEAS_WGS_FLEAS__WGS__WCHARTS_H
  #define SERVER_FLEAS_WGS_FLEAS__WGS__WCHARTS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *fleas__wgs__wcharts_process(AsyncActor *ac, Map *mrq);

#endif
