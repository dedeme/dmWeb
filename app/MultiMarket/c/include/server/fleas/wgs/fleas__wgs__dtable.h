// Copyright 30-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from fleas->wgs->Dtable

#ifndef SERVER_FLEAS_WGS_FLEAS__WGS__DTABLE_H
  #define SERVER_FLEAS_WGS_FLEAS__WGS__DTABLE_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *fleas__wgs__dtable_process(AsyncActor *ac, Map *mrq);

#endif
