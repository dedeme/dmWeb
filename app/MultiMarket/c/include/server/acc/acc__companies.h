// Copyright 24-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from acc->companies

#ifndef SERVER_ACC_ACC__COMPANIES_H
  #define SERVER_ACC_ACC__COMPANIES_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *acc__companies_process(AsyncActor *ac, Map *mrq);

#endif
