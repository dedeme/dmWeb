// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Tests page of Fleas.

#ifndef SERVER_FLEAS_TESTS_H
  #define SERVER_FLEAS_TESTS_H

#include "dmc/async.h"

/// Entry point to process requests.
char *testsFleas_process (AsyncActor *ac, Map *mrq);

#endif
