// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef CORE_SETTINGS_H
  #define CORE_SETTINGS_H

#include "dmc/std.h"
#include "dmc/cgi.h"
#include "dmc/ct/Mjson.h"

///
CgiRp *settings_process(Mjson *rq);

#endif
