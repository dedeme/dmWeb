// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef CORE_CHPASS_H
  #define CORE_CHPASS_H

#include "dmc/std.h"

#include "dmc/cgi.h"
#include "dmc/ct/Mjson.h"

///
CgiRp *chpass_process(Mjson *rq);

#endif
