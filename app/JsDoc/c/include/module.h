// Copyright 06-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef MODULE_H
  #define MODULE_H

#include "dmc/std.h"
#include "dmc/cgi.h"
#include "dmc/ct/Mjson.h"


///
CgiRp *module_process(Mjson *rq);

#endif
