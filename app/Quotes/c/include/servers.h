// Copyright 11-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef SERVERS_H
  #define SERVERS_H

#include "dmc/std.h"
#include "dmc/cgi.h"
#include "dmc/ct/Mjson.h"

///
CgiRp *servers_process(Mjson *rqm);

#endif
