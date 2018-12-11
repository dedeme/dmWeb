// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef CORE_BACKUPS_H
  #define CORE_BACKUPS_H

#include "dmc/std.h"
#include "dmc/cgi.h"
#include "dmc/ct/Mjson.h"

///
CgiRp *backups_process(char *app_name, char *data_version, Mjson *rq);

#endif
