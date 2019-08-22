// Copyright 18-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Configuration data base.

#ifndef IO_CONFDB_H
  #define IO_CONFDB_H

#include "dmc/std.h"
#include "data/Conf.h"

///
Conf *confdb_load (void);

///
void confdb_save (Conf *cf);


#endif
