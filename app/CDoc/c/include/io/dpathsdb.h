// Copyright 18-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Paths reader.

#ifndef IO_DPATHSDB_H
  #define IO_DPATHSDB_H

#include "dmc/std.h"
#include "data/Dpath.h"

/// Arr[Dpath]
Arr *dpathsdb_load (void);

/// paths is Arr[Dpath]
void dpathsdb_save (Arr *paths);

/// Opt[Dpath]
Opt *dpathdb_get (char *id);

#endif
