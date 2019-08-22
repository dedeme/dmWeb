// Copyright 20-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// File system reader.

#ifndef IO_IO_H
  #define IO_IO_H

#include "dmc/std.h"
#include "data/IndexTree.h"
#include "data/Doc.h"

/// 'path' is the absolute path of a library.
IndexTree *io_index (char *path);

/// Returns Opt[Doc]
///   path: Absolute path of .h file.
Opt *io_module (char *path);

/// Returns .h/.c code
///   path: Absolute path of file.
char *io_code (char *path);

#endif
