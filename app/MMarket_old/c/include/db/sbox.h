// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Daily server selector.

#ifndef DB_SBOX_H
  #define DB_SBOX_H

#include "dmc/async.h"

///
void sbox_init ();

/// Generates next server.
void sbox_next ();

/// Returns Opt[Server] Current server.
Opt *sbox_get ();

#endif
