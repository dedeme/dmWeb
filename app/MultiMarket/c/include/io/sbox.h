// Copyright 26-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Daily servers selector.

#ifndef IO_SBOX_H
  #define IO_SBOX_H

#include "dmc/async.h"


///
void sbox_init ();

/// Generates next server
void sbox_next ();

/// Returns Opt[Server] Current server
Opt *sbox_get ();

#endif
