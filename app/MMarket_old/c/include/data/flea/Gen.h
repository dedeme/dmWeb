// Copyright 19-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Flea gen.

#ifndef DATA_FLEA_GEN_H
  #define DATA_FLEA_GEN_H

#include "dmc/async.h"
#include "dmc/Darr.h"

///
typedef Darr Gen;

/// Creates a new gen.
Gen *gen_new(int n, double *params);

/// Number of elements of 'this'
int gen_size(Gen *this);

/// Pointer to parameters of 'this'
double *gen_params(Gen *this);

/// Returns a new gen mutation of 'this'
Gen *gen_mutate(Gen *this);

/// Returns a duplicate of 'this'
Gen *gen_copy(Gen *this);

///
Js *gen_to_js(Gen *this);

///
Gen *gen_from_js(Js *js);

#endif
