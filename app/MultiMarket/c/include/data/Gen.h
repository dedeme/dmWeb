// Copyright 11-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Flea gen.

#ifndef DATA_GEN_H
  #define DATA_GEN_H

#include "dmc/async.h"
#include "dmc/Darr.h"

///
typedef Darr Gen;

/// Creates a new gen with n values equals to 0.5.
Gen *gen_new(int n);

/// Number of elements of 'this'
int gen_n(Gen *this);

/// Pointer to values of 'this'
double *gen_values(Gen *this);

/// Returns a new gen mutation of 'this'
Gen *gen_mutate(Gen *this);

/// Returns a duplicate of 'this'
Gen *gen_copy(Gen *this);

///
Js *gen_to_js(Gen *this);

///
Gen *gen_from_js(Js *js);

#endif
