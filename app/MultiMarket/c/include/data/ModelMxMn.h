// Copyright 13-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Tuple Name-Maximum-Minimum of a parameter.

#ifndef DATA_MODELMXMN_H
  #define DATA_MODELMXMN_H

#include "dmc/std.h"

/*--*/

/// Tuple Name-Maximum-Minimum of a parameter.
///   Arguments:
///     name: char*
///     max: double
///     min: double
typedef struct ModelMxMn_ModelMxMn ModelMxMn;

///
ModelMxMn *modelMxMn_new (char *name, double max, double min);

///
char *modelMxMn_name (ModelMxMn *this);

///
double modelMxMn_max (ModelMxMn *this);

///
double modelMxMn_min (ModelMxMn *this);

///
Js *modelMxMn_to_js (ModelMxMn *this);

///
ModelMxMn *modelMxMn_from_js (Js *js);

/*--*/

/// Returns Arr[char] Names of parameters. 'pmxmns' is Arr[ModelMxMn]
Arr *modelMxMn_names (Arr *pmxmns);

#endif
