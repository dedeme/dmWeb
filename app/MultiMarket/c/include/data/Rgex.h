// Copyright 12-Jan-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_RGEX_H
  #define DATA_RGEX_H

/// Regular expresion used in Server.

#include "dmc/async.h"

/*--*/

///
///   Arguments:
///     rgex: char*
///     subs: char*
typedef struct Rgex_Rgex Rgex;

///
Rgex *rgex_new (char *rgex, char *subs);

///
char *rgex_rgex (Rgex *this);

///
char *rgex_subs (Rgex *this);

///
Js *rgex_to_js (Rgex *this);

///
Rgex *rgex_from_js (Js *js);

/*--*/

#endif
