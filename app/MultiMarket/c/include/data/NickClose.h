// Copyright 24-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Tuple nick-close used in data/Server.

#ifndef DATA_NICKCLOSE_H
  #define DATA_NICKCLOSE_H

#include "dmc/std.h"

/*--*/

///
typedef struct NickClose_NickClose NickClose;

///
NickClose *nickClose_new(int nick, double close);

/// Nick id
int nickClose_nickx(NickClose *this);

///
double nickClose_close(NickClose *this);

/*--*/

#endif
