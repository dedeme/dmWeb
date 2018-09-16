// Copyright 10-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_CLOSE_H
  #define DATA_CLOSE_H

#include "dmc/std.h"

/*.-.*/

///
typedef struct close_Close Close;

///
Close *close_new(char *nick, double close);

///
char *close_nick(Close *this);

///
double close_close(Close *this);

/*.-.*/

#define TY Close
#define FN close
#include "dmc/tpl/tarr.h"
#undef TY
#undef FN

#define TY Aclose
#define FN aclose
#include "dmc/tpl/topt.h"
#undef TY
#undef FN



#endif
