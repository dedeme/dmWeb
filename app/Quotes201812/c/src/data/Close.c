// Copyright 10-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Close.h"

/* .+.
-struct: Close
  nick: char *
  close: double
*/
/*.-.*/
#include "dmc/ct/Ajson.h"

struct close_Close {
  char *nick;
  double close;
};

Close *close_new(char *nick, double close) {
  Close *this = MALLOC(Close);
  XNULL(nick)
  this->nick = nick;
  this->close = close;
  return this;
}

char *close_nick(Close *this) {
  XNULL(this)
  return this->nick;
}

double close_close(Close *this) {
  XNULL(this)
  return this->close;
}
/*.-.*/

#define TY Close
#define FN close
#include "dmc/tpl/tarr.c"
#undef TY
#undef FN

#define TY Aclose
#define FN aclose
#include "dmc/tpl/topt.c"
#undef TY
#undef FN

