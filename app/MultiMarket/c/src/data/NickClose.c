// Copyright 24-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/NickClose.h"

/* .
NickClose
  # Nick id
  nick: int
  close: double
*/

/*--*/

struct NickClose_NickClose {
  int nick;
  double close;
};

NickClose *nickClose_new (int nick, double close) {
  NickClose *this = MALLOC(NickClose);
  this->nick = nick;
  this->close = close;
  return this;
}

int nickClose_nick (NickClose *this) {
  return this->nick;
}

double nickClose_close (NickClose *this) {
  return this->close;
}

/*--*/
