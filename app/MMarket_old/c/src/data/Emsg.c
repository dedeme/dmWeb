// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Emsg.h"

/* .
# Error message
Emsg
  # Its value is one of ErrorMsg defined in DEFS.h
  error: int
  msg: char *
*/

/*--*/

struct Emsg_Emsg {
  int error;
  char *msg;
};

Emsg *emsg_new (int error, char *msg) {
  Emsg *this = MALLOC(Emsg);
  this->error = error;
  this->msg = msg;
  return this;
}

int emsg_error (Emsg *this) {
  return this->error;
}

char *emsg_msg (Emsg *this) {
  return this->msg;
}

/*--*/
