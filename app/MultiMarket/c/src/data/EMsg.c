// Copyright 03-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/EMsg.h"

/* .
EMsg
  error: int
  msg: char *
*/
/*--*/

struct EMsg_EMsg{
  int error;
  char *msg;
};

EMsg *eMsg_new(int error, char *msg) {
  EMsg *this = MALLOC(EMsg);
  this->error = error;
  this->msg = msg;
  return this;
}

int eMsg_error(EMsg *this) {
  return this->error;
}

char *eMsg_msg(EMsg *this) {
  return this->msg;
}

/*--*/
