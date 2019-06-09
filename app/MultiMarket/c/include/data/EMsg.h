// Copyright 03-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_EMSG_H
  #define DATA_EMSG_H

#include "dmc/std.h"

/*--*/

///
typedef struct EMsg_EMsg EMsg;

///
EMsg *eMsg_new(int error, char *msg);

///
int eMsg_error(EMsg *this);

///
char *eMsg_msg(EMsg *this);

/*--*/

#endif
