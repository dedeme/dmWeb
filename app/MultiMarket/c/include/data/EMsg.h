// Copyright 03-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Error message.

#ifndef DATA_EMSG_H
  #define DATA_EMSG_H

#include "dmc/std.h"

/*--*/

///
///   Arguments:
///     error: int
///     msg: char*
typedef struct EMsg_EMsg EMsg;

///
EMsg *eMsg_new (int error, char *msg);

/// Its value is one of ErrorMsg defined in DEFS.h
int eMsg_error (EMsg *this);

///
char *eMsg_msg (EMsg *this);

/*--*/

#endif
