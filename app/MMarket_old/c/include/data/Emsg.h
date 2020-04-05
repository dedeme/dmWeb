// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Error message.

#ifndef DATA_EMSG_H
  #define DATA_EMSG_H

#include "dmc/async.h"

/*--*/

/// Error message
///   Arguments:
///     error: int
///     msg: char*
typedef struct Emsg_Emsg Emsg;

///
Emsg *emsg_new (int error, char *msg);

/// Its value is one of ErrorMsg defined in DEFS.h
int emsg_error (Emsg *this);

///
char *emsg_msg (Emsg *this);

/*--*/

#endif
