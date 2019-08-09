// Copyright 06-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Nick data.

#ifndef DATA_NICK_H
  #define DATA_NICK_H

#include "dmc/async.h"

/*--*/

///
///   Arguments:
///     id: int
///     name: char*
///   Variables:
///     is_sel: bool
typedef struct Nick_Nick Nick;

///
Nick *nick_new (int id, char *name);

///
int nick_id (Nick *this);

///
char *nick_name (Nick *this);

///
void nick_set_name (Nick *this, char *value);

///
int nick_is_sel (Nick *this);

///
void nick_set_is_sel (Nick *this, int value);

///
Js *nick_to_js (Nick *this);

///
Nick *nick_from_js (Js *js);

/*--*/

#endif
