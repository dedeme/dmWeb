// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Nick data.

#ifndef DATA_NICK_H
  #define DATA_NICK_H

#include "dmc/async.h"

/*--*/

/// Nick data.
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

/// Pair nick-quoteValue
///   Arguments:
///     nick: int
///     value: double
typedef struct Nick_NickQvalue NickQvalue;

///
NickQvalue *nickQvalue_new (int nick, double value);

/// Nick id
int nickQvalue_nick (NickQvalue *this);

/// close, open, maximum or minimum
double nickQvalue_value (NickQvalue *this);

/*--*/

#endif
