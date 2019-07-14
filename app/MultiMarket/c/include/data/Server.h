// Copyright 10-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Server to read quotes.

#ifndef DATA_SERVER_H
  #define DATA_SERVER_H

#include "dmc/std.h"

/*--*/

///
///   Arguments:
///     nick_id: int
///     code: Opt-char*
typedef struct Server_ServerCode ServerCode;

///
ServerCode *serverCode_new (int nick_id, Opt *code);

///
int serverCode_nick_id (ServerCode *this);

/// Opt[char]
Opt *serverCode_code (ServerCode *this);

///
Js *serverCode_to_js (ServerCode *this);

///
ServerCode *serverCode_from_js (Js *js);

///
///   Arguments:
///     id: int
///     short_name: char*
///   Variables:
///     name: char*
///     daily_conf: Opt-Rconf
///     historic_conf: Opt-Rconf
///     codes: Arr-ServerCode
typedef struct Server_Server Server;

///
int server_id (Server *this);

///
char *server_short_name (Server *this);

///
void server_set_short_name (Server *this, char *value);

///
char *server_name (Server *this);

///
void server_set_name (Server *this, char *value);

/// Opt[Rconf]
Opt *server_daily_conf (Server *this);

///
void server_set_daily_conf (Server *this, Opt *value);

/// Opt[Rconf]
Opt *server_historic_conf (Server *this);

///
void server_set_historic_conf (Server *this, Opt *value);

/// Arr[ServerCode]
Arr *server_codes (Server *this);

///
Js *server_to_js (Server *this);

///
Server *server_from_js (Js *js);

/*--*/

/// 'nicks' is Arr[Nick]
Server *server_new(int id, char *short_name, Arr *nicks);

/// If nick_id has not code, returns "".
char *server_nick_code (Server *this, int nick_id);

/// Code is "" if nick_id has not code.<p>
/// If this has not nick_id registred, this functions does nothing.
void server_set_nick_code (Server *this, int nick_id, char *code);

#endif
