// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Server to read quotes.

#ifndef DATA_SERVER_H
  #define DATA_SERVER_H

#include "dmc/async.h"

/*--*/

/// Nicks-Code pair.
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

/// Server configuration
///   Arguments:
///     cmd: char*
///     url: char*
///     regex: char*
///     sel: int
///     is_date_eu: bool
///     date_separator: char*
///     is_iso_number: bool
///     fields_type: char*
///     table_start: char*
///     table_end: char*
///     row_start: char*
///     row_end: char*
///     cols_start: Arr-char*
///     cols_end: Arr-char*
typedef struct Server_ServerConf ServerConf;

///
char *serverConf_cmd (ServerConf *this);

///
char *serverConf_url (ServerConf *this);

///
char *serverConf_regex (ServerConf *this);

/// enum Server
int serverConf_sel (ServerConf *this);

///
void serverConf_set_sel (ServerConf *this, int value);

///
int serverConf_is_date_eu (ServerConf *this);

///
char *serverConf_date_separator (ServerConf *this);

///
int serverConf_is_iso_number (ServerConf *this);

///
char *serverConf_fields_type (ServerConf *this);

///
char *serverConf_table_start (ServerConf *this);

///
char *serverConf_table_end (ServerConf *this);

///
char *serverConf_row_start (ServerConf *this);

///
char *serverConf_row_end (ServerConf *this);

///
Arr *serverConf_cols_start (ServerConf *this);

///
Arr *serverConf_cols_end (ServerConf *this);

///
Js *serverConf_to_js (ServerConf *this);

///
ServerConf *serverConf_from_js (Js *js);

/// Server data
///   Arguments:
///     id: int
///     short_name: char*
///   Variables:
///     name: char*
///     daily_conf: Opt-ServerConf
///     historic_conf: Opt-ServerConf
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

/// Opt[ServerConf]
Opt *server_daily_conf (Server *this);

///
void server_set_daily_conf (Server *this, Opt *value);

/// Opt[ServerConf]
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

/// Code is "" if nick_id has not code.
/// If this has not nick_id registred, this functions does nothing.
void server_set_nick_code (Server *this, int nick_id, char *code);

#endif
