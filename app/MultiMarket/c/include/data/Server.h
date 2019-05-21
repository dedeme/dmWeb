// Copyright 10-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_SERVER_H
  #define DATA_SERVER_H

#include "dmc/async.h"

/*--*/

///
typedef struct Server_ServerCode ServerCode;

///
ServerCode *serverCode_new(int nick_id, Opt *code);

///
int serverCode_nick_id(ServerCode *this);

///Opt[char]
Opt *serverCode_code(ServerCode *this);

///
Js *serverCode_to_js(ServerCode *this);

///
ServerCode *serverCode_from_js(Js *js);

///
typedef struct Server_Server Server;

///
Server *server_new(int id, char *short_name);

///
int server_id(Server *this);

///
char *server_short_name(Server *this);

///
void server_set_short_name(Server *this, char *value);

///
char *server_name(Server *this);

///
void server_set_name(Server *this, char *value);

///Opt[Rconf]
Opt *server_daily_conf(Server *this);

///
void server_set_daily_conf(Server *this, Opt *value);

///Opt[Rconf]
Opt *server_historic_conf(Server *this);

///
void server_set_historic_conf(Server *this, Opt *value);

///Arr[ServerCode]
Arr *server_codes(Server *this);

///
Js *server_to_js(Server *this);

///
Server *server_from_js(Js *js);

/*--*/

#endif
