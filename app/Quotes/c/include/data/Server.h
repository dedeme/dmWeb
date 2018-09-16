// Copyright 10-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_SERVER_H
  #define DATA_SERVER_H

#include "dmc/std.h"
#include "Quote.h"
#include "Close.h"

/// 'code' is the server code for a company
typedef Oaquote *(*fread_t) (char *code) /* fread_t */;

///
typedef Oaclose *(*fread_last_t) (void) /* fread_last_t */;

/*.-.*/

///
typedef struct server_Server Server;

///
Server *server_new(char *name, fread_t read, fread_last_t read_last);

///
char *server_name(Server *this);

///
fread_t server_read(Server *this);

///
fread_last_t server_read_last(Server *this);

/*.-.*/

/// Returns a map nickId -> nickCode
Mchar *server_nicks(Server *this);

/// 'codes' es a map QuotesId -> ServerId
void server_set_nicks(Server *this, Mchar *codes);

#define TY Server
#define FN server
#include "dmc/tpl/tarr.h"
#undef TY
#undef FN

#endif
