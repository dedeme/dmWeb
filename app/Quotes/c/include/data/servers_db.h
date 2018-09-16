// Copyright 11-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_SERVERS_DB_H
  #define DATA_SERVERS_DB_H

#include "dmc/std.h"
#include "Server.h"

///
Aserver *servers_db_list(void);

///
Server *servers_db_get(char *server_name);

#endif
