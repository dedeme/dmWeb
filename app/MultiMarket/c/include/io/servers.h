// Copyright 10-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef IO_SERVERS_H
  #define IO_SERVERS_H

#include "dmc/async.h"
#include "data/Server.h"

/*--*/

///
typedef struct servers_Servers Servers;

///
Js *servers_to_js(Servers *this);

///
Servers *servers_from_js(Js *js);

/*--*/

///
void servers_init (void);

/// Arr[Server]
Arr *servers_list (void);

/// If 'short_name' is duplicate, operation is not done and it returns 0.
int servers_add (char *short_name);

/// Adds a new nick with id 'nk_id' if it does not exist
void servers_add_nick (int nk_id);

/// Removes nick with id 'nk_id' if it exists.
void servers_del_nick (int nk_id);

#endif
