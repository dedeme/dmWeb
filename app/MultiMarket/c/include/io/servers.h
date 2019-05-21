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

/// Removes Server with id 'id'
void servers_remove (int id);

/// Sets name and short_name of Server with id 'id'. If short name is duplicated
/// returns 0. Otherwise returns 1.
int servers_set_names (int id, char *short_name, char *name);

/// Set codes. If 'id' does not exists, this function does nothing.
///   id: Server id
///   codes: Arr[ServerCodes]. New codes
void servers_set_codes(int id, Arr *codes);

/// Adds a new nick with id 'nk_id' if it does not exist
void servers_add_nick (int nk_id);

/// Removes nick with id 'nk_id' if it exists.
void servers_del_nick (int nk_id);

#endif
