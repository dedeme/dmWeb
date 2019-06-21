// Copyright 10-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Management of 'servers&#46;db'

#ifndef IO_SERVERS_H
  #define IO_SERVERS_H

#include "dmc/async.h"
#include "data/Server.h"
#include "data/Rconf.h"

/*--*/

///
typedef struct servers_Servers Servers;

///
Js *servers_to_js(Servers *this);

///
Servers *servers_from_js(Js *js);

///
typedef struct servers_ServersIdNameCode ServersIdNameCode;

///
Js *serversIdNameCode_to_js(ServersIdNameCode *this);

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

/// Activates / Deactivates server
///   id: Server identifier. It it does not exists, this function does nothing.
///   historic: (1/0) If historic configuration will be updated
///   conf: Initial configuration
void servers_activate(int id, int historic, Rconf *conf);

/// Sets configurations.
///   id: Server identifier. It it does not exists, this function does nothing.
///   historic: (1/0) If historic configuration will be updated
///   conf: New configuration
void servers_set_conf(int id, int historic, Rconf *conf);

/// Set codes. If 'id' does not exists, this function does nothing.
///   id: Server id
///   codes: Arr[ServerCodes]. New codes
void servers_set_codes(int id, Arr *codes);

/// Adds a new nick with id 'nk_id' if it does not exist
void servers_add_nick (int nk_id);

/// Removes nick with id 'nk_id' if it exists.
void servers_del_nick (int nk_id);

/// Returns Arr[SeversIdNameCode]. If 'nick_id' does not have code, its code
/// value is an empty string.
Arr *servers_nick_codes (int nick_id);

/// Sets code of nick_id.
void servers_set_nick_code (int server_id, int nick_id, char *code);

/// Returns '1' if daily closes reading is correct. Otherwise returns '0' and
/// makes an annotation in Log.
int servers_test_daily_conf (int id);

/// Returns '1' if historic quotes reading is correct. Otherwise returns '0' and
/// makes an annotation in Log.
int servers_test_historic_conf (int id, int nk_id);

#endif
