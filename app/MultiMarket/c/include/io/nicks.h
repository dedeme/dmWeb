// Copyright 06-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Management of 'nicks&#46;db'

#ifndef IO_NICKS_H
  #define IO_NICKS_H

#include "dmc/async.h"
#include "data/Nick.h"

/*--*/

///
typedef struct nicks_Nicks Nicks;

///
Js *nicks_to_js(Nicks *this);

///
Nicks *nicks_from_js(Js *js);

/*--*/

/// Initializes data base.
void nicks_init (void);

/// Returns id of nick model or -1 if it has not been set.
int nicks_model (void);

/// Sets nick model
void nicks_set_model(int nk_id);

/// Arr[Nick]
Arr *nicks_list (void);

/// Adds a nick if it is not duplicated and returns 1. Otherwise returns 0.
int nicks_add(char *nk_name);

/// Removes nick with id 'id' if it exists
void nicks_del(int nk_id);

/// Returns Opt[Nick] with the nick which id is 'id'. If it does not exist
/// returns 'opt_empty()'
Opt *nicks_get(int nk_id);

/// Modifies nick if 'nick_name(nick)' is not duplicated and returns 1.
/// Otherwise returns 0
int nicks_modify(Nick *nick);

/// Set the nick with 'nick_id' selected on/off. If nick_id does not exist,
/// it does nothing
///   value: is 0 or 1.
void nicks_set_selected(int nk_id, int value);

#endif
