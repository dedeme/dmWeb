// Copyright 12-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Manager data base

#ifndef IO_MANAGERDB_H
  #define IO_MANAGERDB_H

#include "dmc/async.h"
#include "data/Manager.h"

///
void managerdb_init ();

///
Manager *managerdb_read (void);

///
void managerdb_write (Manager *mg);

///
ModelParams *managerdb_default (void);

///
ModelParams *managerdb_nick (char *nick);

/// Adds or changes ModelParams of 'nick' to 'managerdb_default'
void managerdb_set_nick_default (char *nick);

/// Adds or changes ModelParams of 'nick'. If model does not exist or 'params'
/// is not correct, returns '1'.
///   nick: nick to change. If its value is "", change default parameters.
///   model: Model name.
///   params: Model parameters
///   return: 0 if success, 1 if fails.
int managerdb_set_nick (char *nick, char *model, Darr *params);

#endif
