// Copyright 04-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef CONF_H
  #define CONF_H

/// Basic data base.

#include "dmc/std.h"
#include "dmc/Json.h"

/// Creates a 'config' file if it does not exist and intializes it.
/// The file is a serialized json object with the following fields:
///   lang: char * (Default 'en')
///   menu: char * (Default "settings")
///   edit_id: char * (Default "")
///   issue_id: char * (Default "")
///   server_id: char * (Default "")
/// <i>NOTE: The file is in data/conf.db and is expected that 'data' exists</i>.
void conf_init(void);

/// Returns the database.
Json *conf_get(void);

/// Sets the field 'lang' of data base.
void conf_set_lang(char *lang);

/// Sets the field 'menu' of data base.
void conf_set_menu(char *option);

/// Sets the nick identifier for edit page.
void conf_set_edit_id(char *id);

/// Set the nick identifier for issue page.
void conf_set_issue_id(char *id);

/// Set the server id selected for working.
void conf_set_server_id(char *id);

#endif
