// Copyright 04-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef CONF_H
  #define CONF_H

/// Basic data base.

#include "dmc/std.h"
#include "dmc/Json.h"

/// [conf_init] Creates a 'config' file if it does not exist and intializes it.
/// The file is a serialized json object with two fields:
///   lang: string (Default 'en')
///   menu: string (Default "settings")
/// <i>NOTE: The file is in data/conf.db and is expected that 'data' exists.
void conf_init(void);

/// [conf_get] returns the database. It has dos fields:
Json *conf_get(void);

/// [conf_set_lang] sets the field 'lang' of data base.
void conf_set_lang(char *lang);

/// [conf_set_menu] sets the field 'menu' of data base.
void conf_set_menu(char *option);

#endif
