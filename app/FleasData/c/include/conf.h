// Copyright 04-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef CONF_H
  #define CONF_H

/// Basic data base.

#include "dmc/std.h"

/// [conf_init] Creates a 'config' file if it does not exist and intializes it.
/// The file is a serialized json object with two fields:
///   lang: string (Default 'en')
///   menu: string (Default "settings")
/// <i>NOTE: The file is in data/conf.db and is expected that 'data' exists.
void conf_init(void);

/// [conf_end] Frees conf resources
void conf_end(void);

/// [conf_get] returns the database.
Js *conf_get_new(void);

/// [conf_set] sets 'field' with 'value'
void conf_set(const char *field, const char *value);

/// [conf_set_lang] sets the field 'lang' of data base.
void conf_set_lang(const char *lang);

/// [conf_set_tmenu] sets option in top menu.
void conf_set_tmenu(const char *option);

/// [conf_set_lmenu] sets option in left menu.
void conf_set_lmenu(const char *option);

#endif
