// Copyright 18-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Configuration data.

#ifndef DATA_CONF_H
  #define DATA_CONF_H

#include "dmc/std.h"

/*--*/

/// Configuration data.
///   Arguments:
///     path: char*
///     lang: char*
///     show_all: bool
typedef struct Conf_Conf Conf;

///
Conf *conf_new (char *path, char *lang, int show_all);

///
char *conf_path (Conf *this);

///
char *conf_lang (Conf *this);

///
int conf_show_all (Conf *this);

///
Js *conf_to_js (Conf *this);

///
Conf *conf_from_js (Js *js);

/*--*/

#endif
