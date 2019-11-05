// Copyright 26-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Configuration data.
///   Structure:
///     lang: char *. Application language ('en' or 'es')

#ifndef IO_CONF_H
  #define IO_CONF_H

#include "dmc/std.h"

/// Returns application language.
char *conf_lang (void);

/// Sets application language.
void conf_set_lang(char *lang);

#endif
