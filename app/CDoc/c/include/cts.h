// Copyright 05-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Global constants.

#include <time.h>

#ifndef CTS_H
  #define CTS_H

/// Application name.
char *cts_app_name(void);

/// Application dir.
char *cts_app_dir(void);

/// Expiration time.
time_t cts_expiration(void);

#endif
