// Copyright 24-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Global constants.

#ifndef DATA_CTS_H
  #define DATA_CTS_H

#include "dmc/char/Mchar.h"

///
enum cts_BET_TYPE { cts_BET_1, cts_BET_x, cts_BET_2 };

/// Application name.
char *cts_app_name (void);

/// Data directory relative to sys_home()
char *cts_data_dir (void);

/// Text for versión control
char *cts_data_version (void);

/// Web directory.
char *cts_web_app_dir(void);

/// Expiration time.
int cts_expiration (void);

// Year initialization ---------------------------------------------------------

/// Current league year.
char *cts_year(void);

/// Current league teams.
Mchar *cts_teams(void);

/// Marca teams index. Its order is equal to 'cts_team' order.
Achar *cts_marca_teams(void);

/// As teams index. Its order is equal to 'cts_team' order.
Achar *cts_as_teams(void);

/// Sportium teams index. Its order is equal to 'cts_team' order.
Achar *cts_sportium_teams(void);

#endif
