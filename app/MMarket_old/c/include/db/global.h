// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Global parameters

#ifndef DB_GLOBAL_H
  #define DB_GLOBAL_H

#include "dmc/async.h"

/// Initialization
void global_init (void);

/// Its value can be 'en' or 'es'
char *global_lang (void);

/// 'lang' must be 'en' or 'es'
void global_set_lang (char *lang);

/// Returns identifier of activity. It can be ACT_SLEEPING1, ACT_SLEEPING2,
/// ACT_ACTIVATING, ACT_ACTIVE or ACT_DEACTIVATING.
char *global_activity (void);

/// 'activity' must be ACT_SLEEPING1, ACT_SLEEPING2, ACT_ACTIVATING,
/// ACT_ACTIVE or ACT_DEACTIVATING.
void global_set_activity (char *activity);

/// Return the current time stamp
char *global_time_stamp (void);

/// Sets time stamp with the current time and returns it.
char *global_set_time_stamp (void);

/// Check if current time stamp is equals to 'ts'.
int global_check_time_stamp (char *ts);

#endif
