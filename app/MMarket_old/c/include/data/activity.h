// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Control of application on-off.
/// Intially is set to 'on'. When user send the message 'end' is set to 'off'.

#ifndef DATA_ACTIVITY_H
  #define DATA_ACTIVITY_H

#include "dmc/async.h"

/// Activity estate: 1 -> on, 0 -> off.
int activity_active (void);

/// Finishes application
void activity_off (void);

#endif
