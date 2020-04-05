// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/activity.h"

static int activity_switch = 1;

int activity_active (void) {
  return activity_switch;
}

/// Finishes application
void activity_off (void) {
  activity_switch = 0;
}
