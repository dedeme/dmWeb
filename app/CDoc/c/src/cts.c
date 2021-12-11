// Copyright 05-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "cts.h"
#include "dmc/str.h"

char *cts_app_name(void) {
  return "CDoc";
}

char *cts_app_dir(void) {
  return str_cat("dmcgi/", cts_app_name(), NULL);
}

/// Expiration time.
time_t cts_expiration(void) {
  return 900;
}
