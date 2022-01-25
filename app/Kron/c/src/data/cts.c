// Copyright 12-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/cts.h"
#include "dmc/str.h"

char *cts_app_name (void) {
  return "Kron";
}

char *cts_data_dir (void) {
  return "data";
}

char *cts_data_version (void) {
  return str_cat(cts_app_name(), "\nData version: 202112\n", NULL);
}

char *cts_port (void) {
  return "88088";
}

int cts_expiration (void) {
  return 900;
}

int cts_scheduler_sleep (void) {
  return 2000;
}

int cts_scheduler_times (void) {
  return 30;
}

int cts_server_sleep (void) {
  return 200;
}

char *cts_server_ok_msg (void) {
  return str_cat(cts_app_name(), " is running.", NULL);
}

char *cts_server_stop_msg (void) {
  return str_cat(cts_app_name(), " stop.", NULL);
}
