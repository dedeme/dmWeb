// Copyright 15-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/cts.h"
#include "dmc/str.h"
#include "dmc/path.h"

char *cts_app_name (void) {
  return "MMarket";
}

char *cts_data_dir (void) {
  return path_cat("/home/deme/.dmCApp", cts_app_name(), "data", NULL);
}

char *cts_multimarket_data_dir (void) {
  return "/home/deme/.dmGoApp/MultiMarket/data";
}

char *cts_data_version (void) {
  return str_cat(cts_app_name(), "\nData version: 202112\n", NULL);
}

int cts_expiration (void) {
  return 900;
}

double cts_initial_capital (void) {
  return 100000;
}

double cts_bet (void) {
  return 10000;
}

double cts_min_to_bet (void) {
  return 11000;
}

double cts_assets_ratio (void) {
  return 0.35;
}

double cts_max_assets_ratio (void) {
  return cts_initial_capital() * 3;
}

double cts_profits_avg_ratio (void) {
  return 0.65;
}

double cts_max_profits_avg_ratio (void) {
  return 3;
}

double cts_eval_weeks (void) {
  return 50;
}

int cts_eval_steps (void) {
  return 20;
}

int cts_env_steps (void) {
  return 5;
}
