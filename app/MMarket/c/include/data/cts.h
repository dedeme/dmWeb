// Copyright 15-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Application constants.

#ifndef DATA_CTS_H
  #define DATA_CTS_H

// General ---------------------------------------------------------------------

/// Application name.
char *cts_app_name (void);

/// Path absolute of data directory
char *cts_data_dir (void);

/// Path absolute of QMarket data directory.
char *cts_ktmarket_data_dir (void);

/// String to test database.
char *cts_data_version (void);

/// Expiration time in seconds (900).
int cts_expiration (void);

// Bets ------------------------------------------------------------------------

/// Model initial capital for each cycle
double cts_initial_capital (void);

/// Bet
double cts_bet (void);

/// Minimun cash to bet
double cts_min_to_bet (void);

// Models evaluation ---------------------------------------------------------

/// Historic simulation ratio
double cts_assets_ratio (void);

/// Maximum assets to calculate 'cts_assets_ratio' (Currency)
double cts_max_assets_ratio (void);

/// Average of simulation profits ratio
double cts_profits_avg_ratio (void);

/// Maximum average to calculate 'cts_profits_avg_ratio' (ratio)
double cts_max_profits_avg_ratio (void);

/// Weeks movil average of historic evaluation.
double cts_eval_weeks (void);

/// Number of evaluations for parameter.
int cts_eval_steps (void);

/// Number of evaluations for environment (from '-value' to '+value')
int cts_env_steps (void);

#endif
