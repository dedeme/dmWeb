// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Global definitions.

#ifndef DEFS_H
  #define DEFS_H

#include "dmc/async.h"

// Application -----------------------------------------------------------------

///
#define APP_NAME "MMarket"

/// Data version
#define DATA_VERSION "MultiMarket\nData version: 202001\n"

/// Data directory relative to sys_home()
#define DATA_PATH "data"

/// Data directory relative to sys_home()
#define FLEAS_PATH "data/fleas"

/// Data directory relative to sys_home()
#define RANKING_PATH "data/rank"

/// Data directory relative to sys_home()
#define ACC_PATH "data/acc"

/// Log maximum entries
#define LOG_MAX_ENTRIES 300

/// Time to server sleep (milliseconds)
#define ACTOR_SLEEP 10

/// Error messages
enum ErrorMsg { MSG_OK, MSG_WARNING, MSG_ERROR };

// Server ----------------------------------------------------------------------

/// Communications port
#define PORT 50204

/// Time of connection expiration
#define EXPIRATION 900

/// Time to server sleep (milliseconds)
#define SERVER_SLEEP 10

/// Time to server sleep (milliseconds) when reading errors
#define SERVER_ERROR_SLEEP 120000

/// Server configuration states
enum Server { SERVER_STOPPED, SERVER_ACTIVE, SERVER_SELECTED };

// scheduler -------------------------------------------------------------------

/// Time to scheduler sleep (milliseconds)
#define SCHEDULER_SLEEP 50

/// External servers (Infobolsa, Invertia, etc.) delay in SECONDS
#define SERVERS_DELAY 1200

/// Activity state
#define ACT_SLEEPING1 "Sleeping (1)"

/// Activity state
#define ACT_HISTORIC "Historic"

/// Activity state
#define ACT_SLEEPING2 "Sleeping (2)"

/// Activity state
#define ACT_ACTIVATING "Activating"

/// Activity state
#define ACT_ACTIVE "Active"

/// Activity state
#define ACT_DEACTIVATING "Deactivating"

/// Hour to start ACT_HISTORIC fase
#define ACT_HISTORIC_START 3

/// Hour to finish ACT_HISTORIC fase
#define ACT_HISTORIC_END 8

// Fleas -----------------------------------------------------------------------

/// Number of quotes in historic
#define HISTORIC_QUOTES 610

/// Fleas initial capital for each cycle
#define INITIAL_CAPITAL 150000

/// Bet
#define BET 15000

/// Minimun cash to bet
#define MIN_TO_BET 16000

/// Minimum operations to survive (multiplicator: days * minSells)
#define MIN_SELLS 0.05

/// Maximun operations to survive (multiplicator: days * maxSells)
#define MAX_SELLS 0.1

/// Maximum of mutation
#define MUTATION_MULTIPLIER 0.3

/// Number of cycle per parameter after insertion to finish a process
#define CYCLES 5

/// Number of cycle to insert historic results
#define INSERTION_CYCLE 10

/// Number of fleas per model
#define FLEAS_PER_MODEL 2000

// Fleas evaluation ------------------------------------------------------------

/// Historic simulation ratio
#define ASSETS_RATIO 0.375

/// Average of profits ratio
#define PROFITS_RATIO 0.375

/// Flea age ratio
#define AGE_RATIO 0.25

/// Number of fleas in pool
#define POOL_NUMBER 1000

/// Number of fleas in ranking
#define RANKING_NUMBER 40

/// Number of fleas that changes per session
#define RANKING_CHANGES 4

/// Number of fleas in ranking
#define HISTORIC_RANKING_ENTRIES 10

// Not assigned ----------------------------------------------------------------

/// Minimal number of companies in a set
#define SET_COMPANIES 10

/// Number of quotes to calculate volume
#define VOLUME_QUOTES 100

/// Maximun number of fleas in "_best"
#define MAXIMUM_HISTORIC_BESTS 252

/// Number of daily results in data base 'data/fleas/MODEL'
#define FLEA_MODEL_DATES 10

/// Avg days of champions
#define CHAMPIONS_AVG 10

/// Number total of champion fleas per group
#define TOTAL_CHAMPIONS 500

/// Quotes number in account charts
#define ACC_CHART_QUOTES 250

/// Ponderation for raking
#define RANKING_ASSETS_RATIO 0.45

/// Ponderation for raking
#define RANKING_PROFITS_RATIO 0.35

/// Ponderation for raking
#define RANKING_AGE_RATIO 0.2

/// Number of fleas in ranking
#define HISTORIC_RANKING_ENTRIES 10

/// Maximum number of data in ranking charts
#define HISTORIC_RANKING_CHAR_MAX 450

/// Server short name to get url in accounting charts
#define ACC_URL "INFOB"

/// External command to download
#define WGET "Wget"

/// External command to download
#define PUPPETEER "Puppeteer"

#endif
