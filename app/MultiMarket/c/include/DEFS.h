// Copyright 03-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Global definitions.

#ifndef DEFS_H
  #define DEFS_H

#include "dmc/async.h"

///
#define APP_NAME "MultiMarket"

/// Log maximum entries
#define LOG_MAX_ENTRIES 300

/// Communications port
#define PORT 50286

/// Time to server sleep (milliseconds)
#define ACTOR_SLEEP 10

/// Time to server sleep (milliseconds)
#define SERVER_SLEEP 10

/// Time to server sleep (milliseconds) when reading errors
#define SERVER_ERROR_SLEEP 120000

/// Time to scheduler sleep (milliseconds)
#define SCHEDULER_SLEEP 50

/// Time of connection expiration
#define EXPIRATION 900

/// Data version
#define DATA_VERSION "MultiMarket\nData version: 201905\n"

/// Number of quotes in historic
#define HISTORIC_QUOTES 610

/// Minimal number of companies in a set
#define SET_COMPANIES 10

/// Number of quotes to calculate volume
#define VOLUME_QUOTES 100

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

/// Servers delay in SECONDS
#define SERVERS_DELAY 1200

/// Maximum of mutation
#define MUTATION_MULTIPLIER 0.3

/// Number of cycle after insertion to finish a process for parameter
#define CYCLES 5

/// Number of cycle to insert historic results
#define INSERTION_CYCLE 10

/// Number of fleas per model
#define FLEAS_PER_MODEL 2000

/// Fleas initial capital for each cycle
#define INITIAL_CAPITAL 150000

/// Bet
#define BET 15000

/// Minimun cash to bet
#define MIN_TO_BET 16000

/// Minimum operations to survive (divisor: days / minSells)
#define MIN_SELLS 30

/// Maximun operations to survive (divisor: days / maxSells)
#define MAX_SELLS 15

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

/// Maximum number of data in ranking charts
#define MAXIMUM_HISTORIC_RANKING 450

/// Server short name to get url in accounting charts
#define ACC_URL "INFOB"

/// Error messages
enum ErrorMsg { MSG_OK, MSG_WARNING, MSG_ERROR };

/// Server configuration states
enum Server { SERVER_STOPPED, SERVER_ACTIVE, SERVER_SELECTED };

#endif
