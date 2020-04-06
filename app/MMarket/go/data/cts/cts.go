// Copyright 04-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Application constants
package cts

// Application -----------------------------------------------------------------

// Application name
const APP_NAME = "MMarket"

// Data version
const DATA_VERSION = "MMarket\nData version: 202004\n"

// Data directory relative to sys_home()
const DATA_PATH = "data"

// Fleas directory relative to sys_home()
const FLEAS_PATH = "data/fleas"

// Ranking directory relative to sys_home()
const RANKING_PATH = "data/rank"

// Accounting directory relative to sys_home()
const ACC_PATH = "data/acc"

// Log maximum entries
const LOG_MAX_ENTRIES = 1000

// Server ----------------------------------------------------------------------

// Communications port.
const PORT = "50204"

// Time (seconds) of connection expiration
const EXPIRATION = 900

// scheduler -------------------------------------------------------------------

/// Time to scheduler sleep (milliseconds)
const SCHEDULER_SLEEP = 2000

/// Tics of SCHEDULER SLEEP
const SCHEDULER_TIMES = 60 // 2 minutes

/// External servers (Infobolsa, Invertia, etc.) delay in SECONDS
const SERVERS_DELAY = 1200

/// Activity state
const ACT_SLEEPING1 = "Sleeping (1)"

/// Activity state
const ACT_HISTORIC = "Historic"

/// Activity state
const ACT_SLEEPING2 = "Sleeping (2)"

/// Activity state
const ACT_ACTIVATING = "Activating"

/// Activity state
const ACT_ACTIVE = "Active"

/// Activity state
const ACT_DEACTIVATING = "Deactivating"

/// Hour to start ACT_HISTORIC fase
const ACT_HISTORIC_START = 3

/// Hour to finish ACT_HISTORIC fase
const ACT_HISTORIC_END = 8
