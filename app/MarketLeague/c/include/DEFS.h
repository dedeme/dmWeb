// Copyright 27-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Global definitions.

#ifndef DEFS_H
  #define DEFS_H

#include "dmc/std.h"

/// Players per league.
#define PLAYERS_PER_LEAGUE 20

/// Players per league.
#define MATCHES_PER_ROUND PLAYERS_PER_LEAGUE + PLAYERS_PER_LEAGUE - 2

/// Directory with company quotes.
#define QUOTES_DIR "/home/deme/.dmCApp/MultiMarket/data/quotes/"

/// Data base of daily quotes.
///   Structure: Js Map
///     nick: string
///     quote: number
#define DAILY_QUOTES_DB "/home/deme/.dmCApp/MultiMarket/data/acc/quotes.db"

/// Nicks database.
///   Structure: Js Array
///     (0) next_id: number
///     (1) model_id: number
///     (2) cia_nicks: Array
///         (0) nick_id: number
///         (1) nick_name: string
///         (2) selected: boolean
#define NICKS_DB "/home/deme/.dmCApp/MultiMarket/data/nicks.db"

/// MultiMarket configuration.
///   * Its data is Js object { key: string, value: Js }
///   * Interesting value is 'key: "activity"' which values can be
///     "Sleeping (2)" and others.
#define MM_CONF_DB "/home/deme/.dmCApp/MultiMarket/data/conf.db"

/// Multimarket calendar.
///   Structure: Object
///     "general": Array
///       (0) open hour: number
///       (1) open minute: number
///       (2) close hour: number
///       (3) close minute: number
///     "holidays": Array<string> // With dates
///     "specialDays": Array<Array
///       (0) date: string
///       (1) open hour: number
///       (2) open minute: number
///       (3) close hour: number
///       (4) close minute: number >
#define CALENDAR_DB "/home/deme/.dmCApp/MultiMarket/data/calendar.db"
#endif
