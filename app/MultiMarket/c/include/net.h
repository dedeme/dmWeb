// Copyright 24-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// HTTP readings.

#ifndef NET_H
  #define NET_H

#include "dmc/async.h"
#include "data/Rconf.h"
#include "data/EMsg.h"

/// Reads a daily conf returning an Opt[Arr[DailyEntry]].<p>
/// Fails are written in Log.
///   conf: Daily configuration
Opt *net_read_daily (Rconf *conf);

/// Reads a historic conf returning an Opt[Arr[HistoricEntry]].<p>
/// Fails are written in Log.
///   conf: Historic configuration
///   code: Company code.
Opt *net_read_historic (Rconf *conf, char *code);

/// Updates quotes of nk_id
///   return: EMsg with values:
///     MSG_OK, "" -> No error
///     MSG_ERROR, "nick" -> Nick not found
///     MSG_ERROR, "server" -> Wrong server data
///     MSG_ERROR, "net" -> Connection fail
///     MSG_ERROR, "server/quotes" -> Wrong server data and quotes corrected
///     MSG_ERROR, "net/quotes" -> Connection fail and quotes corrected
///     MSG_WARNING, "quotes" -> Quotes corrected
EMsg *net_update_historic(AsyncActor *ac, int nk_id);

/// Updates daily quotes. Possible errors are annotated in log.
void net_update_daily (AsyncActor *ac);

#endif
