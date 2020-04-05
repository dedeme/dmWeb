// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// HTTP readings.

#ifndef NET_NET_H
  #define NET_NET_H

#include "dmc/async.h"
#include "data/Server.h"
#include "data/Emsg.h"

/// Returns Opt[Arr[NickClose]]<p>
/// Arr[NickClose] can not contain some nick. In such case an error annotation
/// will be done in Log.
Opt *net_server_daily_read (Server *this);


/// Reads a daily conf returning an Opt[Arr[DailyEntry]].<p>
/// Fails are written in Log.
///   conf: Daily configuration
Opt *net_read_daily (ServerConf *conf);

/// Returns Opt[Arr[Quote]]<p>
///   nick_id: Nick identifier
///   return: Opt[Arr[Quote]]. If Arr[quote] has less than 10 elements an
///           annotation is done in Log.
Opt *net_server_historic_read (Server *this, int nick_id);

/// Reads a historic conf returning an Opt[Arr[HistoricEntry]].<p>
/// Fails are written in Log.
///   conf: Historic configuration
///   code: Company code.
Opt *net_read_historic (ServerConf *conf, char *code);

/// Updates quotes of nk_id
///   return: EMsg with values:
///     MSG_OK, "" -> No error
///     MSG_ERROR, "nick" -> Nick not found
///     MSG_ERROR, "server" -> Wrong server data
///     MSG_ERROR, "net" -> Connection fail
///     MSG_ERROR, "server/quotes" -> Wrong server data and quotes corrected
///     MSG_ERROR, "net/quotes" -> Connection fail and quotes corrected
///     MSG_WARNING, "quotes" -> Quotes corrected
Emsg *net_update_historic(AsyncActor *ac, int nk_id);

/// Updates daily quotes. Possible errors are annotated in log.
void net_update_daily (AsyncActor *ac);

#endif
