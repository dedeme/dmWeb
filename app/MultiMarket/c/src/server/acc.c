// Copyright 23-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/acc.h"
#include "server/acc/acc__downloader.h"
#include "server/acc/acc__companies.h"
#include "server/acc/acc__balance.h"
#include "server/acc/acc__trading.h"
#include "server/acc/acc__profits.h"

char *acc_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(source, mrq)

  if (str_eq(source, "Downloader")) return acc__downloader_process(ac, mrq);
  if (str_eq(source, "Companies")) return acc__companies_process(ac, mrq);
  if (str_eq(source, "Balance")) return acc__balance_process(ac, mrq);
  if (str_eq(source, "Trading")) return acc__trading_process(ac, mrq);
  if (str_eq(source, "Profits")) return acc__profits_process(ac, mrq);

  EXC_ILLEGAL_ARGUMENT(
    "source",
    "Downloader | Companies | Balance | Trading | Profits",
    source
  )
  return NULL; // Unreachable
}
