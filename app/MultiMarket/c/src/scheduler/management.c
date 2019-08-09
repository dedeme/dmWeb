// Copyright 12-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "scheduler/management.h"
#include "io/managerdb.h"
#include "io/nicks.h"
#include "io/quotes.h"
#include "data/Rs.h"

static RsHistoric *historic (char *nick, ModelParams *mps) {
  // Arr[char]
  Arr *dates = arr_new();
  Darr *opens = darr_new();
  Darr *closes = darr_new();
  // Arr[Quote]
  Arr *quotes = quotes_read(nick);
  arr_reverse(quotes);

  EACH(quotes, Quote, q)
    arr_push(dates, quote_date(q));
    darr_push(opens, quote_open(q));
    darr_push(closes, quote_close(q));
  _EACH

  return model_historic(
    modelParams_model(mps),
    modelParams_params(mps),
    dates, opens, closes
  );
}

void management_update (AsyncActor *ac) {
  void fn () {
    EACH(nicks_list(), Nick, nk)
      char *nick = nick_name(nk);
      if (nick_is_sel(nk)) {
        ModelParams *mps = managerdb_nick(nick);
        ModelParams *df = managerdb_default();
        if (!modelParams_eq(mps, df)) {
          int nick_stocks = rsHistoric_stocks(historic(nick, mps));
          int default_stocks = rsHistoric_stocks(historic(nick, df));
          if (
            (!nick_stocks && !default_stocks) ||
            (nick_stocks && default_stocks)
          ) {
            managerdb_set_nick_default(nick);
          }
        }
      }
    _EACH
  }
  asyncActor_wait(ac, fn);
}
