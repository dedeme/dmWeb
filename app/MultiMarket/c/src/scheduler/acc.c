// Copyright 29-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "scheduler/acc.h"
#include "dmc/date.h"
#include "data/Acc.h"
#include "io/accdb.h"
#include "io/log.h"

void acc_historic_profits (AsyncActor *ac) {
  void fn () {
    AccLedPf *rs = accLedPf_new(accdb_diary_read());
    EACH(accLedPf_errors(rs), char, e)
      log_error(e);
    _EACH

    accdb_pf_update(accLedPf_pf(rs));

    Darr *pfs = accLedPf_profits(rs);
    accdb_profits_add(
      date_to_str(date_add(date_now(), -1)),
      darr_get(pfs, 0),
      darr_get(pfs, 1),
      darr_get(pfs, 2)
    );
  }
  asyncActor_wait(ac, fn);
}
