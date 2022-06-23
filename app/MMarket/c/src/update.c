// Copyright 20-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "update.h"
#include "dmc/str.h"
#include "data/dateFns.h"
#include "external/quotesReader.h"
#include "db/quotesTb.h"
#include "db/evalsDb.h"
#include "data/models.h"
#include "data/eval/ModelEvals.h"
#include "db/simProfitsDb.h"
#include "data/simulation/SimProfitsData.h"


void update_run (void) {
  char *last_sunday = dateFns_last_sunday();
  Quotes *qs = quotesTb_read();
  if (str_greater(last_sunday, qs->date)) {
    qs = quotesReader_read();
    quotesTb_write(qs);

    AModel *mds = models_list();
    Model **pmds = mds->es;
    while (pmds < mds->end) {
      Model *md = *pmds++;
      ModelEvals *evs = evalsDb_read(md->id);
      AModelEval *ev = str_greater(last_sunday, evs->date)
        ? model_range_new_evaluation(md, qs, evs->evals)
        : model_range_replace_evaluation(md, qs, evs->evals)
      ;

      evalsDb_write(md->id, modelEvals_new(last_sunday, ev));
      SimProfitsData *pfsdt = simProfitsDb_read(md->id);
      ASimProfitsRow *pfs = str_greater(last_sunday, pfsdt->date)
        ? model_simulation_new(md, qs, pfsdt->rows)
        : model_simulation_replace(md, qs, pfsdt->rows)
      ;
      simProfitsDb_write(md->id, simProfitsData_new(last_sunday, pfs));
    }
  }
}

// NOTE:
// xxx_replace functions are intended for special situations.
// They only will be called if the condition
//    'if (str_greater(last_sunday, qs->date)) {'
// is deactivated.