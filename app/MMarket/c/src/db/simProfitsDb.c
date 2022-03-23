// Copyright 28-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db/simProfitsDb.h"
#include "dmc/path.h"
#include "dmc/file.h"
#include "dmc/str.h"
#include "data/dateFns.h"
#include "data/models.h"
#include "data/simulation/SimProfitsData.h"
#include "db/quotesTb.h"

static char *dir = NULL;

static char *table (char *model_id) {
  return path_cat(dir, str_cat(model_id, ".tb", NULL), NULL);
}

void simProfitsDb_init (char *parent) {
  dir = path_cat(parent, "simulation", NULL);
  if (!file_exists(dir)) {
    file_mkdir(dir);
  }

  Achar *mds = models_ids_list();
  char **pmds = mds->es;
  while (pmds < mds->end) {
    char *id = *pmds++;
    if (!file_exists(table(id))) {
      simProfitsDb_write(id, simProfitsData_new(
        dateFns_last_sunday(),
        model_simulation_new(
          models_get(id),
          quotesTb_read(),
          aSimProfitsRow_new()
      )));
    }
  }
}

SimProfitsData *simProfitsDb_read(char *model_id) {
  return simProfitsData_from_js(file_read(table(model_id)));
}

void simProfitsDb_write(char *model_id, SimProfitsData *data) {
  file_write(table(model_id), simProfitsData_to_js(data));
}
