// Copyright 17-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db/evalsDb.h"
#include "dmc/file.h"
#include "dmc/path.h"
#include "dmc/str.h"
#include "data/models.h"
#include "data/dateFns.h"
#include "db/quotesTb.h"

static char *dir = NULL;

static char *table (char *model_id) {
  return path_cat(dir, str_cat(model_id, ".tb", NULL), NULL);
}

void evalsDb_init (char *parent) {
  dir = path_cat(parent, "evals", NULL);
  if (!file_exists(dir)) {
    file_mkdir(dir);
  }

  Achar *mds = models_ids_list();
  char **pmds = mds->es;
  while (pmds < mds->end) {
    char *id = *pmds++;
    if (!file_exists(table(id))) {
      evalsDb_write(id, modelEvals_new(
        dateFns_last_sunday(),
        model_range_new_evaluation(
          models_get(id),
          quotesTb_read(),
          aModelEval_new()
      )));
    }
  }
}

ModelEvals *evalsDb_read(char *model_id) {
  return modelEvals_from_js(file_read(table(model_id)));
}

void evalsDb_write(char *model_id, ModelEvals *evals) {
  file_write(table(model_id), modelEvals_to_js(evals));
}
