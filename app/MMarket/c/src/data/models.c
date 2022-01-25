// Copyright 16-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/models.h"
#include "dmc/str.h"
#include "dmc/err.h"
#include "data/models/Qfix.h"
#include "data/models/Qmob.h"
#include "data/models/Ma.h"
#include "data/models/MM.h"
#include "data/models/Ea.h"
#include "data/models/Ea2.h"
#include "data/models/Appr.h"

AModel *models_list (void) {
  return aModel_new_from(
    qfix_new(),
    qmob_new(),
    ma_new(),
    ea_new(),
    ea2_new(),
    mm_new(),
    appr_new(),
    NULL
  );
}

Achar *models_ids_list (void) {
  /**/void *fmap (Model *md) { return md->id; }
  return (Achar *)aModel_map(models_list(), fmap);
}

Model *models_get(char *id) {
  /**/int ffind (Model *md) { return str_eq(md->id, id); }
  Model *r = oModel_nsome(aModel_find(models_list(), ffind));
  if (!r) FAIL (str_f("Model '%s' not found", id));
  return r;
}
