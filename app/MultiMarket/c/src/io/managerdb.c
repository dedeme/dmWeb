// Copyright 12-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/managerdb.h"
#include "io/io.h"
#include "data/dfleas/dfleas__models.h"

static char *managerdb = NULL;

void managerdb_init (void) {
  managerdb = path_cat(io_data_dir(), "manager.db", NULL);
  if (!file_exists(managerdb)) {
    ModelParams *mps = dfleas__models_default();
    managerdb_write(manager_new(
      managerEntry_new(
        model_name(modelParams_model(mps)), modelParams_params(mps)
      ),
      map_new()
    ));
  }
}

Manager *managerdb_read (void) {
  if (!managerdb) EXC_ILLEGAL_STATE("'managerdb' was not intiliazed");

  return manager_from_js((Js *)file_read(managerdb));
}

void managerdb_write (Manager *mg) {
  if (!managerdb) EXC_ILLEGAL_STATE("'managerdb' was not intiliazed");

  file_write(managerdb, (char *)manager_to_js(mg));
}

ModelParams *managerdb_default (void) {
  ManagerEntry *e = manager_current(managerdb_read());
  Model *md = opt_nget(dfleas__models_get(managerEntry_model(e)));
  if (md) {
    return modelParams_new(md, managerEntry_params(e));
  }
  return dfleas__models_default();
}

ModelParams *managerdb_nick (char *nick) {
  Manager *mg = managerdb_read();
  ManagerEntry *e = opt_nget(map_get(manager_entries(mg), nick));
  if (e) {
    Model *md = opt_nget(dfleas__models_get(managerEntry_model(e)));
    if (md) {
      return modelParams_new(md, managerEntry_params(e));
    }
  }
  managerdb_set_nick_default(nick);
  return managerdb_default();
}

void managerdb_set_nick_default (char *nick) {
  Manager *mg = managerdb_read();
  map_put(manager_entries(mg), nick, manager_current(mg));
  managerdb_write(mg);
}

int managerdb_set_nick (char *nick, char *model, Darr *params) {
  Model *md = opt_nget(dfleas__models_get(model));
  if (md && arr_size(model_param_cf(md)) == darr_size(params)) {
    Manager *mg = managerdb_read();
    if (*nick) {
      map_put(manager_entries(mg), nick, managerEntry_new(model, params));
    } else {
      manager_set_current(mg, managerEntry_new(model, params));
    }
    managerdb_write(mg);
    return 0;
  }
  return 1;
}
