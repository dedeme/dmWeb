// Copyright 12-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Mananger who matches nick-model.

#ifndef DATA_MANAGER_H
  #define DATA_MANAGER_H

#include "dmc/async.h"
#include "ModelParams.h"

/*--*/

///
///   Arguments:
///     model: char*
///     params: Darr
typedef struct Manager_ManagerEntry ManagerEntry;

///
ManagerEntry *managerEntry_new (char *model, Darr *params);

///
char *managerEntry_model (ManagerEntry *this);

///
Darr *managerEntry_params (ManagerEntry *this);

///
Js *managerEntry_to_js (ManagerEntry *this);

///
ManagerEntry *managerEntry_from_js (Js *js);

///
///   Arguments:
///     current: ManagerEntry
///     entries: Map-ManagerEntry
typedef struct Manager_Manager Manager;

///
Manager *manager_new (ManagerEntry *current, Map *entries);

/// Default ManagerEntry
ManagerEntry *manager_current (Manager *this);

///
void manager_set_current (Manager *this, ManagerEntry *value);

/// Map[ManagerEntry]. Keys are nicks.
Map *manager_entries (Manager *this);

///
Js *manager_to_js (Manager *this);

///
Manager *manager_from_js (Js *js);

/// Format to show parameters in javascript
///   Arguments:
///     prefix: char*
///     multiplicator: int
///     decimals: int
///     suffix: char*
typedef struct Manager_ManagerFormat ManagerFormat;

///
ManagerFormat *managerFormat_new (
  char *prefix,
  int multiplicator,
  int decimals,
  char *suffix
);

///
char *managerFormat_prefix (ManagerFormat *this);

///
int managerFormat_multiplicator (ManagerFormat *this);

///
int managerFormat_decimals (ManagerFormat *this);

///
char *managerFormat_suffix (ManagerFormat *this);

///
Js *managerFormat_to_js (ManagerFormat *this);

///
ManagerFormat *managerFormat_from_js (Js *js);

///
///   Arguments:
///     model: char*
///     params: Darr
///     param_cf: Arr-ModelMxMn
///     param_fmt: Arr-ManagerFormat
typedef struct Manager_ManagerEntry2 ManagerEntry2;

///
ManagerEntry2 *managerEntry2_new (
  char *model,
  Darr *params,
  Arr *param_cf,
  Arr *param_fmt
);

///
char *managerEntry2_model (ManagerEntry2 *this);

///
Darr *managerEntry2_params (ManagerEntry2 *this);

///
Arr *managerEntry2_param_cf (ManagerEntry2 *this);

///
Arr *managerEntry2_param_fmt (ManagerEntry2 *this);

///
Js *managerEntry2_to_js (ManagerEntry2 *this);

///
///   Arguments:
///     current: ManagerEntry2
///     entries: Map-ManagerEntry2
typedef struct Manager_Manager2 Manager2;

///
Manager2 *manager2_new (ManagerEntry2 *current, Map *entries);

/// Default ManagerEntry
ManagerEntry2 *manager2_current (Manager2 *this);

/// Map[ManagerEntry2]. Keys are nicks.
Map *manager2_entries (Manager2 *this);

///
Js *manager2_to_js (Manager2 *this);

/*--*/

#endif
