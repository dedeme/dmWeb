// Copyright 12-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Manager.h"
#include "data/ModelMxMn.h"

/* .
ManagerEntry: serial
  model: char *
  params: Darr
===
Manager: serial
  # Default ManagerEntry
  @current: ManagerEntry
  # Map[ManagerEntry]. Keys are nicks.
  entries: Map - ManagerEntry
===
# Format to show parameters in javascript
ManagerFormat: serial
  prefix: char *
  multiplicator: int
  decimals: int
  suffix: char *
===
ManagerEntry2: to
  model: char *
  params: Darr
  param_cf: Arr - ModelMxMn
  param_fmt: Arr - ManagerFormat
===
Manager2: to
  # Default ManagerEntry
  current: ManagerEntry2
  # Map[ManagerEntry2]. Keys are nicks.
  entries: Map - ManagerEntry2
*/

/*--*/

struct Manager_ManagerEntry {
  char *model;
  Darr *params;
};

ManagerEntry *managerEntry_new (char *model, Darr *params) {
  ManagerEntry *this = MALLOC(ManagerEntry);
  this->model = model;
  this->params = params;
  return this;
}

char *managerEntry_model (ManagerEntry *this) {
  return this->model;
}

Darr *managerEntry_params (ManagerEntry *this) {
  return this->params;
}

Js *managerEntry_to_js (ManagerEntry *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->model));
  arr_push(js, darr_to_js(this->params));
  return js_wa(js);
}

ManagerEntry *managerEntry_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  ManagerEntry *this = MALLOC(ManagerEntry);
  this->model = js_rs(*p++);
  this->params = darr_from_js(*p++);
  return this;
}

struct Manager_Manager {
  ManagerEntry *current;
  Map *entries;
};

Manager *manager_new (ManagerEntry *current, Map *entries) {
  Manager *this = MALLOC(Manager);
  this->current = current;
  this->entries = entries;
  return this;
}

ManagerEntry *manager_current (Manager *this) {
  return this->current;
}

void manager_set_current (Manager *this, ManagerEntry *value) {
  this->current = value;
}

Map *manager_entries (Manager *this) {
  return this->entries;
}

Js *manager_to_js (Manager *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, managerEntry_to_js(this->current));
  arr_push(js, map_to_js(this->entries, (FTO)managerEntry_to_js));
  return js_wa(js);
}

Manager *manager_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  Manager *this = MALLOC(Manager);
  this->current = managerEntry_from_js(*p++);
  this->entries = map_from_js(*p++, (FFROM)managerEntry_from_js);
  return this;
}

struct Manager_ManagerFormat {
  char *prefix;
  int multiplicator;
  int decimals;
  char *suffix;
};

ManagerFormat *managerFormat_new (
  char *prefix,
  int multiplicator,
  int decimals,
  char *suffix
) {
  ManagerFormat *this = MALLOC(ManagerFormat);
  this->prefix = prefix;
  this->multiplicator = multiplicator;
  this->decimals = decimals;
  this->suffix = suffix;
  return this;
}

char *managerFormat_prefix (ManagerFormat *this) {
  return this->prefix;
}

int managerFormat_multiplicator (ManagerFormat *this) {
  return this->multiplicator;
}

int managerFormat_decimals (ManagerFormat *this) {
  return this->decimals;
}

char *managerFormat_suffix (ManagerFormat *this) {
  return this->suffix;
}

Js *managerFormat_to_js (ManagerFormat *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->prefix));
  arr_push(js, js_wi((int)this->multiplicator));
  arr_push(js, js_wi((int)this->decimals));
  arr_push(js, js_ws(this->suffix));
  return js_wa(js);
}

ManagerFormat *managerFormat_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  ManagerFormat *this = MALLOC(ManagerFormat);
  this->prefix = js_rs(*p++);
  this->multiplicator = js_ri(*p++);
  this->decimals = js_ri(*p++);
  this->suffix = js_rs(*p++);
  return this;
}

struct Manager_ManagerEntry2 {
  char *model;
  Darr *params;
  Arr *param_cf;
  Arr *param_fmt;
};

ManagerEntry2 *managerEntry2_new (
  char *model,
  Darr *params,
  Arr *param_cf,
  Arr *param_fmt
) {
  ManagerEntry2 *this = MALLOC(ManagerEntry2);
  this->model = model;
  this->params = params;
  this->param_cf = param_cf;
  this->param_fmt = param_fmt;
  return this;
}

char *managerEntry2_model (ManagerEntry2 *this) {
  return this->model;
}

Darr *managerEntry2_params (ManagerEntry2 *this) {
  return this->params;
}

Arr *managerEntry2_param_cf (ManagerEntry2 *this) {
  return this->param_cf;
}

Arr *managerEntry2_param_fmt (ManagerEntry2 *this) {
  return this->param_fmt;
}

Js *managerEntry2_to_js (ManagerEntry2 *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->model));
  arr_push(js, darr_to_js(this->params));
  arr_push(js, arr_to_js(this->param_cf, (FTO)modelMxMn_to_js));
  arr_push(js, arr_to_js(this->param_fmt, (FTO)managerFormat_to_js));
  return js_wa(js);
}

struct Manager_Manager2 {
  ManagerEntry2 *current;
  Map *entries;
};

Manager2 *manager2_new (ManagerEntry2 *current, Map *entries) {
  Manager2 *this = MALLOC(Manager2);
  this->current = current;
  this->entries = entries;
  return this;
}

ManagerEntry2 *manager2_current (Manager2 *this) {
  return this->current;
}

Map *manager2_entries (Manager2 *this) {
  return this->entries;
}

Js *manager2_to_js (Manager2 *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, managerEntry2_to_js(this->current));
  arr_push(js, map_to_js(this->entries, (FTO)managerEntry2_to_js));
  return js_wa(js);
}

/*--*/

