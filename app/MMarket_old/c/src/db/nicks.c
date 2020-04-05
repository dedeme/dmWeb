// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db/nicks.h"
#include "db/quotes.h"
#include "db/servers.h"
#include "DEFS.h"

static char *path () {
  return path_cat(sys_home(), DATA_PATH, "Nicks.db", NULL);
}

struct nicks_Nicks {
  int next_id;
  int model; // id of nick model. If it does not exist its value is -1
  Arr *list; // Arr[Nick]
};

typedef struct nicks_Nicks Nicks;

// list is Arr[Nick]
static Nicks *nicks_new(int next_id, int model, Arr *list) {
  Nicks *this = MALLOC(Nicks);
  this->next_id = next_id;
  this->model = model;
  this->list = list;
  return this;
}

static Js *to_js (Nicks *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_wi(this->next_id));
  arr_push(js, js_wi(this->model));
  arr_push(js, arr_to_js(this->list, (FTO)nick_to_js));
  return js_wa(js);
}

static Nicks *from_js(Js * js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  Nicks *this = MALLOC(Nicks);
  this->next_id = js_ri(*p++);
  this->model = js_ri(*p++);
  this->list = arr_from_js(*p++, (FFROM)nick_from_js);
  return this;
}

static void write (Nicks *nicks) {
  file_write(path(), (char *)to_js(nicks));
}

static Nicks *read (void) {
  return from_js((Js *)file_read(path()));
}

static int is_duplicate (Nicks *this, char *nk_name) {
  EACH(this->list, Nick, nk)
    if (str_eq(nick_name(nk), nk_name)) return 1;
  _EACH
  return 0;
}

void nicks_init (void) {
  if (!file_exists(path())) {
    write(nicks_new(0, -1, arr_new()));
  }
}

/// Returns id of nick model or -1 if it has not been set.
int nicks_model (void) {
  return read()->model;
}

/// Sets nick model
void nicks_set_model(int nk_id) {
  Nicks *db = read();
  db->model = nk_id;
  write(db);
}

/// Arr[Nick]
Arr *nicks_list (void) {
  return read()->list;
}

/// Adds a nick if it is not duplicated and returns 1. Otherwise returns 0.
int nicks_add (char *nk_name) {
  Nicks *db = read();
  if (is_duplicate(db, nk_name)) return 0;
  int id = db->next_id;
  arr_push(db->list, nick_new(id, nk_name));
  db->next_id = id + 1;
  write(db);
  quotes_add(nk_name);
  servers_add_nick(id);
  return 1;
}

/// Removes nick with id 'id' if it exists
void nicks_del (int nk_id) {
  Nicks *db = read();
  char *name = "";
  int i = -1;
  EACH_IX(db->list, Nick, nk, ix)
    if (nick_id(nk) == nk_id) {
      i = ix;
      name = nick_name(nk);
    }
  _EACH
  if (i != -1) {
    quotes_del(name);
    servers_del_nick(nk_id);
    arr_remove(db->list, i);
    write(db);
  }
}

/// Returns Opt[Nick] with the nick which id is 'id'. If it does not exist
/// returns 'opt_empty()'
Opt *nicks_get (int nk_id) {
  Nicks *db = read();
  EACH(db->list, Nick, nk)
    if (nick_id(nk) == nk_id) {
      return opt_new(nk);
    }
  _EACH
  return opt_empty();
}

/// Modifies nick if 'nick_name(nick)' is not duplicated and returns 1.
/// Otherwise returns 0
int nicks_modify (Nick *nick) {
  Nicks *db = read();
  int nk_id = nick_id(nick);
  char *new_name = nick_name(nick);
  char *old_name = "";
  int i = -1;
  EACH_IX(db->list, Nick, nk, ix)
    if (nick_id(nk) == nk_id) {
      i = ix;
      old_name = nick_name(nk);
    }
  _EACH
  if (i != -1) {
    if (!str_eq(old_name, new_name)) {
      EACH(db->list, Nick, nk)
        if (str_eq(nick_name(nk), new_name)) return 0;
      _EACH
      quotes_modify(old_name, new_name);
    }
    arr_set(db->list, i, nick);
    write(db);
  }
  return 1;
}

/// Set the nick with 'nick_id' selected on/off. If nick_id does not exist,
/// it does nothing
///   value: is 0 or 1.
void nicks_set_selected (int nk_id, int value) {
  Nicks *db = read();
  EACH(db->list, Nick, nk)
    if (nick_id(nk) == nk_id) {
      nick_set_is_sel(nk, value);
      break;
    }
  _EACH
  write(db);
}

/// Returns Arr[Nick] with selected list.
Arr *nicks_selected_list (void) {
  int fn (Nick *nick) { return nick_is_sel(nick); }
  return arr_filter_to(read()->list, (FPRED)fn);
}
