// Copyright 06-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/nicks.h"
#include "io.h"
#include "io/quotes.h"
#include "io/servers.h"

char *nicks_db = NULL;

/* .
-Nicks: serial
  -next_id: int
  -model: int
  -list: Arr - Nick
*/

/*--*/

struct nicks_Nicks{
  int next_id;
  int model;
  Arr *list;
};

static Nicks *_nicks_new(int next_id, int model, Arr *list) {
  Nicks *this = MALLOC(Nicks);
  this->next_id = next_id;
  this->model = model;
  this->list = list;
  return this;
}

Js *nicks_to_js(Nicks *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_wi((int)this->next_id));
  arr_push(js, js_wi((int)this->model));
  arr_push(js, arr_to_js(this->list, (FTO)nick_to_js));
  return js_wa(js);
}

Nicks *nicks_from_js(Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  Nicks *this = MALLOC(Nicks);
  this->next_id = js_ri(*p++);
  this->model = js_ri(*p++);
  this->list = arr_from_js(*p++, (FFROM)nick_from_js);
  return this;
}

/*--*/

static char *fnicks (void) {
  if (nicks_db) return nicks_db;

  EXC_IO("'nicks.db' was not initialized");
  return NULL; // Unreachable
}

static Nicks *read (void) {
  return nicks_from_js((Js *)file_read(fnicks()));
}

static void write (Nicks *this) {
  file_write(fnicks(), (char *)nicks_to_js(this));
}

static int is_duplicate (Nicks *this, char *nk_name) {
  EACH(this->list, Nick, nk)
    if (str_eq(nick_name(nk), nk_name)) return 1;
  _EACH
  return 0;
}

void nicks_init (void) {
  nicks_db = path_cat(io_data_dir(), "nicks.db", NULL);
  if (!file_exists(nicks_db)) {
    Nicks *db = _nicks_new(0, -1, arr_new());
    write(db);
  }
}

int nicks_model (void) {
  return read()->model;
}

// Arr[Nick]
Arr *nicks_list (void) {
  return read()->list;
}

int nicks_add(char *nk_name) {
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

void nicks_del(int nk_id) {
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

// Opt[Nick]
Opt *nicks_get(int nk_id) {
  Nicks *db = read();
  EACH(db->list, Nick, nk)
    if (nick_id(nk) == nk_id) {
      return opt_new(nk);
    }
  _EACH
  return opt_empty();
}

int nicks_modify(Nick *nick) {
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

void nicks_set_model(int nk_id) {
  Nicks *db = read();
  db->model = nk_id;
  write(db);
}

void nicks_set_selected(int nk_id, int value) {
  Nicks *db = read();
  EACH(db->list, Nick, nk)
    if (nick_id(nk) == nk_id) {
      nick_set_is_sel(nk, value);
      break;
    }
  _EACH
  write(db);
}
