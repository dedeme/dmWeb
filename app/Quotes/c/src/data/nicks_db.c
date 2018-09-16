// Copyright 11-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/nicks_db.h"
#include "data/db.h"
#include "dmc/ct/Ochar.h"

/* .+.
struct: @Nick_db
  -next_id: int: _int
  -model: char *: _string
  -nicks: Anick *: _array nick
*/
/*.-.*/
#include "dmc/ct/Ajson.h"

struct nick_db_Nick_db {
  int next_id;
  char *model;
  Anick *nicks;
};

Nick_db *_nick_db_new(int next_id, char *model, Anick *nicks) {
  Nick_db *this = MALLOC(Nick_db);
  this->next_id = next_id;
  XNULL(model)
  this->model = model;
  XNULL(nicks)
  this->nicks = nicks;
  return this;
}

Json *nick_db_to_json(Nick_db *this) {
  XNULL(this)
  Ajson *serial = ajson_new();
  jarr_aint(serial, this->next_id);
  jarr_astring(serial, this->model);
  jarr_aarray(serial, (Arr *)this->nicks, (Json*(*)(void*)) nick_to_json);
  return json_warray(serial);
}

Nick_db *nick_db_from_json(Json *js) {
  XNULL(js)
  Ajson *serial = json_rarray(js);
  Nick_db *this = MALLOC(Nick_db);
  size_t i = 0;
  this->next_id = jarr_gint(serial, i++);
  this->model = jarr_gstring(serial, i++);
  this->nicks = (Anick *)jarr_garray(serial, i++, (void*(*)(Json*)) nick_from_json);
  return this;
}
/*.-.*/

static Nick_db *nicks_db = NULL;

static char *path(void) {
  return path_cat(db_dir(), "nicks.db", NULL);
}

static void write(void) {
  file_write(path(), (char *)nick_db_to_json(nicks_db));
}

static Nick_db *read(void) {
  if (!nicks_db) {
    char *p = path();
    if (file_exists(p)) {
      nicks_db = nick_db_from_json((Json *)file_read(p));
    } else {
      nicks_db = _nick_db_new(0, "", anick_new());
    }
  }
  return nicks_db;
}

char *nicks_db_model(void) {
  Nick_db *db = read();
  return db->model;
}

Anick *nicks_db_list(void) {
  Nick_db *db = read();
  return db->nicks;
}

Ochar *nicks_db_name(char *id) {
  EACH(nicks_db_list(), Nick, n) {
    if (str_eq(id, nick_id(n))) {
      return ochar_new(nick_name(n));
    }
  }_EACH
  return ochar_null();
}

bool nicks_db_add(char *name, bool is_ibex, bool is_sel) {
  Nick_db *db = read();
  Anick *nicks = db->nicks;
  if (anick_size(nicks)) {
    EACH(nicks, Nick, n) {
      if (str_eq(nick_name(n), name)) {
        return false;
      }
    }_EACH
    anick_add(nicks, nick_new(
      str_printf("%d", db->next_id), name, is_ibex, is_sel
    ));
  } else {
    anick_add(nicks, nick_new("0", name, is_ibex, is_sel));
    db->next_id = 0;
    db->model = "0";
  }
  ++db->next_id;

  write();
  file_write(path_cat(db_dir(), "quotes", name, NULL), "");
  return true;
}

void nicks_db_remove(char *id) {
  Nick_db *db = read();
  Anick *nicks = anick_new();
  char *name = NULL;
  EACH(db->nicks, Nick, n) {
    if (str_eq(nick_id(n), id)) {
      name = nick_name(n);
    } else {
      anick_add(nicks, n);
    }
  }_EACH
  if (name) {
    db->nicks = nicks;
    if (str_eq(db->model, id)) {
      if (anick_size(nicks)) {
        db->model = nick_id(anick_get(nicks, 0));
      } else {
        db->model = "";
      }
    }
    write();
    file_del(path_cat(db_dir(), "quotes", name, NULL));
  }
}

void nicks_db_set_ibex(char *id, bool is_ibex) {
  Nick_db *db = read();
  bool set = false;
  EACH(db->nicks, Nick, n) {
    if (str_eq(nick_id(n), id)) {
      nick_set_is_ibex(n, is_ibex);
      set = true;
    }
  }_EACH
  if (set) {
    write();
  }
}

void nicks_db_set_sel(char *id, bool is_sel) {
  Nick_db *db = read();
  bool set = false;
  EACH(db->nicks, Nick, n) {
    if (str_eq(nick_id(n), id)) {
      nick_set_is_sel(n, is_sel);
      set = true;
    }
  }_EACH
  if (set) {
    write();
  }
}

bool nicks_db_set_name(char *id, char *name) {
  Nick_db *db = read();
  char *old = "";
  EACH(db->nicks, Nick, n) {
    if (str_eq(nick_id(n), id)) {
      old = nick_name(n);
      nick_set_name(n, name);
    } else if (str_eq(nick_name(n), name)) {
      return false;
    }
  }_EACH
  if (old) {
    write();
    char *old_name = path_cat(db_dir(), "quotes", old, NULL);
    char *new_name = path_cat(db_dir(), "quotes", name, NULL);
    file_rename(old_name, new_name);
  }
  return true;
}

void nicks_db_set_model(char *id) {
  if (ochar_is_null(nicks_db_name(id))) {
    return;
  }
  Nick_db *db = read();
  db->model = id;
  write();
}

