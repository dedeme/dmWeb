// Copyright 10-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/servers.h"
#include "io/log.h"
#include "io.h"
#include "DEFS.h"

char *servers_db = NULL;

/* .
-Servers: serial
  -next_id: int
  -list: Arr - Server
===
-IdNameCode: to
  -id: int
  -name: char *
  -code: char *
*/

/*--*/

struct servers_Servers{
  int next_id;
  Arr *list;
};

static Servers *_servers_new(int next_id, Arr *list) {
  Servers *this = MALLOC(Servers);
  this->next_id = next_id;
  this->list = list;
  return this;
}

Js *servers_to_js(Servers *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_wi((int)this->next_id));
  arr_push(js, arr_to_js(this->list, (FTO)server_to_js));
  return js_wa(js);
}

Servers *servers_from_js(Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  Servers *this = MALLOC(Servers);
  this->next_id = js_ri(*p++);
  this->list = arr_from_js(*p++, (FFROM)server_from_js);
  return this;
}

struct servers_IdNameCode{
  int id;
  char *name;
  char *code;
};

static IdNameCode *_idNameCode_new(int id, char *name, char *code) {
  IdNameCode *this = MALLOC(IdNameCode);
  this->id = id;
  this->name = name;
  this->code = code;
  return this;
}

Js *idNameCode_to_js(IdNameCode *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_wi((int)this->id));
  arr_push(js, js_ws(this->name));
  arr_push(js, js_ws(this->code));
  return js_wa(js);
}

/*--*/

static char *fservers (void) {
  if (servers_db) return servers_db;

  EXC_IO("'servers.db' was not initialized");
  return NULL; // Unreachable
}

static Servers *read (void) {
//  EXC_IO("'servers.db' was not initialized");
//  puts(file_read(fservers()));
//  return NULL;
  return servers_from_js((Js *)file_read(fservers()));
}

static void write (Servers *this) {
  file_write(fservers(), (char *)servers_to_js(this));
}

static int is_duplicate(Servers *this, char *short_name) {
  EACH(this->list, Server, s)
    if (str_eq(server_short_name(s), short_name)) return 1;
  _EACH
  return 0;
}

void servers_init (void) {
  servers_db = path_cat(io_data_dir(), "servers.db", NULL);
  if (!file_exists(servers_db)) {
    Servers *db = _servers_new(0, arr_new());
    write(db);
  }
}

// Arr[Server]
Arr *servers_list (void) {
   return read()->list;
}

// servers is Arr[Server]
static int id_index (Arr *servers, int id) {
  EACH_IX(servers, Server, sv, ix)
    if (server_id(sv) == id) {
      return ix;
    }
  _EACH
  return -1;
}

// servers is Arr[Server]
static int short_name_index (Arr *servers, char *short_name) {
  EACH_IX(servers, Server, sv, ix)
    if (server_short_name(sv) == short_name) {
      return ix;
    }
  _EACH
  return -1;
}

int servers_add (char *short_name) {
  Servers *db = read();
  if (is_duplicate(db, short_name)) return 0;
  arr_push(db->list, server_new(db->next_id, short_name));
  db->next_id += 1;
  write(db);
  return 1;
}

void servers_remove(int id) {
  Servers *db = read();
  // Arr[Server]
  Arr *list = db->list;
  int i = -1;
  EACH_IX(list, Server, sv, ix)
    if (server_id(sv) == id) {
      i = ix;
      break;
    }
  _EACH
  if (i != -1) {
    arr_remove(list, i);
    write(db);
  }
}

int servers_set_names (int id, char *short_name, char *name) {
  Servers *db = read();
  // Arr[Server]
  Arr *list = db->list;
  int ix = id_index(list, id);
  if (ix != -1) {
    Server *sv = arr_get(list, ix);
    if (
      !str_eq(server_short_name(sv), short_name) &&
      short_name_index(list, short_name) != -1
    ) {
        return 0;
    }
    server_set_short_name(sv, short_name);
    server_set_name(sv, name);
    write(db);
  }
  return 1;
}

void servers_activate(int id, int historic, Rconf *conf) {
  Servers *db = read();
  // Arr[Server]
  Arr *list = db->list;
  int ix = id_index(list, id);
  if (ix != -1) {
    Server *sv = arr_get(list, ix);
    if (historic) {
      if (opt_is_empty(server_historic_conf(sv))) {
        server_set_historic_conf(sv, opt_new(conf));
      } else {
        server_set_historic_conf(sv, opt_empty());
      }
    } else {
      if (opt_is_empty(server_daily_conf(sv))) {
        server_set_daily_conf(sv, opt_new(conf));
      } else {
        server_set_daily_conf(sv, opt_empty());
      }
    }

    write(db);
  }
}

// Returns Arr[char *]
void servers_set_conf(int id, int historic, Rconf *conf) {
  Servers *db = read();
  // Arr[Server]
  Arr *list = db->list;
  int ix = id_index(list, id);
  if (ix != -1) {
    Server *sv = arr_get(list, ix);
    if (historic) {
      if (rconf_sel(conf) == SERVER_SELECTED) {
        EACH(list, Server, sv)
          Opt *ocf = server_historic_conf(sv);
          if (opt_is_full(ocf)) {
            Rconf *cf = opt_get(ocf);
            if (rconf_sel(cf) == SERVER_SELECTED) {
              rconf_set_sel(cf, SERVER_ACTIVE);
            }
          }
        _EACH
      }
      server_set_historic_conf(sv, opt_new(conf));
    } else {
      server_set_daily_conf(sv, opt_new(conf));
    }

    write(db);
  }
}

// codes is Arr[ServerCodes]
void servers_set_codes(int id, Arr *codes) {
  Servers *db = read();
  // Arr[Server]
  Arr *list = db->list;
  int ix = id_index(list, id);
  if (ix != -1) {
    Server *sv = arr_get(list, ix);
    // Arr[ServerCode]
    Arr *scs = server_codes(sv);
    arr_remove_range(scs, 0, arr_size(scs));
    EACH(codes, ServerCode, sc)
      arr_push(scs, sc);
    _EACH

    write(db);
  }
}

void servers_add_nick (int nk_id) {
  Servers *db = read();
  EACH(db->list, Server, sv)
    int missing = 1;
    // Arr[ServerCode]
    Arr *codes = server_codes(sv);
    EACH(codes, ServerCode, c)
      if (serverCode_nick_id(c) == nk_id) {
        missing = 0;
        break;
      }
    _EACH
    if (missing) arr_push(codes, serverCode_new(nk_id, opt_empty()));
  _EACH
  write(db);
}

void servers_del_nick (int nk_id) {
  Servers *db = read();
  EACH(db->list, Server, sv)
    int i = -1;
    // Arr[ServerCode]
    Arr *codes = server_codes(sv);
    EACH_IX(codes, ServerCode, c, ix)
      if (serverCode_nick_id(c) == nk_id) {
        i = ix;
        break;
      }
    _EACH
    if (i != -1) arr_remove(codes, i);
  _EACH
  write(db);
}

// Returns Arr[IdNameCode]
Arr *servers_nick_codes (int nick_id) {
  // Arr[IdNameCode]
  Arr *r = arr_new();
  EACH(servers_list(), Server, sv)
    arr_push(r, _idNameCode_new(
      server_id(sv),
      server_short_name(sv),
      server_nick_code(sv, nick_id)
    ));
  _EACH
  return r;
}

void servers_set_nick_code (int sv_id, int nick_id, char *code) {
  Servers *db = read();
  EACH(db->list, Server, sv)
    if (server_id(sv) == sv_id) {
      server_set_nick_code(sv, nick_id, code);
      break;
    }
  _EACH
  write(db);
}

int servers_test_daily_conf (int id) {
  Servers *db = read();
  EACH(db->list, Server, sv)
    if (server_id(sv) == id) {
      // Opt[Arr[NickClose]]
      Opt *closes = server_daily_read(sv);
      if (opt_is_empty(closes)) return 0;
      if (arr_size(opt_get(closes)) != arr_size(server_codes(sv))) return 0;
      return 1;
    }
  _EACH
  log_error(str_f("Server '%d' not found", id));
  return 0;
}

int servers_test_historic_conf (int id, int nk_id) {
  Servers *db = read();
  EACH(db->list, Server, sv)
    if (server_id(sv) == id) {
      // Opt[Arr[Quote]]
      Opt *quotes = server_historic_read(sv, nk_id);
      if (opt_is_empty(quotes)) return 0;
      if (arr_size(opt_get(quotes)) < 10) return 0;
      return 1;
    }
  _EACH
  log_error(str_f("Server '%d' not found", id));
  return 0;
}

