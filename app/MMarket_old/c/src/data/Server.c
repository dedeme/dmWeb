// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Server.h"
#include "data/Nick.h"
#include "data/Daily.h"
#include "data/Historic.h"
#include "data/Quote.h"

/* .
# Nicks-Code pair.
ServerCode: serial
  nick_id: int
  # Opt[char]
  code: Opt - char *
===

# Server configuration
-ServerConf: SERIAL
  cmd: char *
  url: char *
  regex: char *
  # enum Server
  @sel: int
  is_date_eu: bool
  date_separator: char *
  is_iso_number: bool
  fields_type: char *
  table_start: char *
  table_end: char *
  row_start: char *
  row_end: char *
  cols_start: Arr - char *
  cols_end: Arr - char *
===

# Server data
-Server: serial
  id: int
  @short_name: char *
  # Arr[nick]
  # -nicks: Arr - Nick
  ---
  @name: char *: short_name
  # Opt[ServerConf]
  @daily_conf: Opt - ServerConf: opt_empty()
  # Opt[ServerConf]
  @historic_conf: Opt - ServerConf: opt_empty()
  # Arr[ServerCode]
  codes: Arr - ServerCode: arr_new()
*/

/*--*/

struct Server_ServerCode {
  int nick_id;
  Opt *code;
};

ServerCode *serverCode_new (int nick_id, Opt *code) {
  ServerCode *this = MALLOC(ServerCode);
  this->nick_id = nick_id;
  this->code = code;
  return this;
}

int serverCode_nick_id (ServerCode *this) {
  return this->nick_id;
}

Opt *serverCode_code (ServerCode *this) {
  return this->code;
}

Js *serverCode_to_js (ServerCode *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_wi((int)this->nick_id));
  arr_push(js, opt_is_empty(this->code)
    ? js_wn()
    : js_ws(opt_get(this->code))
  );
  return js_wa(js);
}

ServerCode *serverCode_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  ServerCode *this = MALLOC(ServerCode);
  this->nick_id = js_ri(*p++);
  this->code = js_is_null(*p)
      ? p++? opt_empty(): NULL
      : opt_new(js_rs(*p++))
  ;
  return this;
}

struct Server_ServerConf {
  char *cmd;
  char *url;
  char *regex;
  int sel;
  int is_date_eu;
  char *date_separator;
  int is_iso_number;
  char *fields_type;
  char *table_start;
  char *table_end;
  char *row_start;
  char *row_end;
  Arr *cols_start;
  Arr *cols_end;
};

char *serverConf_cmd (ServerConf *this) {
  return this->cmd;
}

char *serverConf_url (ServerConf *this) {
  return this->url;
}

char *serverConf_regex (ServerConf *this) {
  return this->regex;
}

int serverConf_sel (ServerConf *this) {
  return this->sel;
}

void serverConf_set_sel (ServerConf *this, int value) {
  this->sel = value;
}

int serverConf_is_date_eu (ServerConf *this) {
  return this->is_date_eu;
}

char *serverConf_date_separator (ServerConf *this) {
  return this->date_separator;
}

int serverConf_is_iso_number (ServerConf *this) {
  return this->is_iso_number;
}

char *serverConf_fields_type (ServerConf *this) {
  return this->fields_type;
}

char *serverConf_table_start (ServerConf *this) {
  return this->table_start;
}

char *serverConf_table_end (ServerConf *this) {
  return this->table_end;
}

char *serverConf_row_start (ServerConf *this) {
  return this->row_start;
}

char *serverConf_row_end (ServerConf *this) {
  return this->row_end;
}

Arr *serverConf_cols_start (ServerConf *this) {
  return this->cols_start;
}

Arr *serverConf_cols_end (ServerConf *this) {
  return this->cols_end;
}

Js *serverConf_to_js (ServerConf *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->cmd));
  arr_push(js, js_ws(this->url));
  arr_push(js, js_ws(this->regex));
  arr_push(js, js_wi((int)this->sel));
  arr_push(js, js_wb(this->is_date_eu));
  arr_push(js, js_ws(this->date_separator));
  arr_push(js, js_wb(this->is_iso_number));
  arr_push(js, js_ws(this->fields_type));
  arr_push(js, js_ws(this->table_start));
  arr_push(js, js_ws(this->table_end));
  arr_push(js, js_ws(this->row_start));
  arr_push(js, js_ws(this->row_end));
  arr_push(js, arr_to_js(this->cols_start, (FTO)js_ws));
  arr_push(js, arr_to_js(this->cols_end, (FTO)js_ws));
  return js_wa(js);
}

ServerConf *serverConf_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  ServerConf *this = MALLOC(ServerConf);
  this->cmd = js_rs(*p++);
  this->url = js_rs(*p++);
  this->regex = js_rs(*p++);
  this->sel = js_ri(*p++);
  this->is_date_eu = js_rb(*p++);
  this->date_separator = js_rs(*p++);
  this->is_iso_number = js_rb(*p++);
  this->fields_type = js_rs(*p++);
  this->table_start = js_rs(*p++);
  this->table_end = js_rs(*p++);
  this->row_start = js_rs(*p++);
  this->row_end = js_rs(*p++);
  this->cols_start = arr_from_js(*p++, (FFROM)js_rs);
  this->cols_end = arr_from_js(*p++, (FFROM)js_rs);
  return this;
}

struct Server_Server {
  int id;
  char *short_name;
  char *name;
  Opt *daily_conf;
  Opt *historic_conf;
  Arr *codes;
};

static Server *_server_new (int id, char *short_name) {
  Server *this = MALLOC(Server);
  this->id = id;
  this->short_name = short_name;
  this->name = short_name;
  this->daily_conf = opt_empty();
  this->historic_conf = opt_empty();
  this->codes = arr_new();
  return this;
}

int server_id (Server *this) {
  return this->id;
}

char *server_short_name (Server *this) {
  return this->short_name;
}

void server_set_short_name (Server *this, char *value) {
  this->short_name = value;
}

char *server_name (Server *this) {
  return this->name;
}

void server_set_name (Server *this, char *value) {
  this->name = value;
}

Opt *server_daily_conf (Server *this) {
  return this->daily_conf;
}

void server_set_daily_conf (Server *this, Opt *value) {
  this->daily_conf = value;
}

Opt *server_historic_conf (Server *this) {
  return this->historic_conf;
}

void server_set_historic_conf (Server *this, Opt *value) {
  this->historic_conf = value;
}

Arr *server_codes (Server *this) {
  return this->codes;
}

Js *server_to_js (Server *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_wi((int)this->id));
  arr_push(js, js_ws(this->short_name));
  arr_push(js, js_ws(this->name));
  arr_push(js, opt_is_empty(this->daily_conf)
    ? js_wn()
    : serverConf_to_js(opt_get(this->daily_conf))
  );
  arr_push(js, opt_is_empty(this->historic_conf)
    ? js_wn()
    : serverConf_to_js(opt_get(this->historic_conf))
  );
  arr_push(js, arr_to_js(this->codes, (FTO)serverCode_to_js));
  return js_wa(js);
}

Server *server_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  Server *this = MALLOC(Server);
  this->id = js_ri(*p++);
  this->short_name = js_rs(*p++);
  this->name = js_rs(*p++);
  this->daily_conf = js_is_null(*p)
      ? p++? opt_empty(): NULL
      : opt_new(serverConf_from_js(*p++))
  ;
  this->historic_conf = js_is_null(*p)
      ? p++? opt_empty(): NULL
      : opt_new(serverConf_from_js(*p++))
  ;
  this->codes = arr_from_js(*p++, (FFROM)serverCode_from_js);
  return this;
}

/*--*/

Server *server_new(int id, char *short_name, Arr *nicks) {
  Server *this = _server_new(id, short_name);
  ServerCode *fmap (Nick *nk) {
    return serverCode_new(nick_id(nk), opt_empty());
  }
  this->codes = arr_from_it(it_map(arr_to_it(nicks), (FCOPY)fmap));
  return this;
}

char *server_nick_code (Server *this, int nick_id) {
  EACH(this->codes, ServerCode, sc)
    if (sc->nick_id == nick_id) {
      if (opt_is_empty(sc->code)) return "";
      else return opt_get(sc->code);
    }
  _EACH
  return "";
}

void server_set_nick_code (Server *this, int nick_id, char *code) {
  EACH(this->codes, ServerCode, sc)
    if (sc->nick_id == nick_id) {
      if (*code) sc->code = opt_new(code);
      else sc->code = opt_empty();
    }
  _EACH
}

