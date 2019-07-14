// Copyright 10-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Server.h"
#include "data/Rconf.h"
#include "data/Nick.h"
#include "data/DailyEntry.h"
#include "data/NickClose.h"
#include "data/HistoricEntry.h"
#include "data/Quote.h"

/* .
ServerCode: serial
  nick_id: int
  # Opt[char]
  code: Opt - char *
===

-Server: serial
  id: int
  @short_name: char *
  # Arr[nick]
  # -nicks: Arr - Nick
  ---
  @name: char *: short_name
  # Opt[Rconf]
  @daily_conf: Opt - Rconf: opt_empty()
  # Opt[Rconf]
  @historic_conf: Opt - Rconf: opt_empty()
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
    : rconf_to_js(opt_get(this->daily_conf))
  );
  arr_push(js, opt_is_empty(this->historic_conf)
    ? js_wn()
    : rconf_to_js(opt_get(this->historic_conf))
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
      : opt_new(rconf_from_js(*p++))
  ;
  this->historic_conf = js_is_null(*p)
      ? p++? opt_empty(): NULL
      : opt_new(rconf_from_js(*p++))
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
