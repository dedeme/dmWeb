// Copyright 10-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Server.h"
#include "data/Rconf.h"
#include "data/Nick.h"
#include "io/nicks.h"

/* .
ServerCode: serial
  nick_id: int
  #Opt[char]
  code: Opt - char *

===

Server: serial
  id: int
  short_name: char *
  ---
  name: char *: short_name
  #Opt[Rconf]
  company_conf: Opt - Rconf: opt_empty()
  #Opt[Rconf]
  daily_conf: Opt - Rconf: opt_empty()
  #Opt[Rconf]
  historic_conf: Opt - Rconf: opt_empty()
  #Arr[ServerCode]
  codes: Arr - ServerCode: make_codes()
*/

// Arr[ServerCode]
static Arr *make_codes(void) {
  ServerCode *fmap (Nick *nk) {
    return serverCode_new(nick_id(nk), opt_empty());
  }
  return arr_from_it(it_map(arr_to_it(nicks_list()), (FCOPY)fmap));
}

/*--*/

struct Server_ServerCode{
  int nick_id;
  Opt *code;
};

ServerCode *serverCode_new(int nick_id, Opt *code) {
  ServerCode *this = MALLOC(ServerCode);
  this->nick_id = nick_id;
  this->code = code;
  return this;
}

int serverCode_nick_id(ServerCode *this) {
  return this->nick_id;
}

Opt *serverCode_code(ServerCode *this) {
  return this->code;
}

Js *serverCode_to_js(ServerCode *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_wi((int)this->nick_id));
  arr_push(js, opt_is_empty(this->code)
    ? js_wn()
    : js_ws(opt_get(this->code))
  );
  return js_wa(js);
}

ServerCode *serverCode_from_js(Js *js) {
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

struct Server_Server{
  int id;
  char *short_name;
  char *name;
  Opt *company_conf;
  Opt *daily_conf;
  Opt *historic_conf;
  Arr *codes;
};

Server *server_new(int id, char *short_name) {
  Server *this = MALLOC(Server);
  this->id = id;
  this->short_name = short_name;
  this->name = short_name;
  this->company_conf = opt_empty();
  this->daily_conf = opt_empty();
  this->historic_conf = opt_empty();
  this->codes = make_codes();
  return this;
}

int server_id(Server *this) {
  return this->id;
}

char *server_short_name(Server *this) {
  return this->short_name;
}

char *server_name(Server *this) {
  return this->name;
}

Opt *server_company_conf(Server *this) {
  return this->company_conf;
}

Opt *server_daily_conf(Server *this) {
  return this->daily_conf;
}

Opt *server_historic_conf(Server *this) {
  return this->historic_conf;
}

Arr *server_codes(Server *this) {
  return this->codes;
}

Js *server_to_js(Server *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_wi((int)this->id));
  arr_push(js, js_ws(this->short_name));
  arr_push(js, js_ws(this->name));
  arr_push(js, opt_is_empty(this->company_conf)
    ? js_wn()
    : rconf_to_js(opt_get(this->company_conf))
  );
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

Server *server_from_js(Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  Server *this = MALLOC(Server);
  this->id = js_ri(*p++);
  this->short_name = js_rs(*p++);
  this->name = js_rs(*p++);
  this->company_conf = js_is_null(*p)
      ? p++? opt_empty(): NULL
      : opt_new(rconf_from_js(*p++))
  ;
  this->daily_conf = js_is_null(*p)
      ? p++? opt_empty(): NULL
      : opt_new(rconf_from_js(*p++))
  ;
  this->historic_conf = js_is_null(*p)
      ? p++? opt_empty(): NULL
      : opt_new(rconf_from_js(*p++))
  ;
  this->codes = arr_from_js(*p++, (FFROM)serverCode_to_js);
  return this;
}

/*--*/
