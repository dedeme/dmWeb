// Copyright 12-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "edit.h"
#include "conf.h"
#include "data/nicks_db.h"
#include "data/servers_db.h"
#include "data/Issue.h"
#include "dmc/ct/Ochar.h"
#include "dmc/ct/Ajson.h"

CgiRp *edit_process(Mjson *rqm) {
  char *rq = jmap_gstring(rqm, "rq");

  // -------------------------------------------------------------- idata
  if (str_eq(rq, "idata")) {
    Mjson *conf = json_robject(conf_get());
    char *id = jmap_gstring(conf, "edit_id");
    if (!*id) {
      id = nicks_db_model();
    }
    char *name = "";
    if (id) {
      Ochar *oname = nicks_db_name(id);
      if (!ochar_is_null(oname)) {
        name = ochar_value(oname);
      }
    }
    Ajson *ics = ajson_new();
    EACH(servers_db_list(), Server, s) {
      char *serverId = server_name(s);
      char *nickCode = mchar_oget(server_nicks(s), id, "");
      Ajson *ic = ajson_new();
      ajson_add(ic, json_wstring(serverId));
      ajson_add(ic, json_wstring(nickCode));
      ajson_add(ics, json_warray(ic));
    }_EACH

    Mjson *m = mjson_new();
    jmap_pstring(m, "id", id);
    jmap_pstring(m, "name", name);
    mjson_put(m, "serversIdCode", json_warray(ics));
    return cgi_ok(m);
  }

  // -------------------------------------------------------------- modify
  if (str_eq(rq, "modify")) {
    char *id = jmap_gstring(rqm, "nickId");
    char *name = jmap_gstring(rqm, "nickName");
    Mjson *m = mjson_new();
    jmap_pbool(m, "ok", nicks_db_set_name(id, name));
    return cgi_ok(m);
  }

  // -------------------------------------------------------------- check
  if (str_eq(rq, "check")) {
    char *id = jmap_gstring(rqm, "id");
    Issue *i = issue_check(id);
    Mjson *m = mjson_new();
    mjson_put(m, "issue", issue_to_json(i));
    return cgi_ok(m);
  }

  THROW("") "'%s': Unknown request in edit", rq _THROW
  // Unreacheable
  return NULL;

}

