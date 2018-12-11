// Copyright 11-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "nicks.h"
#include "data/nicks_db.h"
#include "conf.h"
#include "data/Issue.h"

CgiRp *nicks_process(Mjson *rqm) {
  char *rq = jmap_gstring(rqm, "rq");

  // -------------------------------------------------------------- idata
  if (str_eq(rq, "idata")) {
    Mjson *m = mjson_new();
    jmap_pstring(m, "model", nicks_db_model());
    mjson_put(m, "nicks", anick_to_json(nicks_db_list()));
    return cgi_ok(m);
  }

  // -------------------------------------------------------------- new
  if (str_eq(rq, "new")) {
    char *nick = jmap_gstring(rqm, "nick");
    Mjson *m = mjson_new();
    jmap_pbool(m, "ok", nicks_db_add(nick, false, false));
    return cgi_ok(m);
  }

  // -------------------------------------------------------------- setModel
  if (str_eq(rq, "setModel")) {
    char *id = jmap_gstring(rqm, "id");
    nicks_db_set_model(id);
    return cgi_ok(mjson_new());
  }

  // -------------------------------------------------------------- del
  if (str_eq(rq, "del")) {
    char *id = jmap_gstring(rqm, "id");
    nicks_db_remove(id);
    return cgi_ok(mjson_new());
  }

  // -------------------------------------------------------------- setIsIbex
  if (str_eq(rq, "setIsIbex")) {
    char *id = jmap_gstring(rqm, "id");
    bool value = jmap_gbool(rqm, "value");
    nicks_db_set_ibex(id,value);
    return cgi_ok(mjson_new());
  }

  // -------------------------------------------------------------- setIsSel
  if (str_eq(rq, "setIsSel")) {
    char *id = jmap_gstring(rqm, "id");
    bool value = jmap_gbool(rqm, "value");
    nicks_db_set_sel(id,value);
    return cgi_ok(mjson_new());
  }

  // -------------------------------------------------------------- edit
  if (str_eq(rq, "edit")) {
    char *id = jmap_gstring(rqm, "id");
    char *menu = jmap_gstring(rqm, "menu");
    conf_set_edit_id(id);
    conf_set_menu(menu);
    return cgi_ok(mjson_new());
  }

  // -------------------------------------------------------------- issue
  if (str_eq(rq, "issue")) {
    char *id = jmap_gstring(rqm, "id");
    char *menu = jmap_gstring(rqm, "menu");
    conf_set_issue_id(id);
    conf_set_menu(menu);
    return cgi_ok(mjson_new());
  }

  THROW("") "'%s': Unknown request in nicks", rq _THROW
  // Unreacheable
  return NULL;

}
