// Copyright 12-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "issues.h"
#include "data/nicks_db.h"
#include "data/Issue.h"
#include "data/servers_db.h"
#include "conf.h"
#include "dmc/ct/Ochar.h"
#include "dmc/ct/Ajson.h"

CgiRp *issues_process(Mjson *rqm) {
  char *rq = jmap_gstring(rqm, "rq");

  // -------------------------------------------------------------- idata
  if (str_eq(rq, "idata")) {
    Mjson *conf = json_robject(conf_get());
    char *id = jmap_gstring(conf, "issue_id");
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
    Issue *issue = issue_check(id);

    Mjson *m = mjson_new();
    jmap_pstring(m, "id", id);
    jmap_pstring(m, "name", name);
    mjson_put(m, "issue", issue_to_json(issue));
    return cgi_ok(m);
  }

  // ---------------------------------------------------------- nextIssue
  if (str_eq(rq, "nextIssue")) {
    Oissue *i = issue_search();
    Mjson *m = mjson_new();
    if (oissue_is_null(i)) {
      jmap_pstring(m, "nickId", "");
    } else {
      jmap_pstring(m, "nickId", issue_id(oissue_value(i)));
    }
    return cgi_ok(m);
  }

  // ------------------------------------------------------ serverIdCodes
  if (str_eq(rq, "serverIdCodes")) {
    char *id = jmap_gstring(rqm, "id");
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
    mjson_put(m, "serverIdCodes", json_warray(ics));
    return cgi_ok(m);
  }

  THROW("") "'%s': Unknown request in issues", rq _THROW
  // Unreacheable
  return NULL;

}
