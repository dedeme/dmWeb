// Copyright 11-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "servers.h"
#include "data/servers_db.h"
#include "data/nicks_db.h"
#include "conf.h"

CgiRp *servers_process(Mjson *rqm) {
  char *rq = jmap_gstring(rqm, "rq");

  // -------------------------------------------------------------- idata
  if (str_eq(rq, "idata")) {
    Achar *servers = achar_new();
    EACH(servers_db_list(), Server, s) {
      achar_add(servers, server_name(s));
    }_EACH

    Mjson *conf = json_robject(conf_get());
    char *selServer = jmap_gstring(conf, "server_id");
    if (!*selServer) {
      selServer = achar_get(servers, 0);
    }

    Anick *nicks = nicks_db_list();

    Mjson *m = mjson_new();
    jmap_pstring(m, "selServer", selServer);
    mjson_put(m, "servers", achar_to_json(servers));
    mjson_put(m, "nicks", anick_to_json(nicks));
    return cgi_ok(m);
  }

  // ------------------------------------------------------- nickCode
  if (str_eq(rq, "nickCode")) {
    char *serverId = jmap_gstring(rqm, "serverId");
    char *nickId = jmap_gstring(rqm, "nickId");
    Server *sv = servers_db_get(serverId);
    Mchar *nicks = server_nicks(sv);
    char *code = mchar_oget(nicks, nickId, "");
    Mjson *m = mjson_new();
    jmap_pstring(m, "code", code);
    return cgi_ok(m);
  }

  // ------------------------------------------------------- testCode
  if (str_eq(rq, "testCode")) {
    char *serverId = jmap_gstring(rqm, "serverId");
    char *code = jmap_gstring(rqm, "code");
    Server *sv = servers_db_get(serverId);
    bool ok = true;
    if (oaquote_is_null(server_read(sv)(code))) {
      ok = false;
    }
    Mjson *m = mjson_new();
    jmap_pbool(m, "ok", ok);
    return cgi_ok(m);
  }

  // ------------------------------------------------------- testCode
  if (str_eq(rq, "setCode")) {
    char *serverId = jmap_gstring(rqm, "serverId");
    char *nickId = jmap_gstring(rqm, "nickId");
    char *code = jmap_gstring(rqm, "code");
    Server *sv = servers_db_get(serverId);
    Mchar *codes = server_nicks(sv);
    mchar_put(codes, nickId, code);
    server_set_nicks(sv, codes);
    return cgi_ok(mjson_new());
  }

  // ------------------------------------------------------- setSelServer
  if (str_eq(rq, "setSelServer")) {
    char *serverId = jmap_gstring(rqm, "name");
    conf_set_server_id(serverId);
    return cgi_ok(mjson_new());
  }

  THROW("") "'%s': Unknown request in servers", rq _THROW
  // Unreacheable
  return NULL;

}
