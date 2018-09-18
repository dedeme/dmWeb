// Copyright 12-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "edit.h"
#include "conf.h"
#include "data/nicks_db.h"
#include "data/servers_db.h"
#include "data/Issue.h"
#include "dmc/ct/Ochar.h"
#include "dmc/ct/Ajson.h"

CgiRp *edit_process(Mjson *rqm, int max_quotes) {
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

    char *qs = nicks_db_quotes_str(id);
    char *mqs = nicks_db_quotes_str(nicks_db_model());
    Anick *nicks = nicks_db_list();

    Mjson *m = mjson_new();
    jmap_pstring(m, "id", id);
    jmap_pstring(m, "name", name);
    jmap_pstring(m, "modelId", nicks_db_model());
    mjson_put(m, "serversIdCode", json_warray(ics));
    jmap_pstring(m, "quotes", qs);
    jmap_pstring(m, "modelQuotes", mqs);
    mjson_put(m, "nicks", anick_to_json(nicks));
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

  // ------------------------------------------------------------ nickQuotes
  if (str_eq(rq, "nickQuotes")) {
    char *id = jmap_gstring(rqm, "id");
    char *qs = nicks_db_quotes_str(id);
    Mjson *m = mjson_new();
    jmap_pstring(m, "quotes", qs);
    return cgi_ok(m);
  }

  // ---------------------------------------------------------- saveQuotes
  if (str_eq(rq, "saveQuotes")) {
    char *id = jmap_gstring(rqm, "id");
    char *qs_str = jmap_gstring(rqm, "quotes");

    bool ok = true;
    Aquote *qs = aquote_new();
    EACH(str_csplit_trim(qs_str, '\n'), char, l) {
      Achar *parts = str_csplit_trim(l, ':');
      if (achar_size(parts) == 7) {
        aquote_add(qs, quote_from_str(str_cjoin(achar_to_it(parts), ':')));
      } else {
        ok = false;
        break;
      }
    }_EACH
    nicks_db_set_quotes(id, qs);

    Mjson *m = mjson_new();
    jmap_pbool(m, "ok", ok);
    return cgi_ok(m);
  }

  // ---------------------------------------------------------- modifyQuotes
  if (str_eq(rq, "modifyQuotes") || str_eq(rq, "checkQuotes")) {
    char *id = jmap_gstring(rqm, "id");
    char *qs_str = jmap_gstring(rqm, "quotes");

    bool ok = true;
    Buf *bf = buf_new();
    Aquote *qs = aquote_new();
    EACH(str_csplit_trim(qs_str, '\n'), char, l) {
      if (ok) {
        Achar *parts = str_csplit_trim(l, ':');
        if (achar_size(parts) == 7) {
          aquote_add(qs, quote_from_str(str_cjoin(achar_to_it(parts), ':')));
        } else {
          buf_add(bf, "% ******************\n");
          ok = false;
        }
      }
      buf_add(bf, l);
      buf_cadd(bf, '\n');
    }_EACH

    Issue *issue = issue_new(id, ISSUE_EXISTS, "");
    qs_str = buf_str(bf);
    if (ok) {
      issue = issue_check_quotes(id, qs);
      if (issue_type(issue) != ISSUE_NONE) {
        qs_str = issue_annotate(qs, issue);
      } else if (str_eq(rq, "modifyQuotes")) {
        nicks_db_set_quotes(id, qs);
      }
    }

    Mjson *m = mjson_new();
    mjson_put(m, "issue", issue_to_json(issue));
    jmap_pstring(m, "quotes", qs_str);
    return cgi_ok(m);
  }

  // ---------------------------------------------------------- modifyForce
  if (str_eq(rq, "modifyForce")) {
    char *id = jmap_gstring(rqm, "id");
    char *qs_str = jmap_gstring(rqm, "quotes");

    bool ok = true;
    Buf *bf = buf_new();
    Aquote *qs = aquote_new();
    EACH(str_csplit_trim(qs_str, '\n'), char, l) {
      if (ok) {
        Achar *parts = str_csplit_trim(l, ':');
        if (achar_size(parts) == 7) {
          aquote_add(qs, quote_from_str(str_cjoin(achar_to_it(parts), ':')));
        } else {
          ok = false;
          break;
        }
      }
      buf_add(bf, l);
      buf_cadd(bf, '\n');
    }_EACH

    if (ok) {
      nicks_db_set_quotes(id, qs);
    }

    Mjson *m = mjson_new();
    jmap_pbool(m, "ok", ok);
    return cgi_ok(m);
  }

  // ----------------------------------------------------------- download
  if (str_eq(rq, "download")) {
    char *id = jmap_gstring(rqm, "id");

    bool ok = false;
    char *server_name = jmap_gstring(json_robject(conf_get()), "server_id");
    if (*server_name) {
      Server *sv = servers_db_get(server_name);
      Mchar *codes = server_nicks(sv);
      Ochar *code = mchar_get(codes, id);
      if (!ochar_is_null(code)) {
        Oaquote *osv_qs = server_read(sv)(ochar_value(code));
        Oaquote *oqs = nicks_db_quotes(id);
        if (!oaquote_is_null(osv_qs) && !oaquote_is_null(oqs)) {
          Aquote *sv_qs = oaquote_value(osv_qs);
          Aquote *qs = oaquote_value(oqs);
          Aquote *new_qs = aquote_new();
          if (aquote_size(qs)) {
            char *firstDate = quote_date(aquote_get(qs, 0));
            EACH(sv_qs, Quote, q) {
              if (str_cmp(quote_date(q), firstDate) > 0) {
                aquote_add(new_qs, q);
              }
            }_EACH
          }
          aquote_add_arr(new_qs, qs);
          new_qs = aquote_from_it(iquote_take(
            aquote_to_it(new_qs),
            max_quotes
          ));
          nicks_db_set_quotes(id, new_qs);
          ok = true;
        }
      }
    }

    Mjson *m = mjson_new();
    jmap_pbool(m, "ok", ok);
    return cgi_ok(m);
  }

  THROW("") "'%s': Unknown request in edit", rq _THROW
  // Unreacheable
  return NULL;

}

