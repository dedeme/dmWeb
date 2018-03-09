// Copyright 06-Mar-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pgs/main.h"
#include "Conf.h"

CgiRp *pgs_main(Cgi *cgi, char *session_id, Map/*Json*/ *rqm) {
  char *rq = jmap_gstring(rqm, "rq");
  if (!strcmp(rq, "conf")) {
    Conf *cf = conf_read();
    Map/*Json*/ *m = map_new();
    jmap_pstring(m, "lang", conf_lang(cf));
    jmap_pstring(m, "pg", conf_pg(cf));
    jmap_pstring(m, "pg2", conf_pg2(cf));
    return cgi_ok(cgi, m);
  } else if (!strcmp(rq, "go")) {
    Conf *cf = conf_read();
    conf_set_pg(cf, jmap_gstring(rqm, "page"));
    conf_set_pg2(cf, jmap_gstring(rqm, "subpage"));
    conf_write(cf);
    return cgi_ok(cgi, map_new());
  } else {
    return cgi_error(cgi, str_printf("'%s': Unknown request in Main", rq));
  }
}
