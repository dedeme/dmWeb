// Copyright 06-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "paths.h"
#include "db.h"

CgiRp *paths_process(Mjson *mrq) {
  char *rq = jmap_gstring(mrq, "rq");

  if (str_eq(rq, "setLang")) {
    char *lang = jmap_gstring(mrq, "lang");
    db_set_lang(lang);
    return cgi_ok(mjson_new());
  }

  if (str_eq(rq, "setShowAll")) {
    bool value = jmap_gbool(mrq, "showAll");
    db_set_show_all(value);
    return cgi_ok(mjson_new());
  }

  if (str_eq(rq, "addPath")) {
    char *id = jmap_gstring(mrq, "id");
    char *path = jmap_gstring(mrq, "path");
    db_add_path(id, path);
    return cgi_ok(mjson_new());
  }

  if (str_eq(rq, "setShow")) {
    char *id = jmap_gstring(mrq, "id");
    bool value = jmap_gbool(mrq, "value");
    db_set_show(id, value);
    return cgi_ok(mjson_new());
  }

  if (str_eq(rq, "delete")) {
    char *id = jmap_gstring(mrq, "id");
    db_delete(id);
    return cgi_ok(mjson_new());
  }

  if (str_eq(rq, "modify")) {
    char *old_id = jmap_gstring(mrq, "oldId");
    char *new_id = jmap_gstring(mrq, "newId");
    char *path = jmap_gstring(mrq, "path");
    db_modify(old_id, new_id, path);
    return cgi_ok(mjson_new());
  }

  THROW("") "'%s': Unknown request in settings", rq _THROW
  // Unreacheable
  return NULL;
}
