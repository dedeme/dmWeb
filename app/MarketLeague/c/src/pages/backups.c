// Copyright 26-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pages/backups.h"
#include "dmc/cgi.h"
#include "io/backupsdb.h"

char *backups_process(Map *mrq) {
  CGI_GET_STR(rq, mrq)

  // Map<Js>
  Map *m = map_new();

  if (str_eq(rq, "lists")) {
    map_put(m, "backups", arr_to_js(backupsdb_backups(), (FTO)js_ws));
    map_put(m, "trash", arr_to_js(backupsdb_trash(), (FTO)js_ws));
    return cgi_ok(m);
  }
  if (str_eq(rq, "backup")) {
    map_put(m, "name", js_ws(backupsdb_mkbackup()));
    return cgi_ok(m);
  }
  if (str_eq(rq, "restoreStart")) {
    backupsdb_restore_start();
    return cgi_empty();
  }
  if (str_eq(rq, "restoreAbort")) {
    backupsdb_restore_abort();
    return cgi_empty();
  }
  if (str_eq(rq, "restoreAppend")) {
    CGI_GET_STR(data, mrq);
    backupsdb_restore_append(data);
    return cgi_empty();
  }
  if (str_eq(rq, "restoreEnd")) {
    map_put(m, "fail", js_ws(backupsdb_restore_end()));
    return cgi_ok(m);
  }
  if (str_eq(rq, "clearTrash")) {
    backupsdb_clear_trash();
    return cgi_empty();
  }
  if (str_eq(rq, "restoreTrash")) {
    CGI_GET_STR(file, mrq);
    backupsdb_restore_trash(file);
    return cgi_empty();
  }
  if (str_eq(rq, "autorestore")) {
    CGI_GET_STR(file, mrq);
    backupsdb_autorestore(file);
    return cgi_empty();
  }

  EXC_ILLEGAL_ARGUMENT(
    "rq",
    "lists | backup | restoreStart | restoreAbort | restoreAppend | "
    "restoreEnd | clearTrash | restoreTrash | autorestore",
    rq
  )
  return NULL; // Unreachable
}

