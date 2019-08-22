// Copyright 05-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys/sys__backups.h"
#include "dmc/cgi.h"
#include "io/backups.h"
#include "io/trash.h"

// mrq is Map[Js]
char *sys__backups_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq)
  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "lists")) {
    void fn () {
      map_put(rp, "backups", arr_to_js(backups_list(), (FTO)js_ws));
      map_put(rp, "trash", arr_to_js(trash_list(), (FTO)js_ws));
    }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }
  if (str_eq(rq, "backup")) {
    void fn () { map_put(rp, "name", js_ws(backups_make())); }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }
  if (str_eq(rq, "restoreStart")) {
    void fn () { backups_restore_start(); }
    asyncActor_wait(ac, fn);
    return cgi_empty();
  }
  if (str_eq(rq, "restoreAppend")) {
    CGI_GET_STR(data, mrq);
    void fn () { backups_restore_append(data); }
    asyncActor_wait(ac, fn);
    return cgi_empty();
  }
  if (str_eq(rq, "restoreAbort")) {
    void fn () { backups_restore_abort(); }
    asyncActor_wait(ac, fn);
    return cgi_empty();
  }
  if (str_eq(rq, "restoreEnd")) {
    void fn () { map_put(rp, "fail", js_ws(backups_restore_end())); }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }
  if (str_eq(rq, "autorestore")) {
    CGI_GET_STR(file, mrq);
    void fn () { backups_autorestore(file); }
    asyncActor_wait(ac, fn);
    return cgi_empty();
  }
  if (str_eq(rq, "clearTrash")) {
    void fn () { trash_clear(); }
    asyncActor_wait(ac, fn);
    return cgi_empty();
  }
  if (str_eq(rq, "restoreTrash")) {
    CGI_GET_STR(file, mrq);
    void fn () { trash_restore(file); }
    asyncActor_wait(ac, fn);
    return cgi_empty();
  }

  EXC_ILLEGAL_ARGUMENT(
    "rq",
    "lists | backup | "
    "restoreStart | restoreAppend | restoreAbort | restoreEnd | "
    "clearTrash",
    rq
  )
  return NULL; // Unreachable
}
