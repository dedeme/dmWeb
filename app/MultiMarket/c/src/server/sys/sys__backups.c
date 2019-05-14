// Copyright 05-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys/sys__backups.h"
#include "dmc/cgi.h"
#include "io/backups.h"
#include "io/trash.h"
#include "io.h"

// mrq is Map[Js]
char *sys__backups_process(Map *mrq) {
  CGI_GET_STR(rq, mrq, "rq")
  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "lists")) {
    void fn (void *null) {
      map_put(rp, "backups", arr_to_js(backups_list(), (FTO)js_ws));
      map_put(rp, "trash", arr_to_js(trash_list(), (FTO)js_ws));
    }
    io_wait(fn, NULL);
    return cgi_ok(rp);
  }
  if (str_eq(rq, "backup")) {
    void fn (void *null) { map_put(rp, "name", js_ws(backups_make())); }
    io_wait(fn, NULL);
    return cgi_ok(rp);
  }
  if (str_eq(rq, "restoreStart")) {
    void fn (void *null) { backups_restore_start(); }
    io_wait(fn, NULL);
    return cgi_empty();
  }
  if (str_eq(rq, "restoreAppend")) {
    CGI_GET_STR(data, mrq, "data");
    void fn (void *null) { backups_restore_append(data); }
    io_wait(fn, NULL);
    return cgi_empty();
  }
  if (str_eq(rq, "restoreAbort")) {
    void fn (void *null) { backups_restore_abort(); }
    io_wait(fn, NULL);
    return cgi_empty();
  }
  if (str_eq(rq, "restoreEnd")) {
    char *fail = "";
    void fn (void *null) { fail = backups_restore_end(); }
    io_wait(fn, NULL);
    map_put(rp, "fail", js_ws(fail));
    return cgi_ok(rp);
  }
  if (str_eq(rq, "autorestore")) {
    CGI_GET_STR(file, mrq, "file");
    void fn (void *null) { backups_autorestore(file); }
    io_wait(fn, NULL);
    return cgi_empty();
  }
  if (str_eq(rq, "clearTrash")) {
    void fn (void *null) { trash_clear(); }
    io_wait(fn, NULL);
    return cgi_empty();
  }
  if (str_eq(rq, "restoreTrash")) {
    CGI_GET_STR(file, mrq, "file");
    void fn (void *null) { trash_restore(file); }
    io_wait(fn, NULL);
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
