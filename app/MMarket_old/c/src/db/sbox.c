// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db/sbox.h"
#include "db/servers.h"
#include "db/log.h"
#include "DEFS.h"

static char *path () {
  return path_cat(sys_home(), DATA_PATH, "Sbox.db", NULL);
}

void sbox_init (void) {
  if (!file_exists(path())) {
    file_write(path(), "[]");
  }
}

void sbox_next () {
  /// Arr[char]
  Arr *sv_names = arr_from_js((Js *)file_read(path()), (FFROM)js_rs);
  if (arr_size(sv_names) > 0) {
    arr_remove(sv_names, 0);
  }

  if (arr_size(sv_names) == 0) {
    EACH(servers_daily_list(), Server, s)
      arr_push(sv_names, server_name(s));
    _EACH
    arr_shuffle(sv_names);
    log_info("Daily servers box remade");
  }

  file_write(path(), (char *)arr_to_js(sv_names, (FTO)js_ws));
}

// Returns Opt[Server]
Opt *sbox_get (void) {
  /// Arr[char]
  Arr *sv_names = arr_from_js((Js *)file_read(path()), (FFROM)js_rs);
  if (arr_size(sv_names) == 0) {
    sbox_next();
    sv_names = arr_from_js((Js *)file_read(path()), (FFROM)js_rs);
    if (arr_size(sv_names) == 0) return opt_empty();
  }
  char *sv_name = arr_get(sv_names, 0);

  Server *sv = NULL;
  EACH(servers_daily_list(), Server, s)
    if (str_eq(server_name(s), sv_name)) {
      sv = s;
      break;
    }
  _EACH

  if (sv) return opt_new(sv);
  else {
    sbox_next();
    return sbox_get();
  }
}

