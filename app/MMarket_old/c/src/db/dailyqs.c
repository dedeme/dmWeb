// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db/dailyqs.h"
#include "DEFS.h"

static char *path () {
  return path_cat(sys_home(), DATA_PATH, "Dailyqs.db", NULL);
}


void dailyqs_init (void) {
  if (!file_exists(path())) {
    file_write(path(), "{}");
  }
}

QtableRow dailyqs_read (Arr *nicks) {
  // Map[Js]
  Map *qs = js_ro((Js *)file_read(path()));
  QtableRow r = ATOMIC(arr_size(nicks) * sizeof(double));
  EACH_IX(nicks, char, nk, ix) {
    r[ix] = js_rd(opt_oget(map_get(qs, nk), (Js *)"-1"));
  }_EACH
  return r;
}

double dailyqs_read_nick (char *nick) {
  // Map[Js]
  Map *qs = js_ro((Js *)file_read(path()));
  return js_rd(opt_oget(map_get(qs, nick), (Js *)"-1"));
}

void dailyqs_write (Js *quotes) {
  file_write(path(), (char *)quotes);
}
