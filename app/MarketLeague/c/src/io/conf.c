// Copyright 26-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/conf.h"
#include "dmc/cgi.h"

static char *mkpath (void) {
  char *r = path_cat(cgi_home(), "data", "conf.db", NULL);
  if (!file_exists(r)) {
    file_mkdir(path_cat(cgi_home(), "data", NULL));
    file_write(r, "{}");
  }
  return r;
}

// Opt<Js>
static Opt *read (char *key) {
  return map_get(js_ro((Js *)file_read(mkpath())), key);
}

static void write (char *key, Js *value) {
  char *path = mkpath();
  // Map<Js>
  Map *m = js_ro((Js *)file_read(path));
  map_put(m, key, value);
  file_write(path, (char *)js_wo(m));
}

char *conf_lang (void) {
  Js *lang = opt_nget(read("lang"));
  if (lang) return js_rs(lang);
  return "es";
}

void conf_set_lang(char *lang) {
  write("lang", js_ws(lang));
}
