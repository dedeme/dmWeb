// Copyright 04-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/conf.h"
#include "io.h"

static char *conf = NULL;

void conf_init (void) {
  conf = path_cat(io_data_dir(), "conf.db", NULL);
  if (!file_exists(conf)) {
    file_write(conf, "{}");
  }
}

// returns Map[Js]
static Map *read (void) {
  if (!conf) EXC_ILLEGAL_STATE("'conf.db' was not intiliazed")

  return js_ro((Js *)file_read(conf));
}


// kv is Kv[Js]
static void write (char *key, Js *value) {
  // Map[Js]
  Map *m = read();
  map_put(m, key, value);
  file_write(conf, (char *)js_wo(m));
}

char *conf_lang (void) {
  return js_rs(opt_oget(map_get(read(), "lang"), js_ws("")));
}

void conf_set_lang (char *lang) {
  write("lang", js_ws(lang));
}

char *conf_sys_page (void) {
  return js_rs(opt_oget(map_get(read(), "sysPage"), js_ws("")));
}

void conf_set_sys_page (char *sys_page) {
  write("sysPage", js_ws(sys_page));
}

int conf_nick_sel_id (void) {
  return js_ri(opt_oget(map_get(read(), "nickSelId"), js_wi(-1)));
}

void conf_set_nick_sel_id (int id) {
  write("nickSelId", js_wi(id));
}


