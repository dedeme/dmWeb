// Copyright 04-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "conf.h"
#include "dmc/cgi.h"

static char *relative_path = "data/conf.db";

static char *path_null = NULL;

void conf_init(void) {
  path_null = path_cat_new(cgi_home(), relative_path, NULL);
  if (!file_exists(path_null)) {
    // Map[Js]
    Map *m = map_new(free);
    map_put(m, "lang", js_ws_new("en"));
    map_put(m, "menu", js_ws_new("settings"));
    Js *js = js_wo_new(m);
    file_write(path_null, (char *)js);
    map_free(m);
    free(js);
  }
}

void conf_end(void) {
  free(path_null);
}

Js *conf_get_new(void) {
  if (!path_null) FAIL("'data/conf.db' has not been initialized")

  return (Js *)file_read_new(path_null);
}

void conf_set(const char *field, const char *value) {
  if (!path_null) FAIL("'data/conf.db' has not been initialized")

  Js *js = conf_get_new();
  // Map[Js]
  Map *m = js_ro_new(js);
  map_put(m, field, js_ws_new(value));
  Js *newjs = js_wo_new(m);
  file_write(path_null, (char *)newjs);
  map_free(m);
  free(js);
  free(newjs);
}

void conf_set_lang(const char *lang) {
  conf_set("lang", lang);
}

void conf_set_menu(const char *option) {
  conf_set("menu", option);
}

Js *conf_get_model_new() {
  Js *js = conf_get_new();
  // Map[Js]
  Map *m = js_ro_new(js);
  Js *rjs = map_get_null(m, "model");
  Js *r = rjs ? (Js *) str_new((char *)rjs) : js_ws_new("");
  free(js);
  map_free(m);
  return r;
}

void conf_set_model(const char *option) {
  conf_set("model", option);
}

