// Copyright 04-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "conf.h"
#include "dmc/cgi.h"
#include "dmc/ct/Mjson.h"

static char *relative_path = "data/conf.db";

static char *path = NULL;

void conf_init(void) {
  path = path_cat(cgi_home(), relative_path, NULL);
  if (!file_exists(path)) {
    Mjson *m = mjson_new();
    jmap_pstring(m, "lang", "en");
    jmap_pstring(m, "menu", "settings");
    jmap_pstring(m, "edit_id", "");
    jmap_pstring(m, "issue_id", "");
    jmap_pstring(m, "server_id", "");
    file_write(path, (char *)json_wobject(m));
  }
}

Json *conf_get(void) {
  if (!path) {
    exc_illegal_state("'data/conf.db' has not been initialized");
  }

  return (Json *)file_read(path);
}

static void set_field(char *key, char *value) {
  if (!path) {
    exc_illegal_state("'data/conf.db' has not been initialized");
  }

  Mjson *m = json_robject(conf_get());
  jmap_pstring(m, key, value);
  file_write(path, (char *)json_wobject(m));
}

void conf_set_lang(char *lang) {
  set_field("lang", lang);
}

void conf_set_menu(char *option) {
  set_field("menu", option);
}

void conf_set_edit_id(char *id) {
  set_field("edit_id", id);
}

void conf_set_issue_id(char *id) {
  set_field("issue_id", id);
}

void conf_set_server_id(char *id) {
  set_field("server_id", id);
}

