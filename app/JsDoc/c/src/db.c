// Copyright 06-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db.h"
#include "dmc/cgi.h"
#include "data/MenuPath.h"

// ____
// Conf --------------------------------------------------------------
// TTTT

static char *conf_rp = "data/conf.db";
static char *confp = NULL;

static void write_conf(char *path, char *lang, bool show) {
  Mjson *m = mjson_new();
  jmap_pstring(m, "path", path);
  jmap_pstring(m, "lang", lang);
  jmap_pbool(m, "show", show);
  file_write(confp, (char *)json_wobject(m));
}

Mjson *db_conf(void) {
  XNULL(confp)
  return json_robject((Json *)file_read(confp));
}

void db_set_lang(char *lang) {
  Mjson *m = db_conf();
  jmap_pstring(m, "lang", lang);
  file_write(confp, (char *)json_wobject(m));
}

void db_set_path(char *path) {
  Mjson *m = db_conf();
  jmap_pstring(m, "path", path);
  file_write(confp, (char *)json_wobject(m));
}

void db_set_show_all(bool value) {
  Mjson *m = db_conf();
  jmap_pbool(m, "show", value);
  file_write(confp, (char *)json_wobject(m));
}

// _____
// Paths -------------------------------------------------------------
// TTTTT

static char *paths_rp = "data/paths.db";
static char *pathsp = NULL;

static void write_paths(AmenuPath *paths) {
  file_write(pathsp, (char *)amenuPath_to_json(paths, menuPath_to_json));
}

static AmenuPath *get_paths() {
  XNULL(pathsp)
  return amenuPath_from_json((Json*)file_read(pathsp), menuPath_from_json);
}

Json *db_paths(void) {
  AmenuPath *ps = get_paths();
  EACH(ps, MenuPath, mp) {
    if (file_is_directory(menuPath_path(mp))) {
      menuPath_set_ok(mp, true);
    } else {
      menuPath_set_ok(mp, false);
    }
  }_EACH
  return amenuPath_to_json(ps, menuPath_to_json);
}

void db_add_path(char *id, char *path) {
  AmenuPath *ps = get_paths();
  AmenuPath *r = amenuPath_new();
  EACH(ps, MenuPath, mp) {
    if (!str_eq(menuPath_id(mp), id)) {
      amenuPath_add(r, mp);
    }
  }_EACH
  amenuPath_add(r, menuPath_new(id, path, true, true));
  write_paths(r);
}

void db_set_show(char *id, bool value) {
  AmenuPath *ps = get_paths();
  EACH(ps, MenuPath, mp) {
    if (str_eq(menuPath_id(mp), id)) {
      menuPath_set_show(mp, value);
    }
  }_EACH
  write_paths(ps);
}

void db_delete(char *id) {
  AmenuPath *ps = get_paths();
  AmenuPath *r = amenuPath_new();
  EACH(ps, MenuPath, mp) {
    if (!str_eq(menuPath_id(mp), id)) {
      amenuPath_add(r, mp);
    }
  }_EACH
  write_paths(r);
}

void db_modify(char *old_id, char *new_id, char *path) {
  AmenuPath *ps = get_paths();
  EACH(ps, MenuPath, mp) {
    if (str_eq(menuPath_id(mp), old_id)) {
      menuPath_set_id(mp, new_id);
      menuPath_set_path(mp, path);
    }
  }_EACH
  write_paths(ps);
}

// ___________
// Entry point -------------------------------------------------------
// TTTTTTTTTTT

void db_init(void) {
  confp = path_cat(cgi_home(), conf_rp, NULL);
  if (!file_exists(confp)) {
    write_conf("@", "es", true);
  }

  pathsp = path_cat(cgi_home(), paths_rp, NULL);
  if (!file_exists(pathsp)) {
    write_paths(amenuPath_new());
  }
}
