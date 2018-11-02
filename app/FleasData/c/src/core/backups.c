// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "core/backups.h"
#include "dmc/cgi.h"
#include "dmc/ext.h"
#include "dmc/date.h"
#include "dmc/b64.h"

static char *mk_date_new(void) {
  return date_f_new(date_now(), "%Y%m%d");
}

static char *mk_date2_new(void) {
	time_t t = date_now();
  char *d = mk_date_new();
  char *r = str_f_new("%s-%ld", d, t);
  free(d);
  return r;
}

static void filter_backups(void) {
  time_t t0 = date_now();
  time_t t1 = date_add(t0, -7);
  time_t t2 = date_new(date_day(t0), date_month(t0), date_year(t0) - 1);

  char *d1 = date_f_new(t1, "%Y%m%d");
  char *d2 = date_f_new(t2, "%Y%m%d");

  char *backups_dir = path_cat_new(cgi_home(), "backups", NULL);
  // Arr[char]
  Arr *fs = file_dir_new(backups_dir);
  free(backups_dir);

  arr_sort(fs, (FGREATER)str_greater);
  char *previous = str_new("        ");
  EACH(fs, char, f) {
    char *name = str_new(f);
    path_only_name(&name);
    if (!str_greater(name, d2)) {
      str_left(&name, 4);
      str_left(&previous, 4);
      if (str_eq(name, previous)) {
        file_del(f);
      }
    } else if (!str_greater(name, d1)) {
      str_left(&name, 6);
      str_left(&previous, 6);
      if (str_eq(name, previous)) {
        file_del(f);
      }
    }
    free(previous);
    previous = name;
  }_EACH

  free(previous);
  arr_free(fs);
  free(d1);
  free(d2);
}

// Returns Arr[char]
static Arr *read_backups_new(void) {
  char *backups_dir = path_cat_new(cgi_home(), "backups", NULL);
  // Arr[char]
  Arr *fs = file_dir_new(backups_dir);
  free(backups_dir);
  // Arr[char]
  Arr *rs = arr_new(free);
  EACH(fs, char, f) {
    char *pname = str_new(f);
    path_name(&pname);
    arr_push(rs, pname);
  }_EACH
  arr_free(fs);
	return rs;
}

// Returns Arr[char]
static Arr *read_trash_new(void) {
  char *trash_dir = path_cat_new(cgi_home(), "trash", NULL);
  // Arr[char]
  Arr *fs = file_dir_new(trash_dir);
  // Arr[char]
  Arr *rs = arr_new(free);
  EACH(fs, char, f) {
    char *pname = str_new(f);
    path_name(&pname);
    arr_push(rs, pname);
  }_EACH
  arr_free(fs);
	return rs;
}

static void to_trash(void) {
  char *date = mk_date2_new();
  char *source = path_cat_new(cgi_home(), "data", NULL);
  char *fzip = str_f_new("%s.zip", date);
  char *target = path_cat_new(cgi_home(), "trash", fzip, NULL);
  ext_zip(source, target);
  free(date);
  free(source);
  free(fzip);
  free(target);
}

static void clear_tmp(void) {
  char *dir = path_cat_new(cgi_home(), "tmp", NULL);
  file_del(dir);
  file_mkdir(dir);
  free(dir);
}

static char *unzip(const char *app_name, const char *data_version) {
  char *source = path_cat_new(cgi_home(), "tmp", "back.zip", NULL);
  char *target = path_cat_new(cgi_home(), "tmp", NULL);

  ext_unzip(source, target);

  free(source);
  free(target);

  source = path_cat_new(cgi_home(), "tmp", "data", "version.txt", NULL);
  char *fail = "";
  if (!file_exists(source)) {
    fail = "restore:version does not exist";
  } else {
    char *good_version = str_f_new(
      "%s\nData version: %s\n", app_name, data_version
    );
    char *version = file_read_new(source);
    if (!str_eq(version, good_version)) {
      fail = "restore:version is wrong";
    }
    free(good_version);
    free(version);
  }
  free(source);

  return fail;
}

//   _______________
void backups_process(
//   TTTTTTTTTTTTTTT
  const char *app_name,
  const char *data_version,
  // Map[Js]
  Map *rqm
) {
  char *home = cgi_home();
  CGI_GET_STR(rq, rqm, "rq");

  // -------------------------------------------------------------- backup
  if (str_eq(rq, "backup")) {
    clear_tmp();
    char *d = mk_date_new();
    char *name = str_f_new("%sBackup%s.zip", app_name, d);

    char *source = path_cat_new(home, "data", NULL);
    char *target = path_cat_new(home, "tmp", name, NULL);
    ext_zip(source, target);

    // Map[Js]
    Map *m = map_new(free);
    map_put(m, "name", js_ws_new(name));
    cgi_ok(m);

    free(d);
    free(name);
    free(source);
    free(target);
    map_free(m);

  // -------------------------------------------------_------ restoreStart
  } else if (str_eq(rq, "restoreStart")) {
    clear_tmp();
    char *path = path_cat_new(home, "tmp", "back.zip", NULL);
    FileLck *lck = file_wopen(path);
    file_close(lck);
    cgi_empty();
    free(path);

  // ------------------------------------------------------- restoreAppend
  } else if (str_eq(rq, "restoreAppend")) {
    CGI_GET_STR(data, rqm, "data");
    Bytes *bs_data = b64_decode_bytes_new(data);

    char *path = path_cat_new(home, "tmp", "back.zip", NULL);
    FileLck *lck = file_aopen(path);
    file_write_bin(lck, bs_data);
    file_close(lck);
    cgi_empty();

    free(data);
    bytes_free(bs_data);
    free(path);

  // -------------------------------------------_------------ restoreAbort
  } else if (str_eq(rq, "restoreAbort")) {
    clear_tmp();
    cgi_empty();

  // ---------------------------------------------------------- restoreEnd
  } else if (str_eq(rq, "restoreEnd")) {
    char *fail = unzip(app_name, data_version);

    if (!*fail) {
      char *data = path_cat_new(home, "data", NULL);
      char *tmp_data = path_cat_new(home, "tmp", "data", NULL);
      to_trash();
      file_del(data);
      file_rename(tmp_data, data);
      clear_tmp();
      free(data);
      free(tmp_data);
    }

    // Map[Js]
    Map *m = map_new(free);
    map_put(m, "fail", js_ws_new(fail));
    cgi_ok(m);

    map_free(m);

  // ------------------------------------------------------- autorestore
  } else if (str_eq(rq, "autorestore")) {
    CGI_GET_STR(file, rqm, "file")
    to_trash();

    char *datad = path_cat_new(home, "data", NULL);
    char *backupsd = path_cat_new(home, "backups", file, NULL);
    file_del(datad);
    ext_unzip(backupsd, home);

    cgi_empty();

    free(file);
    free(datad);
    free(backupsd);

  // ------------------------------------------------------- logout
  } else if (str_eq(rq, "logout")) {
    char *datad = path_cat_new(home, "data", NULL);
    char *d = mk_date_new();
    char *file = str_f_new("%s.zip", d);
    char *backupsd = path_cat_new(home, "backups", file, NULL);

    ext_zip(datad, backupsd);
    filter_backups();

    free(datad);
    free(d);
    free(file);
    free(backupsd);

  // --------------------------------------------------------- lists
  } else if (str_eq(rq, "lists")) {
    // Arr[char]
    Arr *backups = read_backups_new();
    // Arr[char]
    Arr *trash = read_trash_new();
    // Map[Js]
    Map *m = map_new(free);
    map_put(m, "backups", arr_to_js_new(backups, (FTO)js_ws_new));
    map_put(m, "trash", arr_to_js_new(trash, (FTO)js_ws_new));
    cgi_ok(m);

    arr_free(backups);
    arr_free(trash);
    map_free(m);

  // ---------------------------------------------------------- clearTrash
  } else if (str_eq(rq, "clearTrash")) {
    char *path = path_cat_new(home, "trash", NULL);
    file_del(path);
    file_mkdir(path);
    return cgi_empty();
    free(path);

  // -------------------------------------------------------- restoreTrash
  } else if (str_eq(rq, "restoreTrash")) {
    CGI_GET_STR(file, rqm, "file");

    char *source = path_cat_new(home, "trash", file, NULL);

    if (!file_exists(source)) {
      char *msg = str_f_new("Trash back '%s' not found", source);
      cgi_error(msg);
      free(msg);
    } else {
      to_trash();
      char *ddata = path_cat_new(home, "data", NULL);
      file_del(ddata);
      ext_unzip(source, home);

      cgi_empty();

      free(ddata);
    }

    free(source);
    free(file);
  } else FAIL(str_f_new("'%s': Unknown request in backups", rq));

  free(rq);
}
