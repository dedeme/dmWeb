// Copyright 26-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/backupsdb.h"
#include "dmc/cgi.h"
#include "dmc/date.h"
#include "dmc/ext.h"
#include "dmc/b64.h"
#include "main.h"

static char *version_file_name = "version.txt";

static char *mkdate (void) {
  return date_f(date_now(), "%Y%m%d");
}

static char *mkstrtime (void) {
  return str_f("%ld", date_now());
}

char *version (void) {
  return str_f("%s\nData version: %s\n", main_app_name(), main_data_version());
}

static char *mkpath (char *dir) {
  char *r = path_cat(cgi_home(), dir, NULL);
  if (!file_exists(r)) {
    file_mkdir(path_cat(cgi_home(), dir, NULL));
  }
  return r;
}

static char *mkpath_backups (void) {
  return mkpath("backups");
}

static char *mkpath_data (void) {
  char *r = mkpath("data");
  char *vf = path_cat(r, version_file_name, NULL);
  if (!file_exists(vf))
    file_write(vf, version());
  return r;
}

static char *mkpath_trash (void) {
  return mkpath("trash");
}

static void clear_trash (void) {
  file_del(path_cat(cgi_home(), "trash", NULL));
}

static void to_trash (void) {
  ext_zip(
    mkpath_data(),
    path_cat(mkpath_trash(), str_f("%s.zip", mkstrtime()), NULL)
  );
}

static char *mkpath_tmp (void) {
  return mkpath("tmp");
}

static void clear_tmp (void) {
  file_del(path_cat(cgi_home(), "tmp", NULL));
}

static char *back_file (void) {
  return path_cat(mkpath_tmp(), "back.zip", NULL);
}

// Arr<char>
Arr *backupsdb_backups (void) {
  return file_dir(mkpath_backups());
}

// Arr<char>
Arr *backupsdb_trash (void) {
  return file_dir(mkpath_trash());
}

char *backupsdb_mkbackup (void) {
  clear_tmp();
  char *name = str_f("%sBackup%s.zip", main_app_name(), mkdate());
  ext_zip(mkpath_data(), path_cat(mkpath_tmp(), name, NULL));
  return name;
}

void backupsdb_restore_start() {
  clear_tmp();
  file_close(file_wopen(back_file()));
}

void backupsdb_restore_abort() {
  clear_tmp();
}

void backupsdb_restore_append(char *data) {
  FileLck *lck = file_aopen(back_file());
  file_write_bin(lck, b64_decode_bytes(data));
  file_close(lck);
}

char *backupsdb_restore_end (void) {
  char *tmp = mkpath_tmp();
  ext_unzip(back_file(), tmp);
  char *vf = path_cat(tmp, "data", version_file_name, NULL);
  if (!file_exists(vf)) {
    clear_tmp();
    return "missing";
  }
  if (!str_eq(file_read(vf), version())) {
    clear_tmp();
    return "wrong";
  }

  char *data = mkpath_data();
  to_trash();
  file_del(data);
  file_rename(path_cat(tmp, "data", NULL), data);
  clear_tmp();
  return "";
}

void backupsdb_clear_trash (void) {
  clear_trash();
}

void backupsdb_restore_trash (char *file) {
  char *f = path_cat(mkpath_trash(), file, NULL);
  if (file_exists(f)) {
    char *tmp = mkpath_tmp();
    ext_unzip(f, tmp);
    char *data = mkpath_data();
    to_trash();
    file_del(data);
    file_rename(path_cat(tmp, "data", NULL), data);
    clear_tmp();
  }
}

void backupsdb_mkautobackup (void) {
  ext_zip(
    mkpath_data(),
    path_cat(mkpath_backups(), str_f("%s.zip", mkdate()), NULL)
  );
}

void backupsdb_autorestore (char *file) {
  char *f = path_cat(mkpath_backups(), file, NULL);
  if (file_exists(f)) {
    char *tmp = mkpath_tmp();
    ext_unzip(f, tmp);
    char *data = mkpath_data();
    to_trash();
    file_del(data);
    file_rename(path_cat(tmp, "data", NULL), data);
    clear_tmp();
  }
}
