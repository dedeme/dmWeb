// Copyright 05-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/backups.h"
#include "dmc/date.h"
#include "dmc/ext.h"
#include "dmc/b64.h"
#include "io/io.h"
#include "io/trash.h"
#include "DEFS.h"

static char *backups_dir = NULL;

void backups_init (void) {
  backups_dir = path_cat(sys_home(), "backups", NULL);
  if (!file_exists(backups_dir)) {
    file_mkdir(backups_dir);
  }
}

static char *backups_d (void) {
  if (backups_dir) return backups_dir;

  EXC_ILLEGAL_STATE("backups was not intiliazed")
  return NULL; // Never executed
}

// Arr[char]
Arr *backups_list (void) {
  return file_dir(backups_d());
}

static void backups_makef (Tp *t) {
  ext_zip(tp_e1(t), tp_e2(t));
}

char *backups_make (void) {
  io_clear_tmp();
  char *name = str_f("%sBackup%s.zip", APP_NAME, date_to_str(date_now()));
  char *source = io_data_dir();
  char *target = path_cat(io_tmp_dir(), name, NULL);
  async_thread_detached((FPROC)backups_makef, tp_new(source, target));
  return name;
}

void backups_make_automatic (void) {
  char *zip = str_f("%s.zip", date_to_str(date_now()));
  char *source = io_data_dir();
  char *target = path_cat(backups_d(), zip, NULL);
  ext_zip(source, target);
}

void backups_restore_start (void) {
  io_clear_tmp();
  file_write(path_cat(io_tmp_dir(), "back.zip", NULL), "");
}

void backups_restore_append (char *data) {
  FileLck *f = file_aopen(path_cat(io_tmp_dir(), "back.zip", NULL));
  file_write_bin(f, b64_decode_bytes(data));
  file_close(f);
}

void backups_restore_abort (void) {
  io_clear_tmp();
}

char *backups_restore_end (void) {
  char *target = io_tmp_dir();
  char *source = path_cat(target, "back.zip", NULL);

  ext_unzip(source, target);

  char *version = path_cat(target, "data", "version.txt", NULL);

  if (!file_exists(version)) return "missing";
  if (!str_eq(file_read(version), DATA_VERSION)) return "wrong";

  trash_zip_data();
  char *data_dir = io_data_dir();
  file_del(data_dir);
  file_rename(path_cat(target, "data", NULL), data_dir);
  return "";
}

void backups_autorestore (char *file) {
  char *f = path_cat(backups_d(), file, NULL);
  if (!file_exists(f))
    EXC_IO(str_f("'%s' not found", f))

  trash_zip_data();
  file_del(io_data_dir());
  ext_unzip(f, sys_home());
}
