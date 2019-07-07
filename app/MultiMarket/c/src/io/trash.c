// Copyright 05-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/trash.h"
#include "dmc/ext.h"
#include "io/io.h"

static char *trash_dir = NULL;

void trash_init (void) {
  trash_dir = path_cat(sys_home(), "trash", NULL);
  if (!file_exists(trash_dir)) {
    file_mkdir(trash_dir);
  }
}

static char *trash_d (void) {
  if (trash_dir) return trash_dir;

  EXC_ILLEGAL_STATE("backups was not intiliazed")
  return NULL; // Never executed
}

// Arr[char]
Arr *trash_list (void) {
  return file_dir(trash_d());
}

void trash_zip_data (void) {
  char *zip = str_f("%s.zip", io_time_stamp());
  char *source = io_data_dir();
  char *target = path_cat(trash_d(), zip, NULL);
  ext_zip(source, target);
}

void trash_clear (void) {
  char *d = trash_d();
  file_del(d);
  file_mkdir(d);
}

void trash_restore (char *file) {
  char *f = path_cat(trash_d(), file, NULL);
  if (!file_exists(f))
    EXC_IO(str_f("'%s' not found", f))

  trash_zip_data();
  file_del(io_data_dir());
  ext_unzip(f, sys_home());
}
