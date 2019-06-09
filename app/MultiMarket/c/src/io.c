// Copyright 03-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io.h"
#include "dmc/date.h"
#include "DEFS.h"
#include <unistd.h>

static char *data_dir = NULL;
static char *tmp_dir = NULL;
static int active = 1;

void io_init (void) {
  data_dir = path_cat(sys_home(), "data", NULL);
  tmp_dir = path_cat(sys_home(), "tmp", NULL);
  if (!file_exists(data_dir)) {
    file_mkdir(data_dir);
    file_write(path_cat(data_dir, "version.txt", NULL), DATA_VERSION);

    file_mkdir(tmp_dir);
  }
  file_del(tmp_dir);
  file_mkdir(tmp_dir);
}

void io_set_active (int value) {
  active = value;
}

int io_active (void) {
  return active;
}

char *io_data_dir (void) {
  if (data_dir) return data_dir;

  EXC_ILLEGAL_STATE("io was not intiliazed")
  return NULL; // Never executed
}

char *io_tmp_dir (void) {
  if (tmp_dir) return tmp_dir;

  EXC_ILLEGAL_STATE("io was not intiliazed")
  return NULL; // Never executed
}

void io_clear_tmp (void) {
  char *d = io_tmp_dir();
  file_del(d);
  file_mkdir(d);
}

char *io_time_stamp (void) {
  DateTm *tm = date_tm_now();
  return str_f("%lld", ((long long) tm->tv_sec) + tm->tv_usec);
}
