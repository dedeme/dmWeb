// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db/conftb.h"
#include "dmc/path.h"
#include "dmc/file.h"

static char *conftb_path = NULL;

void conftb_init (char *parent) {
  conftb_path = path_cat(parent, "conf.tb", NULL);
  if (!file_exists(conftb_path)) {
    conftb_write(conf_new("@", "es", 1));
  }
}

char *conftb_read_js (void) {
  return file_read(conftb_path);
}

Conf *conftb_read (void) {
  return conf_from_js(conftb_read_js());
}

void conftb_write (Conf *cf) {
  file_write(conftb_path, conf_to_js(cf));
}
