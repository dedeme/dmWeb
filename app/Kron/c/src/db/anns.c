// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db/anns.h"
#include "dmc/path.h"
#include "dmc/file.h"

static char *anns_path = NULL;

void anns_init (char *parent) {
  anns_path = path_cat(parent, "anns.tb", NULL);
  if (!file_exists(anns_path)) {
    anns_write(aAnn_new());
  }
}

AAnn *anns_read (void) {
  return aAnn_from_js(file_read(anns_path));
}

void anns_write (AAnn *anns) {
  file_write(anns_path, aAnn_to_js(anns));
}
