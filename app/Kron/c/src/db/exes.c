// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db/exes.h"
#include "dmc/path.h"
#include "dmc/file.h"

static char *exes_path = NULL;

void exes_init (char *parent) {
  exes_path = path_cat(parent, "exes.tb", NULL);
  if (!file_exists(exes_path)) {
    exes_write(aExe_new());
  }
}

AExe *exes_read (void) {
  return aExe_from_js(file_read(exes_path));
}

void exes_write (AExe *exes) {
  file_write(exes_path, aExe_to_js(exes));
}
