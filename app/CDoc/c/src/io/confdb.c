// Copyright 18-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/confdb.h"
#include "dmc/cgi.h"

Conf *confdb_load (void) {
  char *path = path_cat(cgi_home(), "conf.db", NULL);
  if (file_exists(path))
    return conf_from_js((Js *)file_read(path));
  return conf_new("@", "es", 1);
}

void confdb_save (Conf *cf) {
  char *path = path_cat(cgi_home(), "conf.db", NULL);
  file_write(path, (char *)conf_to_js(cf));
}
