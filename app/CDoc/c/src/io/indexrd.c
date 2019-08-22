// Copyright 20-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/indexrd.h"

char *indexrd_read(char *path) {
  char *r = "";
  if (!file_exists(path)) return r;

  FileLck *f = file_ropen(path);
  char *l = file_read_line(f);
  while (*l) {
    l = str_trim(l);
    if (*l == '#') break;
    if (str_starts(l, "///")) {
      r = str_ltrim(str_right(l, 3));
      break;
    }
    l = file_read_line(f);
  }
  return r;
}
