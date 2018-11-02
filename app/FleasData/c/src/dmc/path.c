// Copyright 17-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "dmc/path.h"
#include <stdlib.h>
#include "dmc/str.h"
#include "dmc/Buf.h"

void path_name (char **path) {
  int ix = str_last_cindex(*path, '/');
  if (ix != -1) {
    str_right(path, ix + 1);
  }
}

void path_parent (char **path) {
  int ix = str_last_cindex(*path, '/');
  if (ix == -1) {
    ix = 0;
  }
  str_left(path, ix);
}

void path_extension (char **path) {
  path_name(path);
  int ix = str_last_cindex(*path, '.');
  if (ix == -1) {
    ix = strlen(*path);
  }
  str_right(path, ix);
}

void path_only_name (char **path) {
  path_name(path);
  int ix = str_last_cindex(*path, '.');
  if (ix != -1) {
    str_left(path, ix);
  }
}

char *path_cat_new (const char *s, const char *more, ...) {
  va_list args;
  char *tmp;

  Buf *bf = buf_new();
  buf_add(bf, s);
  if (*s) {
    buf_cadd(bf, '/');
  }
  buf_add(bf, more);

  va_start(args, more);
  tmp = va_arg(args, char *);
  while (tmp) {
    if (*tmp) {
      buf_cadd(bf, '/');
      buf_add(bf, tmp);
    }
    tmp = va_arg(args, char *);
  }
  va_end(args);

  char *r = buf_to_str_new(bf);
  buf_free(bf);
  return r;
}
