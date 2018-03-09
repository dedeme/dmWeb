// Copyright 09-Mar-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "backups.h"

static char *mk_date() {
  return date_format(date_now(), "%Y%m%d");
}

/*
static char *mk_date2() {
  Date t = date_now();
  return str_printf("%s-%ld", date_format(t, "%Y%m%d"), t);
}
*/

void backups_auto(Cgi *cgi) {
  char *home = cgi_home(cgi);
  ext_zip(
    path_cat(home, "data", NULL),
    path_cat(home, "backups", str_printf("%s.zip", mk_date()), NULL)
  );

  Date t0 = date_now();
  Date t1 = date_add(t0, -7);
  Date t2 = date_new(date_day(t0), date_month(t0), date_year(t0) - 1);

  char *d1 = date_format(t1, "%Y%m%d");
  char *d2 = date_format(t2, "%Y%m%d");

  Arr/*char*/ *fs = file_dir(path_cat(home, "backups", NULL));
  arr_sort_str(fs);
  char *previous = "        ";
  EACH(fs, char, f) {
    char *name = path_only_name(f);
    if (strcmp(name, d2) < 0) {
      if (!strcmp(str_sub(name, 0, 4), str_sub(previous, 0, 4))) {
        file_del(f);
      }
    } else if (strcmp(name, d1) < 0) {
      if (!strcmp(str_sub(name, 0, 6), str_sub(previous, 0, 6))) {
        file_del(f);
      }
    }
    previous = name;
  }_EACH

}
