// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "dmc/sys.h"
#include <locale.h>
#include <unistd.h>
#include <pwd.h>
#include <execinfo.h>
#include "dmc/std.h"
#include "dmc/rnd.h"

static struct {
  char *home;
  char *uname;
} sys;

void sys_init (const char *path) {
  rnd_init();

  uid_t uid = getuid();
  struct passwd *udata = getpwuid(uid);
  sys.home = path_cat_new(udata->pw_dir, ".dmCApp", path, NULL);
  sys.uname = str_new(udata->pw_name);
  file_mkdir(sys.home);
}

void sys_end(void) {
  free(sys.home);
  free(sys.uname);
}

char *sys_home (void) {
  return sys.home;
}

char *sys_uname (void) {
  return sys.uname;
}

void sys_locale (const char *language) {
  setlocale (LC_ALL, language);
}

char *sys_cmd_new_null(const char *command) {
  char *c = str_f_new("%s 2>&1", command);
  FILE *fp = popen(c, "r");
  free(c);

  if (!fp)
    return NULL;

  Buf *bf = buf_new();
  char *line = NULL;
  size_t len = 0;
  while (getline(&line, &len, fp) != -1) {
    buf_add(bf, line);
    free(line);
    line = NULL;
  }
  free(line);
  char *r = buf_to_str_new(bf);
  buf_free(bf);
  fclose(fp);
  return r;
}

void sys_fail(
  const char *message,
  const char *file,
  const char *func,
  int line
) {
  void *array[25];
  size_t size;
  char **strings;
  size = backtrace(array, 25);
  strings = backtrace_symbols(array, size);

  char *name = str_new(file);
  path_name(&name);
  printf("%s:%d:[%s]:\n%s\n", name, line, func, message);
  free(name);
  RANGE0(i, size) {
    puts(strings[i]);
  }_RANGE
  free(strings);

  exit(1);
}
