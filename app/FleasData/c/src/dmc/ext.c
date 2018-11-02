// Copyright 18-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "dmc/ext.h"
#include "dmc/std.h"


char *ext_wget_new(const char *url) {
  char *cmd = str_f_new("wget -q --no-cache -O - %s", url);
  char *r = sys_cmd_new_null(cmd);
  free(cmd);
  if (!r) {
    r = str_new("");
  }
  return r;
}

char *ext_zenity_entry_new(const char *title, const char *prompt) {
  char *cmd = str_f_new(
    "zenity --entry --title=\"%s\" --text=\"%s\"", title, prompt
  );
  char *rt = sys_cmd_new_null(cmd);
  free(cmd);
  if (!rt) {
    FAIL("Fail running zenity.")
  }
  // Arr[char]
  Arr *parts = str_csplit_trim_new(rt, '\n');
  free(rt);
  char *s = "";
  EACH(parts, char, l) {
    if (!str_starts(l, "Gtk-")) {
      s = l;
      break;
    }
  }_EACH
  char *r = str_new(s);
  free(parts);
  return r;
}

void ext_zenity_msg(const char *icon, const char *text) {
  char *cmd = str_f_new(
    "zenity --info --icon-name=\"%s\" --text=\"%s\"", icon, text
  );
  char *rt = sys_cmd_new_null(cmd);
  free(cmd);
  if (!rt) {
    FAIL("Fail running zenity.")
  }
  free(rt);
}

void ext_pdf(
  const char *tx_source,
  const char *file_target,
  const char *options
) {
  char *tsource0 = str_new("libdm");
  file_tmp(&tsource0);
  char *tsource = str_f_new("%s.html", tsource0);
  file_write(tsource, tx_source);
  free(tsource0);


  char *ttarget0 = str_new("libdm");
  file_tmp(&ttarget0);
  char *ttarget = str_f_new("%s.pdf", ttarget0);
  free(ttarget0);

  char *cmd = str_f_new(
    "pdfPrinter -s %s -t %s %s 2>&1", tsource, ttarget, options
  );
  char *rt = sys_cmd_new_null(cmd);
  free(cmd);
  if (!rt) {
    free(tsource);
    free(ttarget);
    FAIL("Fail running pdfPrinter.")
  }

  file_del(tsource);
  free(tsource);

  if (!file_exists(ttarget)) {
    FAIL(rt)
  }
  free(rt);

  file_copy(ttarget, file_target);
  file_del(ttarget);
  free(ttarget);
}

void ext_zip(const char *source, const char *target) {
  char *cd = file_cwd_new();
  char *tg = str_new(target);
  if (*tg != '/') {
    char *t = path_cat_new(cd, tg, NULL);
    free(tg);
    tg = t;
  }
  char *parent = str_new(source);
  path_parent(&parent);
  char *name = str_new(source);
  path_name(&name);
  file_cd(parent);

  char *cmd = str_f_new("zip -q %s %s 2>&1", tg, name);
  if (file_is_directory(name)) {
    free(cmd);
    cmd = str_f_new("zip -q -r %s %s 2>&1", tg, name);
  }

  char *rt = sys_cmd_new_null(cmd);
  free(cmd);
  if (!rt) {
    free(parent);
    free(name);
    free(tg);
    free(cd);
    FAIL("Fail running zip.")
  }

  file_cd(cd);
  free(parent);
  free(name);
  free(cd);

  if (!file_exists(tg)) {
    if (!*rt) {
      free(rt);
      rt = str_new("Unknown error");
    }
    FAIL(rt)
  }
  free(rt);
  free(tg);
}

void ext_unzip(const char *source, const char *target) {
  if (!file_is_directory(target)) {
    FAIL(str_f_new("'%s' is not a directory", target))
  }

  char *cmd = str_f_new("unzip -q -o %s -d %s 2>&1", source, target);
  char *rt = sys_cmd_new_null(cmd);
  free(cmd);
  if (!rt) {
    FAIL("Fail running unzip.")
  }

  if (*rt) {
    FAIL(rt)
  }
  free(rt);
}

