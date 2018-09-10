// Copyright 06-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "index.h"
#include "dmc/Tuples.h"
#include "dmc/ct/Ajson.h"
#include "db.h"

char *tabs(int n, char *tx) {
  Buf *bf = buf_new();
  RANGE0(i, n) {
    buf_add(bf, "&nbsp;&nbsp;&nbsp;&nbsp;");
  }_RANGE
  return str_cat(buf_to_str(bf), tx, NULL);
}

Tp/*char, char*/ *read_file(char *path) {
  char *name = path_only_name(path);
  LckFile *lck = file_ropen(path);
  char *doc = "";
  char *l = file_read_line(lck);
  while (*l) {
    l = str_trim(l);
    if (str_starts(l, "/**")) {
      l = str_trim(str_sub_end(l, 3));
      if (!*l) {
        l = str_trim(file_read_line(lck));
        if (*l == '*') {
          l = str_ltrim(str_sub_end(l, 1));
        }
      }
      int ix = str_index(l, ".<");
      if (ix == -1) {
        ix = str_index(l, "*/");
        if (ix == -1) {
          doc = l;
        } else {
          doc = str_rtrim(str_sub(l, 0, ix));
        }
      } else {
        doc = str_sub(l, 0, ix + 1);
      }
      break;
    }
    l = file_read_line(lck);
  }
  file_close(lck);
  return tp_new(name, doc);
}

Arr/*Tp3[char , char, char]*/ *read_dir(int level, char *prefix, char *path) {
  Arr/*Tp[char, char, char]*/ *r = arr_new();
  Arr/*Tp[char, char]*/ *files = arr_new();
  Achar *dirs = achar_new();
  EACH(file_dir(path), char, p) {
    if (file_is_directory(p)){
      achar_add(dirs, p);
    } else {
      arr_add(files, read_file(p));
    }
  }_EACH

  bool cmp_files(void *tp1, void *tp2) {
    char *id1 = tp_e1(tp1);
    char *id2 = tp_e1(tp2);
    return str_cmp(str_to_upper(id1), str_to_upper(id2)) > 0;
  }
  arr_sort(files, cmp_files);

  bool cmp_dirs(char *id1, char *id2) {
    return str_cmp(str_to_upper(id1), str_to_upper(id2)) > 0;
  }
  achar_sortf(dirs, cmp_dirs);
  achar_reverse(dirs);

  EACH(files, Tp, tp) {
    arr_add(r, tp3_new(
      tabs(level, tp_e1(tp)),
      *prefix ? path_cat(prefix, tp_e1(tp), NULL) : tp_e1(tp),
      tp_e2(tp)
    ));
  }_EACH

  EACH(dirs, char, d) {
    char *only_name = path_only_name(d);
    arr_add(r, tp3_new("", tabs(level, only_name), ""));
    arr_add_arr(r, read_dir(
      level + 1,
      *prefix ? path_cat(prefix, only_name, NULL) : only_name,
      d
    ));
  }_EACH

  return r;
}

CgiRp *index_process(Mjson *mrq) {
  char *path = jmap_gstring(mrq, "path");

  if (!file_is_directory(path)) {
    Mjson *m = mjson_new();
    mjson_put(m, "entries", json_wnull());
    return cgi_ok(m);
  }

  Ajson *entries = ajson_new();
  EACH(read_dir(0, "", path), Tp3, tp) {
    char *id = (char *)tp3_e1(tp);
    char *link = (char *)tp3_e2(tp);
    char *doc = (char *)tp3_e3(tp);

    Ajson *atp = ajson_new();
    jarr_astring(atp, id);
    jarr_astring(atp, link);
    jarr_astring(atp, doc);

    ajson_add(entries, json_warray(atp));
  }_EACH
  Mjson *m = mjson_new();
  mjson_put(m, "entries", json_warray(entries));
  return cgi_ok(m);
}
