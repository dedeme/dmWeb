// Copyright 10-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "readers/indexRd.h"
#include "dmc/str.h"
#include "dmc/path.h"
#include "dmc/file.h"
#include "db/dpaths.h"
#include "data/Dpath/ADpath.h"

static char *doc_read(char *path) {
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

static void complete_trees (AIndexTree *trees, char *dir) {
  Achar *files = file_dir(dir);
  char **p = files->es;
  while (p < files->end) {
    char *fname = *p++;
    char *fpath = path_cat(dir, fname, NULL);
    if (file_is_directory(fpath)) {
      IndexTree *t = indexTree_new(fname, ochar_mk_none(), aIndexTree_new());
      aIndexTree_push(trees, t);
      complete_trees(t->trees, fpath);
    } else if (str_ends(fname, ".h")) {
      aIndexTree_push(trees, indexTree_new(
        str_left(fname, -2),
        ochar_mk_some(doc_read(fpath)),
        aIndexTree_new()
      ));
    }
  }
}

OIndexTree *indexRd_read(char *lib) {
  /**/int find_lib(Dpath *p) { return str_eq(p->id, lib); }
  Dpath *lb = oDpath_nsome(aDpath_find(dpaths_read(), find_lib));

  if (lb) {
    char *ipath = path_cat(lb->path, "include", NULL);
    if (file_exists(ipath)) {
      IndexTree *r = indexTree_new("", ochar_mk_none(), aIndexTree_new());
      complete_trees(r->trees, ipath);
      return oIndexTree_mk_some(r);
    }
  }
  return oIndexTree_mk_none();
}
