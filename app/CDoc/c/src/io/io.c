// Copyright 20-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/io.h"
#include "io/indexrd.h"
#include "io/modulerd.h"

IndexTree *io_index (char *path) {
  char *include = path_cat(path, "include", NULL);
  if (!file_exists(include))
    return indexTree_empty();

  // trees id Arr[IndexTree]
  void fn (Arr *trees, char *dir) {
    EACH(file_dir(dir), char, f) {
      char *path = path_cat(dir, f, NULL);
      if (file_is_directory(path)) {
        IndexTree *t = indexTree_new(f, 1, "", arr_new());
        arr_push(trees, t);
        fn(indexTree_trees(t), path);
      } else if (str_ends(f, ".h")) {
        char *doc = indexrd_read(path);
        arr_push(trees, indexTree_new(str_left(f, -2), 0, doc, arr_new()));
      }
    }_EACH
  }
  IndexTree *t = indexTree_empty();
  fn(indexTree_trees(t), include);
  return t;
}

Opt *io_module (char *path) {
  if (!file_exists(path))
    return opt_empty();

  return opt_new(modulerd_read(path));
}

char *io_code (char *path) {
  if (file_exists(path))
    return file_read(path);
  return "";
}
