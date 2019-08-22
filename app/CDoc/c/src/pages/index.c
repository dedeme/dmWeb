// Copyright 20-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pages/index.h"
#include "dmc/cgi.h"
#include "io/dpathsdb.h"
#include "io/io.h"

// mrq is Map[Js]
char *index_process (Map *mrq) {
  CGI_GET_STR(id, mrq)

  // Map[Js]
  Map *rp = map_new();
  Dpath *path = opt_nget(dpathdb_get(id));
  IndexTree *tree = path ? io_index(dpath_path(path)) : indexTree_empty();
  map_put(rp, "tree", indexTree_to_js(tree));
  return cgi_ok(rp);
}

