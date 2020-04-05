// Copyright 22-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db/fleas/ranking.h"
#include "data/flea/Investor.h"

#include "DEFS.h"

static char *path () {
  return path_cat(sys_home(), RANKING_PATH, "Ranking.db", NULL);
}

static char *pool_path () {
  return path_cat(sys_home(), RANKING_PATH, "Pool.db", NULL);
}

void ranking_init () {
  if (!file_exists(path())) {
    file_write(path(), "[]");
  }
  if (!file_exists(pool_path())) {
    file_write(pool_path(), "[]");
  }
}

// Arr[Investor]
Arr *ranking_read_pool (void) {
  // Arr[js]
  Arr *jss = js_ra((Js *)file_read(pool_path()));
  // Arr[Investor]
  Arr *r = arr_new();
  EACH(jss, Js, js) {
    Investor *investor = opt_nget(investor_from_js_opt(js));
    if (investor) arr_push(r, investor);
  }_EACH
  return r;
}

// investors is Arr[Investor]
void ranking_write_pool (Arr *investors) {
  file_write(pool_path(), (char *)arr_to_js(investors, (FTO)investor_to_js));
}

// Arr[Arr[Investor]]
Arr *ranking_read (void) {
  // Arr[js]
  Arr *jss = js_ra((Js *)file_read(path()));
  // Arr[Arr[Investor]]
  Arr *r = arr_new();
  EACH(jss, Js, js) {
    // Arr[js]
    Arr *jss = js_ra(js);
    // Arr[Investor]
    Arr *invs = arr_new();
    EACH(jss, Js, js) {
      Investor *investor = opt_nget(investor_from_js_opt(js));
      if (investor) arr_push(invs, investor);
    }_EACH
    arr_push(r, invs);
  }_EACH
  return r;
}

// 'ranking' is Arr[Arr[Investor]]
void ranking_write (Arr *ranking) {
  // rentry is Arr[Investor]
  Js *fn (Arr *rentry) { return arr_to_js(rentry, (FTO)investor_to_js); }
  // Arr[Js]
  Arr *jss = arr_map(ranking, (FCOPY) fn);
  file_write(path(), (char *)js_wa(jss));
}
