// Copyright 04-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "nickSets_tests.h"
#include "assert.h"
#include "io/quotes.h"
#include "data/Nick.h"

int fcontains (Arr *nks, Nick *nk) {
  int fc (Nick *nick) { return str_eq(nick_name(nk), nick_name(nick)); }
  return it_contains(arr_to_it(nks), (FPRED)fc);
}

void nickSets_tests() {
  puts("NickSets tests:");

  char *path = path_cat(sys_home(), "data", "quotes", NULL);
  assert(file_exists(path));

  Qmatrix *mx = opt_oget(quotes_closes(), NULL);
  assert(mx);
  assert(arr_size(qmatrix_nicks(mx)) == 50);

  // Returns Arr[Arr[char]]
  NickSets *sets = opt_oget(quotes_sets(), NULL);
  assert(sets);

  // Arr[Nick]
  Arr *all = arr_new();
  EACH(nickSets_win(sets), Nick, nk)
    char *nkname = nick_name(nk);
    int fcontains (Nick *n) { return str_eq(nick_name(n), nkname); }
    if (!it_contains(arr_to_it(all), (FPRED)fcontains)) arr_push(all, nk);
  _EACH

  /*
  puts("XXXXXXXXXXXXXXXXXXXXX");
  Arr *set = nickSets_all(sets);
  EACH(set, Nick, nk) puts(nick_name(nk)); _EACH
  puts("XXXXXXXXXXXXXXXXXXXXX");
  set = nickSets_win(sets);
  EACH(set, Nick, nk) puts(nick_name(nk)); _EACH
  puts("XXXXXXXXXXXXXXXXXXXXX");
  set = nickSets_loss(sets);
  EACH(set, Nick, nk) puts(nick_name(nk)); _EACH
  puts("XXXXXXXXXXXXXXXXXXXXX");
  set = nickSets_semi_win(sets);
  EACH(set, Nick, nk) puts(nick_name(nk)); _EACH
  puts("XXXXXXXXXXXXXXXXXXXXX");
  set = nickSets_semi_loss(sets);
  EACH(set, Nick, nk) puts(nick_name(nk)); _EACH
  puts("XXXXXXXXXXXXXXXXXXXXX");
  */

  EACH(all, Nick, nk)
    if (fcontains(nickSets_win(sets), nk)) {
      assert(!fcontains(nickSets_loss(sets), nk));
    } else {
      assert(fcontains(nickSets_loss(sets), nk));
    }
    if (fcontains(nickSets_semi_win(sets), nk)) {
      assert(!fcontains(nickSets_semi_loss(sets), nk));
    } else {
      assert(fcontains(nickSets_semi_loss(sets), nk));
    }
  _EACH

  puts("    Finished");
}

