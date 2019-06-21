// Copyright 11-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "gen_tests.h"
#include "assert.h"
#include "data/Gen.h"

void gen_tests() {
  puts("Gen tests:");
  // Arr*[Darr]
  Arr *gens = arr_new();

  REPEAT(100)
    Gen *g = gen_new(4);
    assert(gen_n(g) == 4);
    arr_push(gens, gen_mutate(g));
  _REPEAT

  RANGE0(i, 100)
    Gen *g = arr_get(gens, i);
    Js *js = gen_to_js(g);
    Gen *g2 = gen_from_js(js);
    Js *js2 = gen_to_js(g2);
    assert(str_eq((char *)js, (char *)js2));
  _RANGE

  puts("    Finished");
}

