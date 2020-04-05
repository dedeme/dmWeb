// Copyright 20-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "flea_tests.h"
#include "assert.h"
#include "data/flea/Flea.h"

void flea_tests (void) {
  puts("Flea tests:");

  Gen *g1 = gen_new(2, (double[]){1.0, 2.0});
  Gen *g2 = gen_new(2, (double[]){1.0, 1.0});
  Gen *g3 = gen_new(2, (double[]){1.0, 2.0});

  Flea *f1 = flea_new("20200131", 214, 1, g1);
  Flea *f2 = flea_new("20200130", 214, 2, g2);
  Flea *f3 = flea_new("20200112", 214, 3, g3);

  assert(flea_eq(f1, f1));
  assert(!flea_eq(f1, f2));
  assert(flea_eq(f1, f3));

  // Arr[Flea]
  Arr *a = arr_new();
  arr_push(a, f1);
  arr_push(a, f2);
  arr_push(a, f3);
  arr_push(a, f2);
  flea_remove_duplicates(a);
  assert(arr_size(a) == 2);
  Flea *af0 = arr_get(a, 0);
  Flea *af1 = arr_get(a, 1);
  assert(str_eq(flea_name(af0), "20200112-214-3"));
  assert(str_eq(flea_name(af1), "20200130-214-2"));

  puts("    Finished");
}

