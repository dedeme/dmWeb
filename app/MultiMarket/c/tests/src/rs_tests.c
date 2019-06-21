// Copyright 13-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "rs_tests.h"
#include "assert.h"
#include "data/Rs.h"

static RsBests *mk_rsBests (int id) {
  Darr *params = darr_new();
  darr_push(params, 0.1);
  return rsBests_new("20010102", rsWeb_new(
    rs_new(
      flea_new("20010101", 23, id, 1),
      rsAssets_new(20, 10, 10),
      rsProfits_new(0.1, 0.1, 0.1)
    ),
    params
  ));
}

void rs_tests() {
  puts("Rs tests:");

  // Arr[RsBests]
  Arr *rsB = arr_new();
  arr_push(rsB, mk_rsBests(2));
  arr_push(rsB, mk_rsBests(2));
  arr_push(rsB, mk_rsBests(2));
  arr_push(rsB, mk_rsBests(3));
  arr_push(rsB, mk_rsBests(1));
  arr_push(rsB, mk_rsBests(1));

  // Arr[RsBests]
  Arr *rsB2 = rsBests_distinct(rsB);
  assert(arr_size(rsB2) == 3);

  assert(flea_id(rs_flea(rsWeb_result(rsBests_result(arr_get(rsB2, 0))))) == 2);
  assert(flea_id(rs_flea(rsWeb_result(rsBests_result(arr_get(rsB2, 1))))) == 3);
  assert(flea_id(rs_flea(rsWeb_result(rsBests_result(arr_get(rsB2, 2))))) == 1);

  puts("    Finished");
}


