// Copyright 07-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "rank_tests.h"
#include "assert.h"
#include "data/Rank.h"

void rank_tests (void) {
  puts("Rank tests:");

  // Arr[RankAssets]
  Arr *ar1 = arr_new();
  arr_push(ar1, rankAssets_new("01012019", 30.0));
  arr_push(ar1, rankAssets_new("02012019", 31.0));
  arr_push(ar1, rankAssets_new("03012019", 32.0));
  arr_push(ar1, rankAssets_new("04012019", 33.0));

  // Arr[RankAssets]
  Arr *ar2 = arr_new();
  arr_push(ar2, rankAssets_new("01012019", 28.0));
  arr_push(ar2, rankAssets_new("02012019", 30.0));
  arr_push(ar2, rankAssets_new("03012019", 32.0));
  arr_push(ar2, rankAssets_new("04012019", 34.0));

  // Arr[RankAssets]
  Arr *ar3 = arr_new();
  arr_push(ar3, rankAssets_new("01012019", 40.0));
  arr_push(ar3, rankAssets_new("02012019", 30.0));
  arr_push(ar3, rankAssets_new("03012019", 32.0));
  arr_push(ar3, rankAssets_new("04012019", 20.0));

  // Arr[RankAssets]
  Arr *ar4 = arr_new();
  arr_push(ar4, rankAssets_new("01012019", 10.0));
  arr_push(ar4, rankAssets_new("02012019", 10.0));
  arr_push(ar4, rankAssets_new("03012019", 12.0));
  arr_push(ar4, rankAssets_new("04012019", 10.0));

  // Arr[RankAssets]
  Arr *ar5 = arr_new();
  arr_push(ar5, rankAssets_new("01012019", 50.0));
  arr_push(ar5, rankAssets_new("02012019", 50.0));
  arr_push(ar5, rankAssets_new("03012019", 52.0));
  arr_push(ar5, rankAssets_new("04012019", 52.0));

  // Arr[Arr[RankAssets]]
  Arr *as = arr_new();
  arr_push(as, ar1);
  arr_push(as, ar2);
  arr_push(as, ar3);
  arr_push(as, ar4);
  arr_push(as, ar5);

  // Arr[Arr[RankPositions]]
  Arr *ps = rank_mk_positions(as);

  assert(str_eq(
    rankPosition_date(arr_get(arr_get(ps, 0), 0)), "01012019"
  ));
  assert(
    rankPosition_position(arr_get(arr_get(ps, 0), 0)) == 2
  );
  assert(str_eq(
    rankPosition_date(arr_get(arr_get(ps, 0), 1)), "02012019"
  ));
  assert(
    rankPosition_position(arr_get(arr_get(ps, 0), 1)) == 1
  );
  assert(str_eq(
    rankPosition_date(arr_get(arr_get(ps, 0), 2)), "03012019"
  ));
  assert(
    rankPosition_position(arr_get(arr_get(ps, 0), 2)) == 1
  );
  assert(str_eq(
    rankPosition_date(arr_get(arr_get(ps, 0), 3)), "04012019"
  ));
  assert(
    rankPosition_position(arr_get(arr_get(ps, 0), 3)) == 2
  );

  // ----------------------------

  assert(str_eq(
    rankPosition_date(arr_get(arr_get(ps, 1), 0)), "01012019"
  ));
  assert(
    rankPosition_position(arr_get(arr_get(ps, 1), 0)) == 3
  );
  assert(str_eq(
    rankPosition_date(arr_get(arr_get(ps, 1), 1)), "02012019"
  ));
  assert(
    rankPosition_position(arr_get(arr_get(ps, 1), 1)) == 2
  );
  assert(str_eq(
    rankPosition_date(arr_get(arr_get(ps, 1), 2)), "03012019"
  ));
  assert(
    rankPosition_position(arr_get(arr_get(ps, 1), 2)) == 1
  );
  assert(str_eq(
    rankPosition_date(arr_get(arr_get(ps, 1), 3)), "04012019"
  ));
  assert(
    rankPosition_position(arr_get(arr_get(ps, 1), 3)) == 1
  );

  // ----------------------------

  assert(str_eq(
    rankPosition_date(arr_get(arr_get(ps, 2), 0)), "01012019"
  ));
  assert(
    rankPosition_position(arr_get(arr_get(ps, 2), 0)) == 1
  );
  assert(str_eq(
    rankPosition_date(arr_get(arr_get(ps, 2), 1)), "02012019"
  ));
  assert(
    rankPosition_position(arr_get(arr_get(ps, 2), 1)) == 2
  );
  assert(str_eq(
    rankPosition_date(arr_get(arr_get(ps, 2), 2)), "03012019"
  ));
  assert(
    rankPosition_position(arr_get(arr_get(ps, 2), 2)) == 1
  );
  assert(str_eq(
    rankPosition_date(arr_get(arr_get(ps, 2), 3)), "04012019"
  ));
  assert(
    rankPosition_position(arr_get(arr_get(ps, 2), 3)) == 3
  );

  // ----------------------------

  assert(str_eq(
    rankPosition_date(arr_get(arr_get(ps, 3), 0)), "01012019"
  ));
  assert(
    rankPosition_position(arr_get(arr_get(ps, 3), 0)) == 4
  );
  assert(str_eq(
    rankPosition_date(arr_get(arr_get(ps, 3), 1)), "02012019"
  ));
  assert(
    rankPosition_position(arr_get(arr_get(ps, 3), 1)) == 4
  );
  assert(str_eq(
    rankPosition_date(arr_get(arr_get(ps, 3), 2)), "03012019"
  ));
  assert(
    rankPosition_position(arr_get(arr_get(ps, 3), 2)) == 4
  );
  assert(str_eq(
    rankPosition_date(arr_get(arr_get(ps, 2), 3)), "04012019"
  ));
  assert(
    rankPosition_position(arr_get(arr_get(ps, 3), 3)) == 4
  );

  // ----------------------------

  assert(str_eq(
    rankPosition_date(arr_get(arr_get(ps, 4), 0)), "01012019"
  ));
  assert(
    rankPosition_position(arr_get(arr_get(ps, 4), 0)) == 0
  );
  assert(str_eq(
    rankPosition_date(arr_get(arr_get(ps, 4), 1)), "02012019"
  ));
  assert(
    rankPosition_position(arr_get(arr_get(ps, 4), 1)) == 0
  );
  assert(str_eq(
    rankPosition_date(arr_get(arr_get(ps, 4), 2)), "03012019"
  ));
  assert(
    rankPosition_position(arr_get(arr_get(ps, 4), 2)) == 0
  );
  assert(str_eq(
    rankPosition_date(arr_get(arr_get(ps, 4), 3)), "04012019"
  ));
  assert(
    rankPosition_position(arr_get(arr_get(ps, 4), 3)) == 0
  );

  puts("    Finished");
}


