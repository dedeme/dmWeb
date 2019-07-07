// Copyright 03-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "tests.h"
#include "io/io.h"
#include "io/log.h"
#include "io/nicks.h"
#include "io/quotes.h"
#include "log_tests.h"
#include "nick_tests.h"
#include "quote_tests.h"
#include "quote2_tests.h"
#include "gen_tests.h"
#include "rs_tests.h"
#include "order_tests.h"
#include "facc_tests.h"
#include "nickSets_tests.h"

static void init (void) {
  sys_init("MultiMarket_tests");
  io_init();
  log_init();
  nicks_init();
  quotes_init();
}

int main (int argc, char *argv[]) {
  init();

  log_tests();
  nick_tests();
  quote_tests();
  quote2_tests();
  gen_tests();
  rs_tests();
  order_tests();
  facc_tests();
  nickSets_tests();
}
