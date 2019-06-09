// Copyright 03-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "tests.h"
#include "io.h"
#include "io/log.h"
#include "log_tests.h"
#include "nick_tests.h"
#include "quote_tests.h"
#include "quote2_tests.h"

static void init (void) {
  sys_init("MultiMarket_tests");
  io_init();
  log_init();
}

int main (int argc, char *argv[]) {
  init();
/*
  log_tests();
  nick_tests();
  quote_tests();
*/  quote2_tests();

}
