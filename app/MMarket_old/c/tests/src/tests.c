// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "tests.h"
#include "nick_tests.h"
#include "quote_tests.h"
#include "quote2_tests.h"
#include "flea_tests.h"

int main (int argc, char *argv[]) {
  nick_tests();
  quote_tests();
  quote2_tests();
  flea_tests();
}
