// Copyright 17-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "tests.h"
#include "dmc/cgi.h"
#include "match_tests.h"
#include "io_tests.h"


static char *app_dir = "dmcgi/MarketLeague";
static time_t expiration = 3600;

int main (int argc, char *argv[]) {
  GC_NEW

  cgi_init(gc, app_dir, expiration);
//  match_tests();
  io_tests();

  GC_FREE
  return 0;
}
