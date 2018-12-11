// Copyright 30-Aug-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "main.h"
#include "dmc/std.h"
#include "data/db.h"
#include "finanzas_tests.h"
#include "invertia_tests.h"
#include "infomercados_tests.h"

int main (int argc, char **argv) {
  sys_init("Quotes2_test");
  db_init(sys_home());

  finanzas_tests_run();
  invertia_tests_run();
//  infomercados_tests_run();

  return 0;
}
