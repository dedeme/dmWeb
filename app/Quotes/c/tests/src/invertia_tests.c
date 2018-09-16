// Copyright 11-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "invertia_tests.h"
#include "data/Invertia.h"
#include "assert.h"

void invertia_tests_run(void) {
  puts("Invertia tests");

  Server *s = invertia_mk();

  Oaclose *ocloses = server_read_last(s)();
  assert(!oaclose_is_null(ocloses));
  EACH(oaclose_value(ocloses), Close, c) {
    assert(*close_nick(c));
    assert(close_close(c) > -1);
//    printf("%s:%.4f\n", close_nick(c), close_close(c));
  }_EACH

  Oaquote *oaquotes = server_read(s)("adolfo-dominguez/RV011ADOLFOD");
  assert(!oaquote_is_null(oaquotes));
  EACH(oaquote_value(oaquotes), Quote, q) {
    assert(achar_size(str_csplit(quote_to_str(q), ':')) == 7);
//    puts(quote_to_str(q));
  }_EACH

  puts("    Finished");
}
