// Copyright 15-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db/quotesTb.h"
#include "dmc/file.h"
#include "dmc/path.h"
#include "external/quotesReader.h"

static char *path = NULL;

void quotesTb_init (char *parent) {
  path = path_cat(parent, "quotes.tb", NULL);
  if (!file_exists(path)) {
    quotesTb_write(quotesReader_read());
  }
}

/// Read quotes.
Quotes *quotesTb_read(void) {
  return quotes_from_js(file_read(path));
}

/// Write quotes.
void quotesTb_write(Quotes *qs) {
  file_write(path, quotes_to_js(qs));
}
