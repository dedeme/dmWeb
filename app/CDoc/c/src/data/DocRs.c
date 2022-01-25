// Copyright 11-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/DocRs.h"
#include "dmc/DEFS.h"
#include "dmc/err.h"

DocRs *docRs_new (char *doc, char *line) {
  DocRs *this = MALLOC(DocRs);
  this->doc = doc;
  this->line = line;
  return this;
}

char *docRs_to_js (DocRs *this) {
  return FAIL("Unimplemented function");
}

DocRs *docRs_from_js (char *js) {
  return FAIL("Unimplemented function");
}
