// Copyright 11-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/DocRs.h"
#include "dmc/DEFS.h"

DocRs *docRs_new (char *doc, char *line) {
  DocRs *this = MALLOC(DocRs);
  this->doc = doc;
  this->line = line;
  return this;
}

