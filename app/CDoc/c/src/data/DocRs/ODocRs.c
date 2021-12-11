// Copyright 11-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/DocRs/ODocRs.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"

struct oDocRs_ODocRs {
  DocRs *value;
};

ODocRs *oDocRs_mk_none() {
  return (ODocRs *)opt_mk_none();
}

ODocRs *oDocRs_mk_some(DocRs *value) {
  return (ODocRs *)opt_mk_some(value);
}

int oDocRs_none(ODocRs *opt) {
  return !opt->value;
}

DocRs *oDocRs_some(ODocRs *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

DocRs *oDocRs_esome (ODocRs *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

DocRs *oDocRs_osome (ODocRs *opt, DocRs *value) {
  return opt->value ? opt->value : value;
}

DocRs *oDocRs_nsome (ODocRs *opt) {
  return opt->value;
}

char *oDocRs_to_js (ODocRs *opt, char *(*to)(DocRs *e)) {
  return opt->value ? to(opt->value) : "null";
}

ODocRs *oDocRs_from_js (char *js, DocRs *(*from)(char *jse)) {
  return strcmp(js, "null") ? oDocRs_mk_some(from(js)) : oDocRs_mk_none();
}

//--// Not remove

