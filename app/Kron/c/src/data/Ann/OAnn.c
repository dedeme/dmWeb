// Copyright 25-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Ann/OAnn.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oAnn_OAnn {
  Ann *value;
};

OAnn *oAnn_mk_none() {
  return (OAnn *)opt_mk_none();
}

OAnn *oAnn_mk_some(Ann *value) {
  return (OAnn *)opt_mk_some(value);
}

int oAnn_none(OAnn *opt) {
  return !opt->value;
}

Ann *oAnn_some(OAnn *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

Ann *oAnn_esome (OAnn *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

Ann *oAnn_osome (OAnn *opt, Ann *value) {
  return opt->value ? opt->value : value;
}

Ann *oAnn_nsome (OAnn *opt) {
  return opt->value;
}

char *oAnn_to_js (OAnn *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))ann_to_js);
}

OAnn *oAnn_from_js (char *js) {
  return (OAnn *)opt_from_js(js, (void *(*)(char *))ann_from_js);
}

//--// Not remove

