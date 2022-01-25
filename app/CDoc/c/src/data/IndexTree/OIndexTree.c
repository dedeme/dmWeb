// Copyright 25-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/IndexTree/OIndexTree.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oIndexTree_OIndexTree {
  IndexTree *value;
};

OIndexTree *oIndexTree_mk_none() {
  return (OIndexTree *)opt_mk_none();
}

OIndexTree *oIndexTree_mk_some(IndexTree *value) {
  return (OIndexTree *)opt_mk_some(value);
}

int oIndexTree_none(OIndexTree *opt) {
  return !opt->value;
}

IndexTree *oIndexTree_some(OIndexTree *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

IndexTree *oIndexTree_esome (OIndexTree *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

IndexTree *oIndexTree_osome (OIndexTree *opt, IndexTree *value) {
  return opt->value ? opt->value : value;
}

IndexTree *oIndexTree_nsome (OIndexTree *opt) {
  return opt->value;
}

char *oIndexTree_to_js (OIndexTree *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))indexTree_to_js);
}

OIndexTree *oIndexTree_from_js (char *js) {
  return (OIndexTree *)opt_from_js(js, (void *(*)(char *))indexTree_from_js);
}

//--// Not remove

