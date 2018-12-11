// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "core/chpass.h"

CgiRp *chpass_process(Mjson *rq) {
  return cgi_change_pass(
    jmap_gstring(rq, "user"),
    jmap_gstring(rq, "pass"),
    jmap_gstring(rq, "newPass")
  );
}
