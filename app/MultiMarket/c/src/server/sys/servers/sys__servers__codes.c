// Copyright 21-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys/servers/sys__servers__codes.h"
#include "dmc/cgi.h"
#include "io/servers.h"
#include "data/Server.h"

char *sys__servers__codes_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq)

  if (str_eq(rq, "setCodes")) {
    CGI_GET_INT(id, mrq)
    CGI_GET_ARR(codes, mrq)
    void fn () {
      servers_set_codes(
        id,
        arr_from_it(it_map(arr_to_it(codes), (FCOPY)serverCode_from_js))
      );
    }
    asyncActor_wait(ac, fn);
    return cgi_empty();
  }

  EXC_ILLEGAL_ARGUMENT("rq", "setCodes", rq)
  return NULL; // Unreachable

}

