// Copyright 29-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "scheduler/historic.h"
#include "io/nicks.h"
#include "io/servers.h"
#include "io/log.h"
#include "io/net.h"
#include "io/io.h"

void historic_update (AsyncActor *ac) {
  // Arr[Nick]
  Arr *nicks = NULL;
  int model_id = -1;
  void fn () {
    nicks = nicks_list();
    model_id = nicks_model();
  }
  asyncActor_wait(ac, fn);

  Nick *model = NULL;
  EACH(nicks, Nick, nk)
    if (nick_id(nk) == model_id) {
      model = nk;
      break;
    }
  _EACH
  if (!model) {
    model = arr_get(nicks, 0);
  }

  void fn2 () {
    log_info(str_f("Reading model %s", nick_name(model)));
  }
  asyncActor_wait(ac, fn2);

  model_id = nick_id(model);
  net_update_historic(ac, model_id);

  EACH(nicks, Nick, nk)
    void fn3 () { log_info(str_f("Reading %s", nick_name(nk))); }
    asyncActor_wait(ac, fn3);

    int nk_id = nick_id(nk);
    if (!io_active()) break;
    if (nk_id == model_id) continue;
    net_update_historic(ac, nk_id);
  _EACH
}
