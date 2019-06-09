// Copyright 29-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "scheduler/historic.h"
#include "io/nicks.h"
#include "io/servers.h"
#include "io.h"

static void read (AsyncActor *ac, Nick *nk, Nick *model) {
  // Arr[Server]
//  Arr *servers = servers_list();


  printf("historic static read (%s) NOT IMPLEMENTED\n", nick_name(nk));
}

void historic_read (AsyncActor *ac) {
  // Arr[Nick]
  Arr *nicks = NULL;
  int model_id = -1;
  void fn (void *null) {
    nicks = nicks_list();
    model_id = nicks_model();
  }
  asyncActor_wait(ac, fn, NULL);

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

  read(ac, model, NULL);

  EACH(nicks, Nick, nk)
    if (!io_active()) break;
    if (nick_id(nk) == nick_id(model)) continue;
    read(ac, nk, model);
  _EACH
}
