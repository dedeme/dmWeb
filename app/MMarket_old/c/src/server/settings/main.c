// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/settings/main.h"
#include "server/settings/settings.h"
#include "server/settings/changepass.h"
#include "server/settings/calendar.h"

char *mainSettings_process (AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(source, mrq);
  if (str_eq(source, "Settings")) return settings_process(ac, mrq);
  if (str_eq(source, "ChangePass")) return changepass_process(ac, mrq);
  if (str_eq(source, "Calendar")) return calendar_process(ac, mrq);

  EXC_ILLEGAL_ARGUMENT("source", "Settings", source)
  return NULL; // Unreachable
}
