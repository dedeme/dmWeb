// Copyright 04-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys.h"
#include "server/sys/sys__main.h"
#include "server/sys/sys__home.h"
#include "server/sys/sys__settings.h"
#include "server/sys/sys__chpass.h"
#include "server/sys/sys__backups.h"
#include "server/sys/sys__schedule.h"
#include "server/sys/sys__nicks.h"
#include "server/sys/nicks/sys__nicks__wnick.h"
#include "server/sys/nicks/sys__nicks__editor.h"
#include "server/sys/sys__servers.h"
#include "server/sys/servers/sys__servers__names.h"
#include "server/sys/servers/sys__servers__codes.h"
#include "server/sys/servers/sys__servers__download.h"

char *sys_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(source, mrq, "source")

  if (str_eq(source, "SysMain")) return sys__main_process(ac, mrq);
  if (str_eq(source, "Home")) return sys__home_process(ac, mrq);
  if (str_eq(source, "Settings")) return sys__settings_process(ac, mrq);
  if (str_eq(source, "Chpass")) return sys__chpass_process(ac, mrq);
  if (str_eq(source, "Backups")) return sys__backups_process(ac, mrq);
  if (str_eq(source, "Backups")) return sys__backups_process(ac, mrq);
  if (str_eq(source, "Schedule")) return sys__schedule_process(ac, mrq);
  if (str_eq(source, "Nicks")) return sys__nicks_process(ac, mrq);
  if (str_eq(source, "nicks/Wnick")) return sys__nicks__wnick_process(ac, mrq);
  if (str_eq(source, "nicks/Editor"))
    return sys__nicks__editor_process(ac, mrq);
  if (str_eq(source, "Servers")) return sys__servers_process(ac, mrq);
  if (str_eq(source, "servers/Names"))
    return sys__servers__names_process(ac, mrq);
  if (str_eq(source, "servers/Codes"))
    return sys__servers__codes_process(ac, mrq);
  if (str_eq(source, "servers/Download"))
    return sys__servers__download_process(ac, mrq);

  EXC_ILLEGAL_ARGUMENT(
    "source",
    "SysMain | Home | Settings | Chpass | Backups | Schedule "
    "Nicks | nicks/Wnick | nicks/Editor "
    "Servers | servers/Names | servers/Codes | servers/Download",
    source
  )
  return NULL; // Unreachable
}
