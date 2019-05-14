// Copyright 04-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys.h"
#include "server/sys/sys__main.h"
#include "server/sys/sys__home.h"
#include "server/sys/sys__settings.h"
#include "server/sys/sys__chpass.h"
#include "server/sys/sys__backups.h"
#include "server/sys/sys__nicks.h"
#include "server/sys/sys__servers.h"

char *sys_process(Map *mrq) {
  CGI_GET_STR(source, mrq, "source")

  if (str_eq(source, "SysMain")) return sys__main_process(mrq);
  if (str_eq(source, "Home")) return sys__home_process(mrq);
  if (str_eq(source, "Settings")) return sys__settings_process(mrq);
  if (str_eq(source, "Chpass")) return sys__chpass_process(mrq);
  if (str_eq(source, "Backups")) return sys__backups_process(mrq);
  if (str_eq(source, "Backups")) return sys__backups_process(mrq);
  if (str_eq(source, "Nicks")) return sys__nicks_process(mrq);
  if (str_eq(source, "Servers")) return sys__servers_process(mrq);

  EXC_ILLEGAL_ARGUMENT(
    "source",
    "SysMain | Home | Settings | Chpass | Backups | Nicks | Servers",
    source
  )
  return NULL; // Unreachable
}
