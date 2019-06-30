// Copyright 19-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/fleas.h"
#include "server/fleas/fleas__main.h"
#include "server/fleas/fleas__bests.h"
#include "server/fleas/fleas__charts.h"
#include "server/fleas/fleas__model.h"
#include "server/fleas/fleas__champions.h"
#include "server/fleas/wgs/fleas__wgs__wcharts.h"
#include "server/fleas/wgs/fleas__wgs__dtable.h"

char *fleas_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(source, mrq, "source")

  if (str_eq(source, "FleasMain")) return fleas__main_process(ac, mrq);
  if (str_eq(source, "bests")) return fleas__bests_process(ac, mrq);
  if (str_eq(source, "charts")) return fleas__charts_process(ac, mrq);
  if (str_eq(source, "model")) return fleas__model_process(ac, mrq);
  if (str_eq(source, "champions")) return fleas__champions_process(ac, mrq);
  if (str_eq(source, "Wcharts")) return fleas__wgs__wcharts_process(ac, mrq);
  if (str_eq(source, "Dtable")) return fleas__wgs__dtable_process(ac, mrq);

  EXC_ILLEGAL_ARGUMENT(
    "source",
    "FleasMain | bests | charts | model | champions",
    source
  )
  return NULL; // Unreachable
}
