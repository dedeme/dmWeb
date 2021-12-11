// Copyright 09-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pgs/pathsPg.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "db/conftb.h"
#include "db/dpaths.h"

char *pathsPg_process(Mchar *mrq) {
  char *rq = cgi_rq_string(mrq, "rq");
  if (str_eq(rq, "changeLang")) {
    Conf *cf = conftb_read();
    conftb_write(conf_new(
      cf->path,
      str_eq(cf->lang, "es") ? "en" : "es",
      cf->show_all
    ));
    return cgi_rp_empty();
  } else if (str_eq(rq, "changeShowAll")) {
    Conf *cf = conftb_read();
    conftb_write(conf_new(
      cf->path,
      cf->lang,
      cf->show_all ? 0 : 1
    ));
    return cgi_rp_empty();
  } else if (str_eq(rq, "new")) {
    char *id = cgi_rq_string(mrq, "id");
    char *path = cgi_rq_string(mrq, "path");
    ADpath *ps = dpaths_read();
    aDpath_push(ps, dpath_new(id, path, 1, 0));
    dpaths_write(ps);
    return cgi_rp_empty();
  } else if (str_eq(rq, "changeShown")) {
    char *id = cgi_rq_string(mrq, "id");
    ADpath *ps = dpaths_read();
    Dpath **p = ps->es;
    while (p < ps->end) {
      Dpath *pth = *p++;
      if (str_eq(pth->id, id)){
        dpath_set_shown(pth, !pth->is_shown);
        break;
      }
    }
    dpaths_write(ps);
    return cgi_rp_empty();
  } else if (str_eq(rq, "delete")) {
    char *id = cgi_rq_string(mrq, "id");
    ADpath *ps = dpaths_read();
    /**/int filter (Dpath *p) { return !str_eq(p->id, id); }
    aDpath_filter_in(ps, filter);
    dpaths_write(ps);
    return cgi_rp_empty();
  } else if (str_eq(rq, "modify")) {
    char *id = cgi_rq_string(mrq, "id");
    char *newId = cgi_rq_string(mrq, "newId");
    char *path = cgi_rq_string(mrq, "path");
    ADpath *new_ps = aDpath_new();
    ADpath *ps = dpaths_read();
    Dpath **p = ps->es;
    while (p < ps->end) {
      Dpath *pth = *p++;
      if (str_eq(pth->id, id)){
        aDpath_push(new_ps, dpath_new(newId, path, pth->is_shown, 0));
      } else {
        aDpath_push(new_ps, pth);
      }
    }
    dpaths_write(new_ps);
    return cgi_rp_empty();
  } else {
    return FAIL(str_f("Unexpected value for 'rq': %s", rq));
  }
}
