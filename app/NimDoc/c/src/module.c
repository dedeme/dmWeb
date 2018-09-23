// Copyright 06-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "module.h"
#include "data/Mdoc.h"
#include "dmc/ct/Ajson.h"
#include "dmc/ct/Ojson.h"

static char *overview(char *code) {
  Buf *bf = buf_new();
  int first = true;
  EACH(str_csplit(code, '\n'), char, l) {
    l = str_trim(l);
    if (first) {
      if (str_starts(l, "##")) {
        int end = str_index(l, ".<");
        if (end != -1) {
          l = str_sub(l, 0, end + 1);
        }
        buf_add(bf, str_rtrim(str_sub_end(l, 2)));
        buf_cadd(bf, '\n');
        first = false;
      }
    } else {
      if (str_starts(l, "##")) {
        buf_add(bf, str_rtrim(str_sub_end(l, 2)));
        buf_cadd(bf, '\n');
      } else {
        break;
      }
    }
  }_EACH
  return buf_to_str(bf);
}

static Ajson *filter(Ajson *entries, char *class) {
  Ajson *a = ajson_new();
  EACH(entries, Json, js) {
    Mjson *entry = json_robject(js);
    Ojson *otype = mjson_get(entry, "type");
    if (!ojson_is_null(otype)){
      char *type = json_rstring(ojson_value(otype));
      if (str_eq(type, class)) {
        ajson_add(a, js);
      }
    }
  }_EACH
  return a;
}

CgiRp *module_process(Mjson *rq) {
  char *path = jmap_gstring(rq, "path");
  if (!file_exists(path)) {
    return cgi_ok(mjson_new());
  }
  char *tmp = path_cat(cgi_home(), "tmp", NULL);
  if (file_exists(tmp)) {
    file_del(tmp);
  }
  file_mkdir(tmp);
  file_cd(tmp);
  file_copy(path, "tmp.nim");
  sys_cmd(str_printf("nim jsondoc2 tmp.nim"));

  Mjson *m = mjson_new();
  jmap_pbool(m, "ok", false);
  if (file_exists("tmp.json")) {
    jmap_pbool(m, "ok", true);
    jmap_pstring(m, "overview", overview(file_read("tmp.nim")));
    Mjson *doc = json_robject((Json *)file_read("tmp.json"));
    Ajson *entries = json_rarray(mjson_oget(doc, "entries", (Json *)"[]"));

    mjson_put(m, "types", json_warray(filter(entries, "skType")));
    mjson_put(m, "enums", json_warray(filter(entries, "skEnumField")));
    mjson_put(m, "params", json_warray(filter(entries, "skParam")));
    mjson_put(m, "methods", json_warray(filter(entries, "skMethod")));
    mjson_put(m, "macros", json_warray(filter(entries, "skMacro")));
    mjson_put(m, "templates", json_warray(filter(entries, "skTemplate")));
    mjson_put(m, "iterators", json_warray(filter(entries, "skIterator")));
    mjson_put(m, "procs", json_warray(filter(entries, "skProc")));
    mjson_put(m, "all", json_warray(entries));
  }

  return cgi_ok(m);
}
