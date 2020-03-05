// Copyright 06-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "module.h"
#include "data/Mdoc.h"

static char *last_doc = "";
static char *overview = "";
static bool is_first_doc = true;

static bool is_private(char *doc) {
  return str_index(doc, "@private") != -1;
}

static void comment(char *line, LckFile *lck) {
  bool is_doc = str_starts(line, "/**");
  if (is_doc) {
    last_doc = "";
  }
  line = str_trim(str_sub_end(line, is_doc ? 3 : 2));

  int ix = str_index(line, "*/");
  if (ix != -1) {
    if (is_doc) {
      last_doc = str_trim(str_sub(line, 0, ix));
      if (!*overview) {
        overview = last_doc;
      }
    }
    return;
  }

  if (*line && is_doc) {
    last_doc = line;
  }

  line = file_read_line(lck);
  while (*line) {
    int ix = str_index(line, "*/");
    if (ix != -1) {
      if (is_doc) {
        if (!*overview) {
          overview = last_doc;
        }
      }
      return;
    }

    if (is_doc) {
      last_doc = str_cat(last_doc, line, NULL);
    }
    line = file_read_line(lck);
  }
}

static char *end_definition(char *line, int ix, LckFile *lck) {
  line = str_sub_end(line, ix);
  ix = str_cindex(line, ')');
  if (ix != -1) {
    return str_sub(line, 0, ix + 1);
  }
  Buf *bf = buf_new();
  buf_add(bf, line);

  line = file_read_line(lck);
  while (*line) {
    ix = str_cindex(line, ')');
    if (ix != -1) {
      buf_add(bf, str_sub(line, 0, ix + 1));
      break;
    }
    buf_add(bf, line);
    line = file_read_line(lck);
  }

  return buf_to_str(bf);
}

static void function(ApackDoc *functions, char *line, int ix, LckFile *lck) {
  char *left_line = str_sub(line, 0, ix);
  char *id = str_trim(left_line);
  if (str_index(line, " function ") == -1) {
    return;
  }
  int i = str_last_cindex(id, ' ');
  id = str_trim(str_sub_end(id, i + 1));

  char *edef = end_definition(line, ix, lck);

  if (is_first_doc) {
    if (!*overview) {
      overview = last_doc;
    }
    is_first_doc = false;
  }

  apackDoc_add(functions, packDoc_new(
    id, str_cat(left_line, edef, NULL), last_doc
  ));
}

static OpackDoc *construct(char *line, LckFile *lck) {
  int ix = str_cindex(line, '(');
  if (ix == -1) {
    return opackDoc_null();
  }
  char *edef = end_definition(line, ix, lck);
  edef = str_replace(edef, "\n  ", "\n");
  char *definition = str_cat("constructor ", edef, NULL);
  return opackDoc_new(packDoc_new("", definition, last_doc));
}

static OpackDoc *method(bool is_static, char *line, LckFile *lck) {
  int ix = str_cindex(line, '(');
  if (ix == -1) {
    return opackDoc_null();
  }
  char *id = str_trim(str_sub(line, 0, ix));
  char *edef = end_definition(line, ix, lck);
  edef = str_replace(edef, "\n  ", "\n");
  char *definition =
    str_cat(
      is_static ? "static " : "",
      id, " ", edef, NULL
    );
  return opackDoc_new(packDoc_new(id, definition, last_doc));
}

static void getter(AclassAtt *atts, PackDoc *pack) {
  char *pid = packDoc_id(pack);
  bool find (ClassAtt *e) {
    return str_eq(pid, classAtt_id(e));
  }

  OclassAtt *oatt = iclassAtt_find(aclassAtt_to_it(atts), find);
  if (oclassAtt_is_null(oatt)) {
    aclassAtt_add(atts, classAtt_new(
      packDoc_id(pack),
      "g",
      pack,
      packDoc_new("", "", "")
    ));
  } else {
    ClassAtt *att = oclassAtt_value(oatt);
    char *tp = classAtt_type(att);
    if (str_eq(tp, "s")) {
      tp = "gs";
    }
    classAtt_set_type(att, tp);
    classAtt_set_get_doc(att, pack);
  }
}

static void setter(AclassAtt *atts, PackDoc *pack) {
  char *pid = packDoc_id(pack);
  bool find (ClassAtt *e) {
    return str_eq(pid, classAtt_id(e));
  }

  OclassAtt *oatt = iclassAtt_find(aclassAtt_to_it(atts), find);
  if (oclassAtt_is_null(oatt)) {
    aclassAtt_add(atts, classAtt_new(
      packDoc_id(pack),
      "s",
      packDoc_new("", "", ""),
      pack
    ));
  } else {
    ClassAtt *att = oclassAtt_value(oatt);
    char *tp = classAtt_type(att);
    if (str_eq(tp, "g")) {
      tp = "gs";
    }
    classAtt_set_type(att, tp);
    classAtt_set_set_doc(att, pack);
  }
}

static void class(Aclass *classes, char *line, int ix, LckFile *lck) {
  char *definition = str_trim(str_sub(line, 0, ix));
  if (str_index(definition, " class ") == -1) {
    return;
  }

  int i = str_last_cindex(definition, ' ');
  char *id = str_ltrim(str_sub_end(definition, i));


  char *doc = last_doc;
  if (is_first_doc) {
    if (!*overview) {
      overview = last_doc;
    }
    is_first_doc = false;
  }

  PackDoc *constructor = packDoc_new("", "", "");
  ApackDoc *methods = apackDoc_new();
  AclassAtt *atts = aclassAtt_new();

  line = file_read_line(lck);
  while (*line) {
    if (*line > ' ') {
      break;
    }

    char *ltrim = str_trim(line);
    if (str_starts(ltrim, "/*")) {
      comment(ltrim, lck);
    } else if (line[0] == ' ' && line[1] == ' ' && line[2] > ' ') {
      line = str_sub_end(line, 2);
      bool is_static = false;
      if (str_starts(line, "static ")) {
        is_static = true;
        line = str_ltrim(str_sub_end(line, 7));
      }
      if (str_starts(line, "get ")) {
        line = str_ltrim(str_sub_end(line, 4));
        OpackDoc *opack = method(is_static, line, lck);
        if (!opackDoc_is_null(opack)) {
          getter(atts, opackDoc_value(opack));
        }
      } else if (str_starts(line, "set ")) {
        line = str_ltrim(str_sub_end(line, 4));
        OpackDoc *opack = method(is_static, line, lck);
        if (!opackDoc_is_null(opack)) {
          setter(atts, opackDoc_value(opack));
        }
      } else if (
        str_starts(line, "constructor ") ||
        str_starts(line, "constructor(")
      ) {
        OpackDoc *opack = construct(line, lck);
        if (!opackDoc_is_null(opack)) {
          constructor = opackDoc_value(opack);
        }
      } else {
        OpackDoc *opack = method(is_static, line, lck);
        if (!opackDoc_is_null(opack)) {
          apackDoc_add(methods, opackDoc_value(opack));
        }
      }
    }

    line = file_read_line(lck);
  }

  constructor = is_private(packDoc_doc(constructor))
    ? packDoc_new("", "", "")
    : constructor
  ;

  ApackDoc *fmethods = apackDoc_new();
  EACH(methods, PackDoc, m) {
    if (!is_private(packDoc_doc(m))) {
      apackDoc_add(fmethods, m);
    }
  }_EACH

  AclassAtt *fatts = aclassAtt_new();
  EACH(atts, ClassAtt, a) {
    bool is_get = str_cindex(classAtt_type(a), 'g') != -1;
    bool is_get_private = is_private(packDoc_doc(classAtt_get_doc(a)));
    bool is_set = str_cindex(classAtt_type(a), 's') != -1;
    bool is_set_private = is_private(packDoc_doc(classAtt_set_doc(a)));
    if (is_get) {
      if (is_set) {
        if (!is_get_private && !is_set_private) {
          aclassAtt_add(fatts, a);
        } else if (!is_get_private) {
          aclassAtt_add(fatts, classAtt_new(
            classAtt_id(a),
            "g",
            classAtt_get_doc(a),
            packDoc_new("", "", "")
          ));
        } else if (!is_set_private) {
          aclassAtt_add(fatts, classAtt_new(
            classAtt_id(a),
            "s",
            packDoc_new("", "", ""),
            classAtt_get_doc(a)
          ));
        }
      } else {
        if (!is_get_private) {
          aclassAtt_add(fatts, a);
        }
      }
    } else {
      if (!is_set_private) {
        aclassAtt_add(fatts, a);
      }
    }
  }_EACH

  aclass_add(classes, class_new(
    id, definition, doc, constructor, fmethods, fatts
  ));
}

static void export(
  ApackDoc *functions, Aclass *classes, char *line, LckFile *lck
) {
  int ix = str_cindex(line, '(');
  if (ix != -1) {
    function(functions, line, ix, lck);
    return;
  }

  ix = str_cindex(line, '{');
  if (ix != -1) {
    class(classes, line, ix, lck);
  }
}

static Mdoc *rfile(LckFile *lck) {
  ApackDoc *functions = apackDoc_new();
  Aclass *classes = aclass_new();

  char *line = file_read_line(lck);
  while (*line) {
    char *ltrim = str_trim(line);
    if (str_starts(ltrim, "/*")) {
      comment(ltrim, lck);
    } else if (str_starts(line, "export ")) {
      export(functions, classes, line, lck);
    }
    line = file_read_line(lck);
  }

  if (is_private(overview)) {
    overview = "";
  }

  ApackDoc *ffunctions = apackDoc_new();
  EACH(functions, PackDoc, f) {
    if (!is_private(packDoc_doc(f))) {
      apackDoc_add(ffunctions, f);
    }
  }_EACH

  Aclass *fclasses = aclass_new();
  EACH(classes, Class, c) {
    if (!is_private(class_doc(c))) {
      aclass_add(fclasses, c);
    }
  }_EACH

  return mdoc_new(overview, ffunctions, fclasses);
}

CgiRp *module_process(Mjson *rq) {
  char *path = jmap_gstring(rq, "path");
  if (!file_exists(path)) {
    return cgi_ok(mjson_new());
  }
  LckFile *lck = file_ropen(path);
  Mdoc *doc = rfile(lck);
  file_close(lck);
  Mjson *m = mjson_new();
  mjson_put(m, "mdoc", mdoc_to_json(doc));
  return cgi_ok(m);
}
