// Copyright 20-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Doc.h"

/* .
# Documentation entry.
DocEntry: serial
  # Entry name.
  name: char *
  # Entry documentation.
  doc: char *
  # Code documented.
  code: char *
  # C code link.
  link: char *
===
# Module documentation.
Doc: serial
  # Module documentation.
  doc: char *
  ---
  # Arr[DocEntry]
  defines: Arr - DocEntry : arr_new()
  # Arr[DocEntry]
  enums: Arr - DocEntry : arr_new()
  # Arr[DocEntry]
  structs: Arr - DocEntry : arr_new()
  # Arr[DocEntry]
  typedefs: Arr - DocEntry : arr_new()
  # Arr[DocEntry]
  unions: Arr - DocEntry : arr_new()
  # Arr[DocEntry]
  functions: Arr - DocEntry : arr_new()
  # Arr[DocEntry]
  vars: Arr - DocEntry : arr_new()
*/

/*--*/

struct Doc_DocEntry {
  char *name;
  char *doc;
  char *code;
  char *link;
};

DocEntry *docEntry_new (
  char *name,
  char *doc,
  char *code,
  char *link
) {
  DocEntry *this = MALLOC(DocEntry);
  this->name = name;
  this->doc = doc;
  this->code = code;
  this->link = link;
  return this;
}

char *docEntry_name (DocEntry *this) {
  return this->name;
}

char *docEntry_doc (DocEntry *this) {
  return this->doc;
}

char *docEntry_code (DocEntry *this) {
  return this->code;
}

char *docEntry_link (DocEntry *this) {
  return this->link;
}

Js *docEntry_to_js (DocEntry *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->name));
  arr_push(js, js_ws(this->doc));
  arr_push(js, js_ws(this->code));
  arr_push(js, js_ws(this->link));
  return js_wa(js);
}

DocEntry *docEntry_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  DocEntry *this = MALLOC(DocEntry);
  this->name = js_rs(*p++);
  this->doc = js_rs(*p++);
  this->code = js_rs(*p++);
  this->link = js_rs(*p++);
  return this;
}

struct Doc_Doc {
  char *doc;
  Arr *defines;
  Arr *enums;
  Arr *structs;
  Arr *typedefs;
  Arr *unions;
  Arr *functions;
  Arr *vars;
};

Doc *doc_new (char *doc) {
  Doc *this = MALLOC(Doc);
  this->doc = doc;
  this->defines = arr_new();
  this->enums = arr_new();
  this->structs = arr_new();
  this->typedefs = arr_new();
  this->unions = arr_new();
  this->functions = arr_new();
  this->vars = arr_new();
  return this;
}

char *doc_doc (Doc *this) {
  return this->doc;
}

Arr *doc_defines (Doc *this) {
  return this->defines;
}

Arr *doc_enums (Doc *this) {
  return this->enums;
}

Arr *doc_structs (Doc *this) {
  return this->structs;
}

Arr *doc_typedefs (Doc *this) {
  return this->typedefs;
}

Arr *doc_unions (Doc *this) {
  return this->unions;
}

Arr *doc_functions (Doc *this) {
  return this->functions;
}

Arr *doc_vars (Doc *this) {
  return this->vars;
}

Js *doc_to_js (Doc *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->doc));
  arr_push(js, arr_to_js(this->defines, (FTO)docEntry_to_js));
  arr_push(js, arr_to_js(this->enums, (FTO)docEntry_to_js));
  arr_push(js, arr_to_js(this->structs, (FTO)docEntry_to_js));
  arr_push(js, arr_to_js(this->typedefs, (FTO)docEntry_to_js));
  arr_push(js, arr_to_js(this->unions, (FTO)docEntry_to_js));
  arr_push(js, arr_to_js(this->functions, (FTO)docEntry_to_js));
  arr_push(js, arr_to_js(this->vars, (FTO)docEntry_to_js));
  return js_wa(js);
}

Doc *doc_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  Doc *this = MALLOC(Doc);
  this->doc = js_rs(*p++);
  this->defines = arr_from_js(*p++, (FFROM)docEntry_from_js);
  this->enums = arr_from_js(*p++, (FFROM)docEntry_from_js);
  this->structs = arr_from_js(*p++, (FFROM)docEntry_from_js);
  this->typedefs = arr_from_js(*p++, (FFROM)docEntry_from_js);
  this->unions = arr_from_js(*p++, (FFROM)docEntry_from_js);
  this->functions = arr_from_js(*p++, (FFROM)docEntry_from_js);
  this->vars = arr_from_js(*p++, (FFROM)docEntry_from_js);
  return this;
}

/*--*/

