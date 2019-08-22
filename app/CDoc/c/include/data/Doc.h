// Copyright 20-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_DOC_H
  #define DATA_DOC_H

#include "dmc/std.h"

/*--*/

/// Documentation entry.
///   Arguments:
///     name: char*
///     doc: char*
///     code: char*
///     link: char*
typedef struct Doc_DocEntry DocEntry;

///
DocEntry *docEntry_new (
  char *name,
  char *doc,
  char *code,
  char *link
);

/// Entry name.
char *docEntry_name (DocEntry *this);

/// Entry documentation.
char *docEntry_doc (DocEntry *this);

/// Code documented.
char *docEntry_code (DocEntry *this);

/// C code link.
char *docEntry_link (DocEntry *this);

///
Js *docEntry_to_js (DocEntry *this);

///
DocEntry *docEntry_from_js (Js *js);

/// Module documentation.
///   Arguments:
///     doc: char*
///   Variables:
///     defines: Arr-DocEntry
///     enums: Arr-DocEntry
///     structs: Arr-DocEntry
///     typedefs: Arr-DocEntry
///     unions: Arr-DocEntry
///     functions: Arr-DocEntry
///     vars: Arr-DocEntry
typedef struct Doc_Doc Doc;

///
Doc *doc_new (char *doc);

/// Module documentation.
char *doc_doc (Doc *this);

/// Arr[DocEntry]
Arr *doc_defines (Doc *this);

/// Arr[DocEntry]
Arr *doc_enums (Doc *this);

/// Arr[DocEntry]
Arr *doc_structs (Doc *this);

/// Arr[DocEntry]
Arr *doc_typedefs (Doc *this);

/// Arr[DocEntry]
Arr *doc_unions (Doc *this);

/// Arr[DocEntry]
Arr *doc_functions (Doc *this);

/// Arr[DocEntry]
Arr *doc_vars (Doc *this);

///
Js *doc_to_js (Doc *this);

///
Doc *doc_from_js (Js *js);

/*--*/

#endif
