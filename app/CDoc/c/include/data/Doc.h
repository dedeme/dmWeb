// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Module documentation data.

#ifndef DATA_DOC_H
  #define DATA_DOC_H

#include "data/DocEntry/ADocEntry.h"

/// Module documentation data.
struct doc_Doc {
  char *doc; // Module documentation.
  ADocEntry *defines; // 'defines' documentation.
  ADocEntry *enums; // 'enums' documentation.
  ADocEntry *structs; // 'structs' documentation.
  ADocEntry *typedefs; // 'typedefs' documentation.
  ADocEntry *unions; // 'unions' documentation.
  ADocEntry *functions; // functions documentation.
  ADocEntry *vars; // variables documentation.
};

/// Module documentation data.
typedef struct doc_Doc Doc;

/// Creates a new Doc, with every entry empty.
///   doc: Module documentation.
Doc *doc_new (char *doc);

///
char *doc_to_js (Doc *this);

#endif
