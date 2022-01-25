// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Documentation entry.

#ifndef DATA_DOCENTRY_H
  #define DATA_DOCENTRY_H

/// Documentation entry.
struct Doc_DocEntry {
  char *name; // Entry name.
  char *doc; // Entry documentation.
  char *code; // Entry code.
  char *link; // Entry code page link.
};

/// Documentation entry.
typedef struct Doc_DocEntry DocEntry;

///
DocEntry *docEntry_new (
  char *name,
  char *doc,
  char *code,
  char *link
);

///
char *docEntry_to_js (DocEntry *this);

///
DocEntry *docEntry_from_js (char *js);

#endif
