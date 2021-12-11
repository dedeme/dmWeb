// Copyright 11-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Result of reading data for documentation.

#ifndef DATA_DOCRS_H
  #define DATA_DOCRS_H

/// Result of reading data for documentation.
struct docRs_DocRs {
  char *doc; // Last documentation read.
  char *line; // File line text.
};

/// Result of reading data for documentation.
typedef struct docRs_DocRs DocRs;

/// Constructor.
///   doc : Last documentation read.
///   line: File line text.
DocRs *docRs_new (char *doc, char *line);

#endif
