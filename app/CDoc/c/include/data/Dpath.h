// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Path data.

#ifndef DATA_DPATH_H
  #define DATA_DPATH_H

/// Path data.
struct Dpath_Dpath {
  char *id; // Path identifier.
  char *path; // Source path.
  int is_shown; // 'true' if the path should be shown.
  int is_valid; // 'true' if the path is a valid one.
};

/// Path data.
typedef struct Dpath_Dpath Dpath;

///
Dpath *dpath_new (
  char *id,
  char *path,
  int is_shown,
  int is_valid
);

/// Modifies in place the field 'is_shown'
void dpath_set_shown (Dpath *this, int value);

/// Modifies in place the field 'is_valid'
void dpath_set_valid (Dpath *this, int value);

///
char *dpath_to_js (Dpath *this);

///
Dpath *dpath_from_js (char *js);


#endif
