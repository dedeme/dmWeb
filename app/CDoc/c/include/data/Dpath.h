// Copyright 18-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Documentation path data.

#ifndef DATA_DPATH_H
  #define DATA_DPATH_H

#include "dmc/std.h"

/*--*/

/// Documentation path data.
///   Arguments:
///     id: char*
///     path: char*
///     show: bool
///     valid: bool
typedef struct Dpath_Dpath Dpath;

///
Dpath *dpath_new (
  char *id,
  char *path,
  int show,
  int valid
);

///
char *dpath_id (Dpath *this);

///
char *dpath_path (Dpath *this);

///
int dpath_show (Dpath *this);

///
void dpath_set_show (Dpath *this, int value);

///
int dpath_valid (Dpath *this);

///
void dpath_set_valid (Dpath *this, int value);

///
Js *dpath_to_js (Dpath *this);

///
Dpath *dpath_from_js (Js *js);

/*--*/

#endif
