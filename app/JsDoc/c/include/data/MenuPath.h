// Copyright 06-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_MENUPATH_H
  #define DATA_MENUPATH_H

#include "dmc/std.h"

/*.-.*/

#include "dmc/Json.h"

///
typedef struct menuPath_MenuPath MenuPath;

///
MenuPath *menuPath_new(
  char *id,
  char *path,
  bool show,
  bool ok
);

///
char *menuPath_id(MenuPath *this);

///
void menuPath_set_id(MenuPath *this, char *value);

///
char *menuPath_path(MenuPath *this);

///
void menuPath_set_path(MenuPath *this, char *value);

///
bool menuPath_show(MenuPath *this);

///
void menuPath_set_show(MenuPath *this, bool value);

///
bool menuPath_ok(MenuPath *this);

///
void menuPath_set_ok(MenuPath *this, bool value);

///
Json *menuPath_to_json(MenuPath *this);

///
MenuPath *menuPath_from_json(Json *s);

/*.-.*/

#define TY MenuPath                    // Element type
#define FN menuPath                    // Function prefix
#include "dmc/tpl/tarr.h"
#undef TY
#undef FN


///
Json *amenuPath_to_json(AmenuPath *this);

///
AmenuPath *amenuPath_from_json(Json *js);

#endif
