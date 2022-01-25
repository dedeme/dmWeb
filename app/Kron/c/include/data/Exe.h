// Copyright 22-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Executed periodic annotation data.

#ifndef DATA_EXE_H
  #define DATA_EXE_H

#include <time.h>

/// Executed periodic annotation data.
struct exe_Exe {
  int id; // Executed annotation id.
  char *day; // day in format yyyymmdd.
};

/// Executed periodic annotation data.
typedef struct exe_Exe Exe;

///
Exe *exe_new (int id, char *day);

///
char *exe_to_js (Exe *this);

///
Exe *exe_from_js (char *js);

#endif
