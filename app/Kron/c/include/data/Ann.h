// Copyright 14-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Annotation data.

#ifndef DATA_ANN_H
  #define DATA_ANN_H

#include <time.h>
#include "dmc/AInt.h"

/// Time types
enum AnnTimeType { ann_PERIODIC, ann_FIX, ann_INIT, ann_NOTE };

/// Text types.
enum AnnTextType { ann_COMMAND, ann_MESSAGE };

/// Annotation data.
struct ann_Ann {
  int id;
  int time_type;
  // JSON value which returns:
  //   ann_PERIODIC: [(time_t)js_rd(e1) -> a date, AInt of js_ri(e2) -> (0-6)
  //   ann_FIX: (time_t)js_rd() -> a date.
  //   ann_INIT: FAIL.
  //   ann_NOTES: Notes group.
  char *data; // JSON value.
  int text_type;
  char *text;
};

/// Annotation data.
typedef struct ann_Ann Ann;

///
Ann *ann_new (int id, int time_type, char *time, int text_type, char *text);

/// Returns js_ri() -> (0-6) if time_type is ann_PERIODIC.
AInt *ann_days (Ann *a);

/// Returns '(time_t)js_rd()' (a date) if time_type is ann_FIX.
time_t ann_date (Ann *a);

/// Returns Notes group if time_type is ann_NOTE.
char *ann_group (Ann *a);

/// Set in place the id field.
void ann_set_id (Ann *this, int id);

/// Execute an annotation and returns a text for log.
char *ann_run (Ann *this);

///
char *ann_to_js (Ann *this);

///
Ann *ann_from_js (char *js);

#endif
