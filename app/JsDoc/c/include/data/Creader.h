// Copyright 10-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_CREADER_H
  #define DATA_CREADER_H

#include "dmc/std.h"

/*.-.*/

///
typedef struct creader_Creader Creader;

///
bool creader_end(Creader *this);

/*.-.*/

///
Creader *creader_new(LckFile *lck, char *link);

///
char creader_next_char(Creader *this);

///
char *creader_next_line(Creader *this);

///
char creader_peek_char(Creader *this);

///
char *creader_start_long_comment(Creader *this);

///
char *creader_start_long_doc(Creader *this);

///
char *creader_start_short_comment(Creader *this);

///
char *creader_start_short_doc(Creader *this);

///
char *creader_end_comment(Creader *this);

///
char *creader_id(Creader *this);

///
char *creader_number(Creader *this);

#endif
