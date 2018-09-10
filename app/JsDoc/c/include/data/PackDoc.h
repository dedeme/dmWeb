// Copyright 06-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_PACKDOC_H
  #define DATA_PACKDOC_H

#include "dmc/std.h"

/*.-.*/

#include "dmc/Json.h"

///
typedef struct packDoc_PackDoc PackDoc;

///
PackDoc *packDoc_new(char *id, char *definition, char *doc);

///
char *packDoc_id(PackDoc *this);

///
char *packDoc_definition(PackDoc *this);

///
char *packDoc_doc(PackDoc *this);

///
Json *packDoc_to_json(PackDoc *this);

///
PackDoc *packDoc_from_json(Json *s);

/*.-.*/

#define TY PackDoc
#define FN packDoc
#include "dmc/tpl/tarr.h"
#include "dmc/tpl/topt.h"
#undef TY
#undef FN


#endif
