// Copyright 07-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Defines the struct ClassAtt.<p>
/// It has the fallowing fields:
///   id: Identifier equals for get and set operations.
///   type: With the following values:
///         "g"  -> The attribute has the operation 'get'
///         "s"  -> The attribute has the operation 'set'
///         "gs" -> The attribute has both operations
///   get_doc: Its field 'id' is ""
///   set_doc: Its field 'id' is ""
/// If any operation 'get/set' is missing, its corresponding 'get_doc' and
/// 'set_doc' will have all its values in blank ("").

#ifndef DATA_CLASSATT_H
  #define DATA_CLASSATT_H

#include "dmc/std.h"
#include "PackDoc.h"

/*.-.*/

#include "dmc/Json.h"

///
typedef struct classAtt_ClassAtt ClassAtt;

///
ClassAtt *classAtt_new(
  char *id,
  char *type,
  PackDoc *get_doc,
  PackDoc *set_doc
);

///
char *classAtt_id(ClassAtt *this);

///
char *classAtt_type(ClassAtt *this);

///
void classAtt_set_type(ClassAtt *this, char *value);

///
PackDoc *classAtt_get_doc(ClassAtt *this);

///
void classAtt_set_get_doc(ClassAtt *this, PackDoc *value);

///
PackDoc *classAtt_set_doc(ClassAtt *this);

///
void classAtt_set_set_doc(ClassAtt *this, PackDoc *value);

///
Json *classAtt_to_json(ClassAtt *this);

///
ClassAtt *classAtt_from_json(Json *s);

/*.-.*/

#define TY ClassAtt
#define FN classAtt
#include "dmc/tpl/tarr.h"
#include "dmc/tpl/tit.h"
#include "dmc/tpl/topt.h"
#undef TY
#undef FN

#endif
