// Copyright 10-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef IO_QUOTES_H
  #define IO_QUOTES_H

#include "dmc/async.h"
#include "data/Quote.h"
#include "data/EMsg.h"

///
void quotes_init (void);

/// Returns quotes of 'nick_name'.
///   nick_name: Name of nick (e.g. TEF)
///   return: Arr[Quote]. If an error results, the array is empty.
///           Order of quotes is from after to before.
Arr *quotes_read (char *nick_name);

/// Writes 'qs' of 'nick_name'
///   nick_name: Name of nick (e.g. TEF)
///   qs: It is Arr[Quote]. Its order is from after to before.
void quotes_write (char *nick_name, Arr *qs);

/// Creates a new empty file if it do not already exist.
///   nick_name: Name of nick (e.g. TEF)
void quotes_add (char *nick_name);

/// Deletes a file if it exist.
///   nick_name: Name of nick (e.g. TEF)
void quotes_del (char *nick_name);

/// Changes file 'old_name' by 'new_name' if 'old_name' exists
void quotes_modify (char *old_name, char *new_name);

/// Returns is Tp[EMsg, char]
///   nick_id Nick identifier.
///   returns: Tp of:
///     EMsg: whith values:
///       MSG_OK, "" -> No error
///       MSG_WARNING, "quotes" -> Warning: quotes with error
///       MSG_WARNING, "number" -> Warning: Bad quotes number
///       MSG_ERROR, "io" -> Error: File not found
///       MSG_ERROR, "syntax" -> Error: Syntax error in lines
///       MSG_ERROR, "empty" -> Error: There is no quote
///     char: The text file or "" if it does not exists.
Tp *quotes_editor_read(int nick_id);

/// Sets quotes of a company (inclusive quotes model)
///   nick_id: Nick identifier.
///   qs_text: Text with quotes
///   return: An EMsg with values:
///       MSG_OK, "" -> No error
///       MSG_WARNING, "quotes" -> Warning: quotes with error
///       MSG_WARNING, "number" -> Warning: Bad quotes number
///       MSG_ERROR, "io" -> Error: File not found
///       MSG_ERROR, "syntax" -> Error: Syntax error in lines
///       MSG_ERROR, "empty" -> Error: There is no quote
///       MSG_ERROR, "model" -> Error: Nick model was not defined
///       <i>NOTE: MSG_OK and MSG_WARNING modifies data, MSG_ERROR not</i>.
EMsg *quotes_editor_set_quotes(int nick_id, char *qs_text);

#endif