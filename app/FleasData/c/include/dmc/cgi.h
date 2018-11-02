// Copyright 18-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Utilities for HTML conections between client - server

#ifndef DMC_CGI_H
  #define DMC_CGI_H

#include "dmc/std.h"

/// cgi_klen returns the standard length of keys.
int cgi_klen(void);

/// cgi_init initializes a new interface of commnications.
///   home        : Aboslute path of application directory below wwwcgi/dmcgi
///                 directory. (e.g. /peter/wwwcgi/dmcgi/JsMon)
///   t_expiration: Time in seconds.
void cgi_init(const char *home, time_t t_expiration);

/// Finalizes cgi session and frees resources
void cgi_end(void);

///
char *cgi_home(void);

/// cgi_set_key sets the key which 'cgi_ok' and 'cgi_err' will use. This
/// function is called when connection or authentication.
void cgi_set_key(const char *k);

/// cgi_get_key returns 'key' and 'connectionId'. If session_id is wrong its
/// values are "". This function is called when normal requests.
void cgi_get_session_data(
  char **key_new, char **connectionId_new, const char *session_id
);

/// cgi_add_usser adds an user to database and send response.
///   admin: Admin name
///   akey: Admin password
///   user: New user name
///   ukey: New user password
///   level: New user level. Level "0" is the admin one.
///   Response: A boolean field {ok:true|false}, sets to true if
///     operation succeeded. A fail can come up if 'admin' authentication fails.
void cgi_add_user(
  const char *admin, const char *akey,
  const char *user, const char *ukey, const char *level
);

/// cgi_del_user remove an user from database and send response
///   this:
///   admin: Admin name
///   akey: Admin password
///   user: User name to remove
///   Response: A boolean field {ok:true|false}, sets to true if
///     operation succeeded. A fail can come up if 'admin' authentication fails.
void cgi_del_user(const char *admin, const char *akey, const char *user);

/// cgi_change_level modify a user level and send response
///   this:
///   admin: Admin name
///   akey: Admin password
///   user: User name to change level
///   level: New user level. Level "0" is the admin one.
///   Response: A boolean field {ok:true|false}, sets to true if
///     operation succeeded. A fail can come up if 'admin' authentication fails
///     or 'user' does not exist.
void cgi_change_level(
  const char *admin, const char *akey, const char *user, const char *level
);

/// cgi_change pass change a user password and send response
///   this:
///   user: User name to change password
///   key: Old password
///   new_key: New password
///   Response: A boolean field {ok:true|false}, sets to true if
///     operation succeeded. A fail can come up if 'user' authentication fails.
void cgi_change_pass(const char *user, const char *key, const char *new_key);

/// cgi_del_session deletes 'session' and return an empty response.
void cgi_del_session(const char *session_id);

/// cgi_authentication send to client level, key, page_id and session_id.
/// If authentication failed every value is "".
///   this:
///   user: User name
///   key: User password
///   expiration: If is set to false, session will expire after 30 days.
///   Response: 'level', 'key', 'pageId' and 'sessionId' fields.
void cgi_authentication(const char *user, const char *key, int expiration);

/// cig_connect send client 'connectionId' and 'key'. If conection failed
/// both are "".<br>
/// 'key' is a new key, set for the new connection.
void cgi_connect(const char  *session_id);

/// cgi_ok send a normal response.<br>
/// 'data' is a Map[Js]
void cgi_ok(Map *data);

/// cgi_empty send an empty response.
void cgi_empty(void);

/// cgi_ok send an error response, setting {error:msg}.
void cgi_error(const char *msg);

/// cgi_expired and expired response, setting {expired:true, error:""}
void cgi_expired(void);

#endif
