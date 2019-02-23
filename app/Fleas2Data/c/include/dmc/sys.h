// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Utilities for managing globals values.<br>
/// A way to initializes system is:
///   sys_init("app_name");
///   sys_locale("es_ES.utf8");

#ifndef DMC_SYS_H
  #define DMC_SYS_H

/// Initializates a normal program and call 'rnd_init()'.<br>
/// After call this function 'sys_home' and 'sys_user' are available.<p>
/// It creates the user directory in "~/.dmCApp/" + 'path'
void sys_init (const char *path);

/// Frees system resources.
void sys_end(void);

///
char *sys_home (void);

///
char *sys_uname (void);

/// Sets LC_ALL, for example: <tt>sys_locale("es_ES.utf8")</tt>
void sys_locale (const char *language);

/// Executes 'command', redirecting stderr to stdout, and returns its standard
/// out and standard error. If command fails, function
/// returns NULL.
char *sys_cmd_new_null(const char *command);

/// sys_fail prints a message an exits with code 1.<p>
///   message: Message to show
///   file   : Error file
///   func   : Error function
///   line   : Error line number
void sys_fail(
  const char *message,
  const char *file,
  const char *func,
  int line
);


#endif
