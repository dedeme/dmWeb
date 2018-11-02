// Copyright 17-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Utilities for managing file paths.

#ifndef DMC_PATH_H
  #define DMC_PATH_H

/// path_name sets name and extension of path.<br>
/// If path is "" or ends at ("/") it sets "".
void path_name (char **path);

/// path_parent sets the parent path of 'path'.<br>
/// If 'path' is "/" or a string without '/', it sets an empty string.
void path_parent (char **path);

/// path_extendsion sets only extension of path. Extension is
/// set with point (e.g., ".", ".txt") <br>
/// If path does not have extension it sets "".
void path_extension (char **path);

/// path_only_name sets only name of path.<br>
/// If path is "", ends at ("/"), or if file starts with point, it sets "".
void path_only_name (char **path);

/// path_cat concatenates paths. Variable argumens must finish with NULL.
char *path_cat_new (const char *s, const char *more, ...);

#endif
