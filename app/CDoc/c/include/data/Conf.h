// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Configuration data.

#ifndef DATA_CONF_H
  #define DATA_CONF_H

/// Configuration data.
struct Conf_Conf {
  char *path; // Default source path.
  char *lang; // Language.
  int show_all; // 'true' if all libraries should be shown.
};

/// Configuration data.
typedef struct Conf_Conf Conf;

///
Conf *conf_new (char *path, char *lang, int show_all);

///
char *conf_to_js (Conf *this);

///
Conf *conf_from_js (char *js);

#endif
