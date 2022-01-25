// Copyright 12-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Configuration data.

#ifndef DATA_CONF_H
  #define DATA_CONF_H

/// Configuration data.
struct conf_Conf {
  char *lang; // Language.
};

/// Configuration data.
typedef struct conf_Conf Conf;

///
Conf *conf_new (char *lang);

///
char *conf_to_js (Conf *this);

///
Conf *conf_from_js (char *js);

#endif
