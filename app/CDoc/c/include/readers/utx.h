// Copyright 11-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef READERS_UTX_H
  #define READERS_UTX_H

/// Returns 'true' id ch is a letter.
int utx_is_l (char ch);

/// Returns 'true' id ch is a letter or digit.
int utx_is_ld (char ch);

/// Create a code link.
char *utx_mk_link (char *s);

/// Reads a identifier in 'tx' from 'i' inclusive. 'tx[i]' can be a blank.
char *utx_read_name (char *tx, int i);

/// Reads back a identifier in 'tx' from 'i' inclusive.
char *utx_read_namebk (char *tx, int i);

#endif
