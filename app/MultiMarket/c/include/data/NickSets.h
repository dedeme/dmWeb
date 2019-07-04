// Copyright 04-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_NICKSETS_H
  #define DATA_NICKSETS_H

#include "dmc/std.h"

/*--*/

///
typedef struct NickSets_NickSets NickSets;

///
NickSets *nickSets_new(
  Arr *win,
  Arr *loss,
  Arr *semi_win,
  Arr *semi_loss
);

/// Arr[Nick]
Arr *nickSets_win(NickSets *this);

/// Arr[Nick]
Arr *nickSets_loss(NickSets *this);

/// Arr[Nick]
Arr *nickSets_semi_win(NickSets *this);

/// Arr[Nick]
Arr *nickSets_semi_loss(NickSets *this);

/*--*/

#endif
