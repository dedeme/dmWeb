// Copyright 04-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/NickSets.h"

/* .
NickSets
  # Arr[Nick]
  win: Arr - Nick
  # Arr[Nick]
  loss: Arr - Nick
  # Arr[Nick]
  semi_win: Arr - Nick
  # Arr[Nick]
  semi_loss: Arr - Nick
*/

/*--*/

struct NickSets_NickSets {
  Arr *win;
  Arr *loss;
  Arr *semi_win;
  Arr *semi_loss;
};

NickSets *nickSets_new (
  Arr *win,
  Arr *loss,
  Arr *semi_win,
  Arr *semi_loss
) {
  NickSets *this = MALLOC(NickSets);
  this->win = win;
  this->loss = loss;
  this->semi_win = semi_win;
  this->semi_loss = semi_loss;
  return this;
}

Arr *nickSets_win (NickSets *this) {
  return this->win;
}

Arr *nickSets_loss (NickSets *this) {
  return this->loss;
}

Arr *nickSets_semi_win (NickSets *this) {
  return this->semi_win;
}

Arr *nickSets_semi_loss (NickSets *this) {
  return this->semi_loss;
}

/*--*/
