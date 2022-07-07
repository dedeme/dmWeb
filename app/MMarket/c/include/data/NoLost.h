// Copyright 29-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Data for No-Lost calculation of model.

#ifndef DATA_NOLOST_H
  #define DATA_NOLOST_H

#include "dmc/ADouble.h"
#include "OrderNL/AOrderNL.h"

/// Data for No-Lost calculation of model.
struct noLost_NoLost {
  ADouble *assets;
  AOrderNL *orders;
};

/// Order send in Operations page.
typedef struct noLost_NoLost NoLost;

///
NoLost *noLost_new (ADouble *assets, AOrderNL *orders);

///
char *noLost_to_js (NoLost *this);

///
NoLost *noLost_from_js (char *);

#endif
