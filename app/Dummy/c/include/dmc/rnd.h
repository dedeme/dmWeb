// Copyright 17-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Generator of random numbers.

#ifndef DMC_RND_H
  #define DMC_RND_H

#include "dmc/Varr.h"

///
typedef struct rnd_Box Box;

/// rnd_init intializates the random number generator.
void rnd_init(void);

/// rnd_d generates a new double between 0.0 (inclusive) and 1.0 (exclusive)
double rnd_d(void);

/// rnd_i generates a new int between 0 (inclusive) and 'top' (exclusive)
int rnd_i(int top);

/// rnd_box_new returns an 'Box' that iterates over 'a' elements randomly. When
/// it finishes with all the elements of 'a', restarts again.
Box *rnd_box_new(Varr *a);

///
void rnd_box_free(Box *this);

/// rnd_box_next returns the next element of 'this'.
void *rnd_box_next(Box *this);

#endif
