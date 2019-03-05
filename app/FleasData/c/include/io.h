// Copyright 26-Feb-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef IO_H
  #define IO_H

#include "dmc/std.h"

/// Returns [names:String]
Js *io_fmodels_new(void);

/// Returns:
///   [
///     [names:String],
///     [
///       [
///         prefix:String,
///         multiplier:Number,
///         decimals:Number,
///         sufix:String
///       ]
///     ]
///   ]
Js *io_params_new(char *model);

/// Returns data of model best. It is
/// [
Js *io_bests_new(char *model);

/// Returns dates of model results. It is [date:String]
Js *io_dates_new(char *model);

/// Returns results of model and date. To see the return structure see
/// 'io_bests_ new'
Js *io_results_new(char *model, char *date);

/// Returna a companies list. It is [nick:String]
Js *io_charts_list_new(char *model);

/// Returns chart data of a company. It is
///   [
///     [
///       nick:String,
///       profits:Number,
///       [
///         [
///           date:String
///           close:Number
///           ref:Number
///         ]
///       ]
///     ]
///   ]
Js *io_charts_data_new(char *model, char *nick);

#endif
