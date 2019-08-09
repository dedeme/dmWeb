// Copyright 18-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "Data.h"

/* .
DataPreviousGroup: serial
  # Arr[char]
  first: Arr - char *
  # Arr[char]
  second: Arr - char *
  # Arr[char]
  third: Arr - char *
===
DataPrevious: serial
  daily_g: DataPreviousGroup
  short_g: DataPreviousGroup
  medium_g: DataPreviousGroup
  long_g: DataPreviousGroup
===
DataMatchResult: serial
  # Positive -> up wins. Negative down wins.
  dif: double
  # 1 (up wins), 0 (draw), 2 (down wins) or '-1' (Resting)
  result: int
===
DataRoundResult: serial
  results: Arr - DataMatchResult
===
DataLeague: serial
  # Arr[char]
  nicks: Arr - char *
  results: Arr - DataRoundResult
===
DataLeagueGroup: serial
  # 'code' is sum of quotes for daily group and last date for the rest
  code: char *
  # Arr[char]
  first: DataLeague
  # Arr[char]
  second: DataLeague
  # Arr[char]
  third: DataLeague
===
DataCurrent: serial
  daily_g: DataLeagueGroup
  short_g: DataLeagueGroup
  medium_g: DataLeagueGroup
  long_g: DataLeagueGroup
===
DataAll: serial
  previous: DataPrevious
  current: DataCurrent
*/

/*--*/

struct Data_DataPreviousGroup {
  Arr *first;
  Arr *second;
  Arr *third;
};

DataPreviousGroup *dataPreviousGroup_new (Arr *first, Arr *second, Arr *third) {
  DataPreviousGroup *this = MALLOC(DataPreviousGroup);
  this->first = first;
  this->second = second;
  this->third = third;
  return this;
}

Arr *dataPreviousGroup_first (DataPreviousGroup *this) {
  return this->first;
}

Arr *dataPreviousGroup_second (DataPreviousGroup *this) {
  return this->second;
}

Arr *dataPreviousGroup_third (DataPreviousGroup *this) {
  return this->third;
}

Js *dataPreviousGroup_to_js (DataPreviousGroup *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, arr_to_js(this->first, (FTO)js_ws));
  arr_push(js, arr_to_js(this->second, (FTO)js_ws));
  arr_push(js, arr_to_js(this->third, (FTO)js_ws));
  return js_wa(js);
}

DataPreviousGroup *dataPreviousGroup_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  DataPreviousGroup *this = MALLOC(DataPreviousGroup);
  this->first = arr_from_js(*p++, (FFROM)js_rs);
  this->second = arr_from_js(*p++, (FFROM)js_rs);
  this->third = arr_from_js(*p++, (FFROM)js_rs);
  return this;
}

struct Data_DataPrevious {
  DataPreviousGroup *daily_g;
  DataPreviousGroup *short_g;
  DataPreviousGroup *medium_g;
  DataPreviousGroup *long_g;
};

DataPrevious *dataPrevious_new (
  DataPreviousGroup *daily_g,
  DataPreviousGroup *short_g,
  DataPreviousGroup *medium_g,
  DataPreviousGroup *long_g
) {
  DataPrevious *this = MALLOC(DataPrevious);
  this->daily_g = daily_g;
  this->short_g = short_g;
  this->medium_g = medium_g;
  this->long_g = long_g;
  return this;
}

DataPreviousGroup *dataPrevious_daily_g (DataPrevious *this) {
  return this->daily_g;
}

DataPreviousGroup *dataPrevious_short_g (DataPrevious *this) {
  return this->short_g;
}

DataPreviousGroup *dataPrevious_medium_g (DataPrevious *this) {
  return this->medium_g;
}

DataPreviousGroup *dataPrevious_long_g (DataPrevious *this) {
  return this->long_g;
}

Js *dataPrevious_to_js (DataPrevious *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, dataPreviousGroup_to_js(this->daily_g));
  arr_push(js, dataPreviousGroup_to_js(this->short_g));
  arr_push(js, dataPreviousGroup_to_js(this->medium_g));
  arr_push(js, dataPreviousGroup_to_js(this->long_g));
  return js_wa(js);
}

DataPrevious *dataPrevious_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  DataPrevious *this = MALLOC(DataPrevious);
  this->daily_g = dataPreviousGroup_from_js(*p++);
  this->short_g = dataPreviousGroup_from_js(*p++);
  this->medium_g = dataPreviousGroup_from_js(*p++);
  this->long_g = dataPreviousGroup_from_js(*p++);
  return this;
}

struct Data_DataMatchResult {
  double dif;
  int result;
};

DataMatchResult *dataMatchResult_new (double dif, int result) {
  DataMatchResult *this = MALLOC(DataMatchResult);
  this->dif = dif;
  this->result = result;
  return this;
}

double dataMatchResult_dif (DataMatchResult *this) {
  return this->dif;
}

int dataMatchResult_result (DataMatchResult *this) {
  return this->result;
}

Js *dataMatchResult_to_js (DataMatchResult *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_wd(this->dif));
  arr_push(js, js_wi((int)this->result));
  return js_wa(js);
}

DataMatchResult *dataMatchResult_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  DataMatchResult *this = MALLOC(DataMatchResult);
  this->dif = js_rd(*p++);
  this->result = js_ri(*p++);
  return this;
}

struct Data_DataRoundResult {
  Arr *results;
};

DataRoundResult *dataRoundResult_new (Arr *results) {
  DataRoundResult *this = MALLOC(DataRoundResult);
  this->results = results;
  return this;
}

Arr *dataRoundResult_results (DataRoundResult *this) {
  return this->results;
}

Js *dataRoundResult_to_js (DataRoundResult *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, arr_to_js(this->results, (FTO)dataMatchResult_to_js));
  return js_wa(js);
}

DataRoundResult *dataRoundResult_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  DataRoundResult *this = MALLOC(DataRoundResult);
  this->results = arr_from_js(*p++, (FFROM)dataMatchResult_from_js);
  return this;
}

struct Data_DataLeague {
  Arr *nicks;
  Arr *results;
};

DataLeague *dataLeague_new (Arr *nicks, Arr *results) {
  DataLeague *this = MALLOC(DataLeague);
  this->nicks = nicks;
  this->results = results;
  return this;
}

Arr *dataLeague_nicks (DataLeague *this) {
  return this->nicks;
}

Arr *dataLeague_results (DataLeague *this) {
  return this->results;
}

Js *dataLeague_to_js (DataLeague *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, arr_to_js(this->nicks, (FTO)js_ws));
  arr_push(js, arr_to_js(this->results, (FTO)dataRoundResult_to_js));
  return js_wa(js);
}

DataLeague *dataLeague_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  DataLeague *this = MALLOC(DataLeague);
  this->nicks = arr_from_js(*p++, (FFROM)js_rs);
  this->results = arr_from_js(*p++, (FFROM)dataRoundResult_from_js);
  return this;
}

struct Data_DataLeagueGroup {
  char *code;
  DataLeague *first;
  DataLeague *second;
  DataLeague *third;
};

DataLeagueGroup *dataLeagueGroup_new (
  char *code,
  DataLeague *first,
  DataLeague *second,
  DataLeague *third
) {
  DataLeagueGroup *this = MALLOC(DataLeagueGroup);
  this->code = code;
  this->first = first;
  this->second = second;
  this->third = third;
  return this;
}

char *dataLeagueGroup_code (DataLeagueGroup *this) {
  return this->code;
}

DataLeague *dataLeagueGroup_first (DataLeagueGroup *this) {
  return this->first;
}

DataLeague *dataLeagueGroup_second (DataLeagueGroup *this) {
  return this->second;
}

DataLeague *dataLeagueGroup_third (DataLeagueGroup *this) {
  return this->third;
}

Js *dataLeagueGroup_to_js (DataLeagueGroup *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->code));
  arr_push(js, dataLeague_to_js(this->first));
  arr_push(js, dataLeague_to_js(this->second));
  arr_push(js, dataLeague_to_js(this->third));
  return js_wa(js);
}

DataLeagueGroup *dataLeagueGroup_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  DataLeagueGroup *this = MALLOC(DataLeagueGroup);
  this->code = js_rs(*p++);
  this->first = dataLeague_from_js(*p++);
  this->second = dataLeague_from_js(*p++);
  this->third = dataLeague_from_js(*p++);
  return this;
}

struct Data_DataCurrent {
  DataLeagueGroup *daily_g;
  DataLeagueGroup *short_g;
  DataLeagueGroup *medium_g;
  DataLeagueGroup *long_g;
};

DataCurrent *dataCurrent_new (
  DataLeagueGroup *daily_g,
  DataLeagueGroup *short_g,
  DataLeagueGroup *medium_g,
  DataLeagueGroup *long_g
) {
  DataCurrent *this = MALLOC(DataCurrent);
  this->daily_g = daily_g;
  this->short_g = short_g;
  this->medium_g = medium_g;
  this->long_g = long_g;
  return this;
}

DataLeagueGroup *dataCurrent_daily_g (DataCurrent *this) {
  return this->daily_g;
}

DataLeagueGroup *dataCurrent_short_g (DataCurrent *this) {
  return this->short_g;
}

DataLeagueGroup *dataCurrent_medium_g (DataCurrent *this) {
  return this->medium_g;
}

DataLeagueGroup *dataCurrent_long_g (DataCurrent *this) {
  return this->long_g;
}

Js *dataCurrent_to_js (DataCurrent *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, dataLeagueGroup_to_js(this->daily_g));
  arr_push(js, dataLeagueGroup_to_js(this->short_g));
  arr_push(js, dataLeagueGroup_to_js(this->medium_g));
  arr_push(js, dataLeagueGroup_to_js(this->long_g));
  return js_wa(js);
}

DataCurrent *dataCurrent_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  DataCurrent *this = MALLOC(DataCurrent);
  this->daily_g = dataLeagueGroup_from_js(*p++);
  this->short_g = dataLeagueGroup_from_js(*p++);
  this->medium_g = dataLeagueGroup_from_js(*p++);
  this->long_g = dataLeagueGroup_from_js(*p++);
  return this;
}

struct Data_DataAll {
  DataPrevious *previous;
  DataCurrent *current;
};

DataAll *dataAll_new (DataPrevious *previous, DataCurrent *current) {
  DataAll *this = MALLOC(DataAll);
  this->previous = previous;
  this->current = current;
  return this;
}

DataPrevious *dataAll_previous (DataAll *this) {
  return this->previous;
}

DataCurrent *dataAll_current (DataAll *this) {
  return this->current;
}

Js *dataAll_to_js (DataAll *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, dataPrevious_to_js(this->previous));
  arr_push(js, dataCurrent_to_js(this->current));
  return js_wa(js);
}

DataAll *dataAll_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  DataAll *this = MALLOC(DataAll);
  this->previous = dataPrevious_from_js(*p++);
  this->current = dataCurrent_from_js(*p++);
  return this;
}

/*--*/

Kv *dataAll_check_nicks (DataAll *this) {

  DataPrevious *previous = dataAll_previous(this);
  DataPreviousGroup *pdaily = dataPrevious_daily_g(previous);
  DataPreviousGroup *pshort = dataPrevious_short_g(previous);
  DataPreviousGroup *pmedium = dataPrevious_medium_g(previous);
  DataPreviousGroup *plong = dataPrevious_long_g(previous);

  DataCurrent *current = dataAll_current(this);
  DataLeagueGroup *ldaily = dataCurrent_daily_g(current);
  DataLeagueGroup *lshort = dataCurrent_short_g(current);
  DataLeagueGroup *lmedium = dataCurrent_medium_g(current);
  DataLeagueGroup *llong = dataCurrent_long_g(current);

  // Arr[char]
  Arr *cat (DataPreviousGroup *gr) {
    return it_to(it_cat(
      it_from(dataPreviousGroup_first(gr)),
      it_cat(
        it_from(dataPreviousGroup_second(gr)),
        it_from(dataPreviousGroup_third(gr))
      )
    ));
  }

  // Arr[char]
  int ok (Arr *a1, Arr *a2) {
    if (arr_size(a1) != arr_size(a2)) return 0;
    EACH(a1, char, e1)
      int missing = 1;
      EACH(a2, char, e2)
        if (str_eq(e1, e2)) {
          missing = 0;
          break;
        }
      _EACH
      if (missing) return 0;
    _EACH
    return 1;
  }

  char *okpc (DataPreviousGroup *pr, DataLeagueGroup *lg) {
    if (ok(
      dataPreviousGroup_first(pr),
      dataLeague_nicks(dataLeagueGroup_first(lg))
    )) {
      if (ok(
        dataPreviousGroup_second(pr),
        dataLeague_nicks(dataLeagueGroup_second(lg))
      )) {
        if (ok(
          dataPreviousGroup_third(pr),
          dataLeague_nicks(dataLeagueGroup_third(lg))
        )) {
          return "";
        }
        return "previus != current;3";
      }
      return "previus != current;2";
    }
    return "previus != current;1";
  }

  char *rs = okpc(pdaily, ldaily);
  if (*rs) {
    return kv_new(str_f("%s;DAILY", rs), arr_new());
  }
  rs = okpc(pshort, lshort);
  if (*rs) {
    return kv_new(str_f("%s;SHORT", rs), arr_new());
  }
  rs = okpc(pmedium, lmedium);
  if (*rs) {
    return kv_new(str_f("%s;MEDIUM", rs), arr_new());
  }
  rs = okpc(plong, llong);
  if (*rs) {
    return kv_new(str_f("%s;LONG", rs), arr_new());
  }

  /// Arr [char]
  Arr *nicks = cat(pdaily);

  if (ok(nicks, cat(pshort))) {
    if (ok(nicks, cat(pmedium))) {
      if (ok(nicks, cat(plong))) {
        return kv_new("", nicks);
      }
      return kv_new("group != group;DAILY;MEDIUM", arr_new());
    }
    return kv_new("group != group;DAILY;MEDIUM", arr_new());
  }
  return kv_new("group != group;DAILY;SHORT", arr_new());
}
