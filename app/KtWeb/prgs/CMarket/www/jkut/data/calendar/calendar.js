import * as iter from '../../_js/iter.js';import * as str from '../../_js/str.js';import * as bytes from '../../_js/bytes.js';import * as cryp from '../../_js/cryp.js';import * as dic from '../../_js/dic.js';import * as timer from '../../_js/timer.js';import * as js from '../../_js/js.js';import * as storage from '../../_js/storage.js';import * as sys from '../../_js/sys.js';import * as math from '../../_js/math.js';import * as domo from '../../_js/domo.js';import * as ui from '../../_js/ui.js';import * as arr from '../../_js/arr.js';import * as time from '../../_js/time.js';import * as client from '../../_js/client.js';import * as b64 from '../../_js/b64.js';




import * as timetable from  "../../data/calendar/timetable.js";
import * as marketDay from  "../../data/calendar/marketDay.js";






export  function mk(General, Holidays, SpecialDays)  {sys.$params(arguments.length, 3);
   return {General:General, Holidays:Holidays, SpecialDays:SpecialDays};};


export  function toJs(C)  {sys.$params(arguments.length, 1);  return [
    timetable.toJs(C.General),
    C.Holidays,
    arr.map(C.SpecialDays, marketDay.toJs)
  ];};


export  function fromJs(A)  {sys.$params(arguments.length, 1);  return mk(
    timetable.fromJs(A[0]),
    A[1],
    arr.map(A[2], marketDay.fromJs)
  );};
