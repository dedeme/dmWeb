import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




const Q =sys.$checkNull( ui.q);








export  function mk(width, height, ratio, foreground, background)  {sys.$params(arguments.length, 5);  return [
    width, height, ratio, foreground, background
  ];};



export  function withWidth(b, value)  {sys.$params(arguments.length, 2);
  const R =sys.$checkNull( arr.copy(b));
  R[0] =sys.$checkExists(R[0],sys.$checkNull( value));
   return R;
};



export  function withHeight(b, value)  {sys.$params(arguments.length, 2);
  const R =sys.$checkNull( arr.copy(b));
  R[1] =sys.$checkExists(R[1],sys.$checkNull( value));
   return R;
};



export  function withRatio(b, value)  {sys.$params(arguments.length, 2);
  const R =sys.$checkNull( arr.copy(b));
  R[2] =sys.$checkExists(R[2],sys.$checkNull( value));
   return R;
};



export  function withForeground(b, value)  {sys.$params(arguments.length, 2);
  const R =sys.$checkNull( arr.copy(b));
  R[3] =sys.$checkExists(R[3],sys.$checkNull( value));
   return R;
};



export  function withBackground(b, value)  {sys.$params(arguments.length, 2);
  const R =sys.$checkNull( arr.copy(b));
  R[4] =sys.$checkExists(R[4],sys.$checkNull( value));
   return R;
};



export  function mkWg(B)  {sys.$params(arguments.length, 1);
  const foreWidth =sys.$checkNull( math.toInt(B[2] * B[0]));
   return Q("table")
    .att("align", "left")
    .style(
      "border : 1px solid rgb(110,130,150);" +
      "border-collapse : collapse;" +
      "background-color : " + B[4] + ";" +
      "width: " + B[0] + "px;"
    )
    .add(Q("tr")
      .add(Q("td")
        .style(
          "border : 1px solid rgb(110,130,150);" +
          "background-color : " + B[3] + ";" +
          "width: " + foreWidth + "px;" +
          "height: " + B[1] + "px;"
        ))
      .add(Q("td")))
  ;
};
