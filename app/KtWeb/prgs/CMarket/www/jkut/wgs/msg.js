import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as modalBox from  "../libdm/modalBox.js";
import * as i18n from  "../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);

const iwg =sys.$checkNull( Q("div"));
const Box =sys.$checkNull( modalBox.mk(iwg, false));



export const wg =sys.$checkNull( Box.wg);






export  function show(icon, msg, fn)  {sys.$params(arguments.length, 3);
  iwg
    .removeAll()
    .add(Q("table")
      .add(Q("tr")
        .add(Q("td")
          .style("valign:middle;width:50px")
          .att("rowspan", 3)
          .add(ui.img(icon)))
        .add(Q("td")
          .style("text-align:left")
          .html(msg)))
      .add(Q("tr")
        .add(Q("td")
          .add(Q("hr"))))
      .add(Q("tr")
        .add(Q("td")
          .style("text-align: right")
          .add(Q("button")
            .text(II("Close"))
            .on("click", function(e)  {sys.$params(arguments.length, 1);
                modalBox.show(Box, false);
                fn();
              })))))
  ;
  modalBox.show(Box, true);
};





export  function ok(msg, fn)  {sys.$params(arguments.length, 2);
  show("well2", msg, fn);
};





export  function info(msg, fn)  {sys.$params(arguments.length, 2);
  show("info", msg, fn);
};





export  function error(msg, fn)  {sys.$params(arguments.length, 2);
  show("error2", msg, fn);
};





export  function showWg(wg, fn)  {sys.$params(arguments.length, 2);
  iwg
    .removeAll()
    .add(Q("table")
      .add(Q("tr")
        .add(Q("td")
          .style("width:100%;text-align:right;padding-bottom:5px")
          .add(Q("span")
            .text("[ "))
          .add(ui.link(function(e)  {sys.$params(arguments.length, 1); modalBox.show(Box, false);})
            .style(
              "cursor:pointer;text-decoration: none; font-family: sans;" +
              "color: #000080;font-weight: normal;font-size:14px;"
            ).text("X"))
          .add(Q("span")
            .text(" ]"))))
      .add(Q("tr")
        .add(Q("td")
          .add(wg)))
      .add(Q("tr")
        .add(Q("td")
          .add(Q("hr"))))
      .add(Q("tr")
        .add(Q("td")
          .style("text-align: center")
          .add(Q("button")
            .text(II("Close"))
            .on("click", function(e)  {sys.$params(arguments.length, 1);
              modalBox.show(Box, false);
              fn();
            })))))
  ;
  modalBox.show(Box, true);
};
