import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




const Q =sys.$checkNull( ui.q);






export  function mkEntry(Id, wg)  {sys.$params(arguments.length, 2);  return {Id:Id, wg:wg};};









export  function mk(Lopts, Ropts, selected, withSeparator)  {sys.$params(arguments.length, 4);
   function setId(o)  {sys.$params(arguments.length, 1);
    if (sys.asBool(o.Id))
      o.wg.style(sys.asBool(sys.$eq(o.Id[0] , selected))
        ?
          "background-color: rgb(250, 250, 250);" +
          "border: 1px solid rgb(110,130,150);" +
          "padding: 4px;border-radius: 4px;"
        : "text-decoration: none;color: #000080;" +
          "font-weight: normal;cursor:pointer;"
      );
  };
  for (let o  of sys.$forObject( Lopts)) setId(o);
  for (let o  of sys.$forObject( Ropts)) setId(o);

   return Q("div")
    .add(Q("table")
      .style("border-collapse:collapse;width:100%;")
      .add(Q("tr")
        .add(Q("td")
          .style(
            "text-align:left;padding-right:4px;" +
            (sys.asBool(withSeparator) ? 'border-right: 1px solid #000000;' : '')
          )
          .adds(arr.map(Lopts, function(e)  {sys.$params(arguments.length, 1);  return e.wg;})))
        .add(Q("td")
          .style(
            "padding-left:4px;vertical-align:top;" +
            "text-align:right;white-space:nowrap"
          )
          .adds(arr.map(Ropts, function(e)  {sys.$params(arguments.length, 1);  return e.wg;})))))
    .add(Q("hr"))
  ;
};



export  function separator() {sys.$params(arguments.length, 0);  return mkEntry([], Q("span").text(" · "));};



export  function separator2() {sys.$params(arguments.length, 0);  return mkEntry([], Q("span").text(" | "));};






export  function toption(id, tx, fn)  {sys.$params(arguments.length, 3);  return mkEntry([id], ui.link(function(ev)  {sys.$params(arguments.length, 1); fn();}).html(tx));};








export  function ioption(id, img, fn)  {sys.$params(arguments.length, 3);  return mkEntry(
    [id],
    ui.link(function(ev)  {sys.$params(arguments.length, 1); fn();})
      .add(ui.img(img)
        .style("vertical-align:middle"))
  );};











export  function tlink(id, tx, Module)  {sys.$params(arguments.length, 3);  return mkEntry(
    [id],
    Q("a")
      .att("href", "?" + (sys.asBool(Module) ? Module[0] + "&" : "") + id)
      .html(tx)
  );};












export  function ilink(id, img, Module)  {sys.$params(arguments.length, 3);  return mkEntry(
    [id],
    Q("a")
      .att("href", "?" + (sys.asBool(Module) ? Module[0] + "&" : "") + id)
      .add(ui.img(img)
        .style("vertical-align:top"))
  );};




export  function close(fbye)  {sys.$params(arguments.length, 1);  return mkEntry(
    [],
    ui.link(function(ev)  {sys.$params(arguments.length, 1); fbye();})
      .add(ui.img("cross")
        .style("vertical-align:middle"))
  );};
