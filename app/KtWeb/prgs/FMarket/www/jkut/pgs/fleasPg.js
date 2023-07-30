import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as menu from  "../libdm/menu.js";
import * as cts from  "../data/cts.js";
import * as flea from  "../data/flea.js";
import * as fns from  "../fns.js";
import * as i18n from  "../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


 function mk2(wg, Fleas, order)  {sys.$params(arguments.length, 3);
  switch (order) {
    case "1id":{
      arr.sort(Fleas, function(F1, F2)  {sys.$params(arguments.length, 2);  return F1.id < F2.id;});break;}
    case "2id":{
      arr.sort(Fleas, function(F1, F2)  {sys.$params(arguments.length, 2);  return F1.id > F2.id;});break;}
    case "1cy":{
      arr.sort(Fleas, function(F1, F2)  {sys.$params(arguments.length, 2); return sys.asBool( sys.$eq(F1.cycle , F2.cycle))
        ? flea.greater(F1, F2)
        : F1.cycle > F2.cycle
      ;});break;}
    case "2cy":{
      arr.sort(Fleas, function(F1, F2)  {sys.$params(arguments.length, 2); return sys.asBool( sys.$eq(F1.cycle , F2.cycle))
        ? flea.greater(F1, F2)
        : F1.cycle < F2.cycle
      ;});break;}
    case "1p1":{
      arr.sort(Fleas, function(F1, F2)  {sys.$params(arguments.length, 2); return sys.asBool( sys.$eq(F1.params[0] , F2.params[0]))
        ? flea.greater(F1, F2)
        : F1.params[0] < F2.params[0]
      ;});break;}
    case "2p1":{
      arr.sort(Fleas, function(F1, F2)  {sys.$params(arguments.length, 2); return sys.asBool( sys.$eq(F1.params[0] , F2.params[0]))
        ? flea.greater(F1, F2)
        : F1.params[0] > F2.params[0]
      ;});break;}
    case "1p2":{
      arr.sort(Fleas, function(F1, F2)  {sys.$params(arguments.length, 2); return sys.asBool( sys.$eq(F1.params[1] , F2.params[1]))
        ? flea.greater(F1, F2)
        : F1.params[1] < F2.params[1]
      ;});break;}
    case "2p2":{
      arr.sort(Fleas, function(F1, F2)  {sys.$params(arguments.length, 2); return sys.asBool( sys.$eq(F1.params[1] , F2.params[1]))
        ? flea.greater(F1, F2)
        : F1.params[1] > F2.params[1]
      ;});break;}
    case "1as":{
      arr.sort(Fleas, function(F1, F2)  {sys.$params(arguments.length, 2); return sys.asBool( sys.$eq(F1.assets , F2.assets))
        ? flea.greater(F1, F2)
        : F1.assets > F2.assets
      ;});break;}
    case "2as":{
      arr.sort(Fleas, function(F1, F2)  {sys.$params(arguments.length, 2); return sys.asBool( sys.$eq(F1.assets , F2.assets))
        ? flea.greater(F1, F2)
        : F1.assets < F2.assets
      ;});break;}
    case "1pr":{
      arr.sort(Fleas, function(F1, F2)  {sys.$params(arguments.length, 2); return sys.asBool( sys.$eq(F1.profits , F2.profits))
        ? flea.greater(F1, F2)
        : F1.profits > F2.profits
      ;});break;}
    case "2pr":{
      arr.sort(Fleas, function(F1, F2)  {sys.$params(arguments.length, 2); return sys.asBool( sys.$eq(F1.profits , F2.profits))
        ? flea.greater(F1, F2)
        : F1.profits < F2.profits
      ;});break;}
    case "1po":{
      arr.sort(Fleas, flea.greater);break;}
    case "2po":{
      arr.sort(Fleas, function(F1, F2)  {sys.$params(arguments.length, 2);  return !sys.asBool(flea.greater(F1, F2));});break;}
    case "1sa":{
      arr.sort(Fleas, function(F1, F2)  {sys.$params(arguments.length, 2); return sys.asBool( sys.$eq(F1.sales , F2.sales))
        ? flea.greater(F1, F2)
        : F1.profits > F2.profits
      ;});break;}
    case "2sa":{
      arr.sort(Fleas, function(F1, F2)  {sys.$params(arguments.length, 2); return sys.asBool( sys.$eq(F1.sales , F2.sales))
        ? flea.greater(F1, F2)
        : F1.profits < F2.profits
      ;});break;}
    default:{
      arr.sort(Fleas, flea.greater);}
  }
  const F0 =sys.$checkNull( Fleas[0]);
  const npars =sys.$checkNull( arr.size(F0.params));
  const isDate =sys.$checkNull( arr.any(
    ["ME", "ME2", "MM", "MX_MN"],
    function(m)  {sys.$params(arguments.length, 1);  return sys.$eq(m , F0.model);}
  ));

  

   function setOrder(o)  {sys.$params(arguments.length, 1);
    if (sys.asBool(sys.$eq("1" + o , order))) mk2(wg, Fleas, "2" + o);
    else mk2(wg, Fleas, "1" + o);}
  ;

  

  const rId =sys.$checkNull( 0);
  const rCy =sys.$checkNull( 1);
  const rP1 =sys.$checkNull( 2);
  const rP2 =sys.$checkNull( 3);
  const rAs =sys.$checkNull( 4);
  const rPr =sys.$checkNull( 5);
  const rPo =sys.$checkNull( 6);
  const rSa =sys.$checkNull( 7);
  const Rows =sys.$checkNull( arr.map(Fleas, function(F)  {sys.$params(arguments.length, 1);  return [
      F.id, F.cycle, F.params[0],sys.asBool(
      sys.$eq(npars , 2)) ? F.params[1] :  -1,
      F.assets, F.profits, F.points, F.sales
    ];}));

  wg
    .removeAll()
    .add(Q("table")
      .klass("border")
      .att("align", "center")
      .add(Q("tr")
        .adds(function()  {sys.$params(arguments.length, 0);
            const Tds =sys.$checkNull( []);
            arr.push(Tds, fns.mkTh(II("Id."), function() {sys.$params(arguments.length, 0); setOrder("id");}));
            arr.push(Tds, fns.mkTh(II("Cy."), function() {sys.$params(arguments.length, 0); setOrder("cy");}));
            arr.push(Tds, fns.mkTh(II("P1"), function() {sys.$params(arguments.length, 0); setOrder("p1");}));
            if (sys.asBool(npars > 1)) arr.push(Tds, fns.mkTh(II("P2"), function() {sys.$params(arguments.length, 0); setOrder("p2");}));
            arr.push(Tds, fns.mkTh(II("Assets"), function() {sys.$params(arguments.length, 0); setOrder("as");}));
            arr.push(Tds, fns.mkTh(II("Profits"), function() {sys.$params(arguments.length, 0); setOrder("pr");}));
            arr.push(Tds, fns.mkTh(II("Points"), function() {sys.$params(arguments.length, 0); setOrder("po");}));
            arr.push(Tds, fns.mkTh(II("Sales"), function() {sys.$params(arguments.length, 0); setOrder("sa");}));
             return Tds;
          }()))
      .adds(arr.map(Rows, function(R)  {sys.$params(arguments.length, 1);
          const Tds =sys.$checkNull( []);
          arr.push(Tds, fns.mkTdN(R[rId], 0));
          arr.push(Tds, fns.mkTdN(R[rCy], 0));
          arr.push(Tds, fns.mkTdN(R[rP1],sys.asBool( isDate) ? 0 : 4));
          if (sys.asBool(npars > 1)) arr.push(Tds, fns.mkTdN(R[rP2], 4));
          arr.push(Tds, fns.mkTdN(R[rAs], 2));
          arr.push(Tds, fns.mkTdN(R[rPr], 4));
          arr.push(Tds, fns.mkTdN(R[rPo], 0));
          arr.push(Tds, fns.mkTdN(R[rSa], 0));
           return Q("tr").adds(Tds);
        })))
  ;

};


export  async  function mk(wg)  {sys.$params(arguments.length, 1);
  const Url =sys.$checkNull( ui.url());
  const modelId =sys.$checkNull(sys.asBool( dic.hasKey(Url, "1")) ? Url["1"] : "");

  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    source: "FleasPg",
    rq: "idata",
    modelId:modelId
  }));

  const mdId =sys.$checkNull( Rp.mdId);
  const Models =sys.$checkNull( Rp.models);
  const Fleas =sys.$checkNull( arr.map(Rp.fleas, flea.fromJs));
  const page =sys.$checkNull( "fleas&" + mdId);

  const Lopts =sys.$checkNull( arr.reduce(
    Models, [],
    function(R, mId)  {sys.$params(arguments.length, 2);
      arr.push(R, menu.tlink("fleas&" + mId, mId, []));
      arr.push(R, menu.separator());
       return R;
    }
  ));
  arr.pop(Lopts);

  const menuWg =sys.$checkNull( menu.mk(Lopts, [], page, false));

  const body =sys.$checkNull( Q("div"));

  wg
    .removeAll()
    .add(menuWg)
    .add(body)
  ;

  mk2(body, Fleas, "1po");
};
