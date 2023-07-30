import * as iter from '../../../_js/iter.js';import * as str from '../../../_js/str.js';import * as bytes from '../../../_js/bytes.js';import * as cryp from '../../../_js/cryp.js';import * as dic from '../../../_js/dic.js';import * as timer from '../../../_js/timer.js';import * as js from '../../../_js/js.js';import * as storage from '../../../_js/storage.js';import * as sys from '../../../_js/sys.js';import * as math from '../../../_js/math.js';import * as domo from '../../../_js/domo.js';import * as ui from '../../../_js/ui.js';import * as arr from '../../../_js/arr.js';import * as time from '../../../_js/time.js';import * as client from '../../../_js/client.js';import * as b64 from '../../../_js/b64.js';




import * as menu from  "../../../libdm/menu.js";
import * as datePicker from  "../../../libdm/datePicker.js";
import * as msg from  "../../../wgs/msg.js";
import * as cts from  "../../../data/cts.js";
import * as ann from  "../../../data/acc/ann.js";
import * as opr from  "../../../data/acc/opr.js";
import * as annotationsWg from  "../../../pgs/settings/acc/annotationsWg.js";
import * as i18n from  "../../../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


 function mkField(name, wg, width)  {sys.$params(arguments.length, 3);  return {name:name, wg:wg, width:width};};


 function checkEmpty(field, value)  {sys.$params(arguments.length, 2);
  if (sys.asBool(value))  return "";
  else  return i18n.fmt(II("'%0' is empty"), [field]);}
;


 function checkInt(field, value)  {sys.$params(arguments.length, 2);
  if (sys.asBool(sys.asBool(value) && sys.asBool(math.isDigits(value))))  return "";
  else  return i18n.fmt(II("'%0' is not a positive integer"), [field]);}
;


 function checkFloat(field, value)  {sys.$params(arguments.length, 2);
  if (sys.asBool(sys.asBool(value) && sys.asBool(math.fromIso(value))))  return "";
  else  return i18n.fmt(II("'%0' is not a positive number"), [field]);}
;


 function dt2s(d)  {sys.$params(arguments.length, 1);  return time.toStr(time.fromIso(d, "/"));};


export  async  function mk2(wg, inv, year0)  {sys.$params(arguments.length, 3);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    module: "Settings",
    source: "acc/InvWg",
    rq: "idata",
    inv:inv,
    year: year0
  }));
  if (sys.asBool(!sys.asBool(Rp.ok)))
    msg.error(II("Some error was found.<br>See Log."), function(){sys.$params(arguments.length, 0);});
  const year =sys.$checkNull( Rp.year); 
  const Years =sys.$checkNull( Rp.years); 
  const Anns =sys.$checkNull( arr.map(Rp.anns, ann.fromJs));
  arr.sort(Anns, function(A1, A2)  {sys.$params(arguments.length, 2);  return A1.date > A2.date;}); 
  const cash =sys.$checkNull( Rp.cash); 

  const editor =sys.$checkNull( Q("div"));
  const dtPck =sys.$checkNull( datePicker.mk(true, time.now(), function(d){sys.$params(arguments.length, 1);}));

  const ShowSelector =sys.$checkNull( [[]]);

  

  
   async  function sendAnn(Ann)  {sys.$params(arguments.length, 1);
    await client.ssend({
      prg: cts.appName,
      module: "Settings",
      source: "acc/InvWg",
      rq: "new",
      inv:inv,
      annotation: ann.toJs(Ann)
    });
    mk2(wg, inv, year);
  };

  
   async  function del(annId)  {sys.$params(arguments.length, 1);
    const Ann =sys.$checkNull( arr.find(Anns, function(A)  {sys.$params(arguments.length, 1);  return sys.$eq(A.id , annId);}));
    if (sys.asBool(!sys.asBool(Ann))) {
      ui.alert(i18n.fmt(II("Annotation con id '%0' not found"), ["" + annId]));
      return;
    }
    const AnnJs =sys.$checkNull( ann.toJs(Ann[0]));
    if (sys.asBool(ui.confirm(i18n.fmt(II("Delete %0?"), [js.w(AnnJs)])))) {
      await client.ssend({
        prg: cts.appName,
        module: "Settings",
        source: "acc/InvWg",
        rq: "del",
        inv:inv,
        annId:annId
      });
      mk2(wg, inv, year);
    }
  };

  
   function changeYear(y)  {sys.$params(arguments.length, 1); mk2(wg, inv, y);};

  

  
   function mkTypeDate(tp, dt)  {sys.$params(arguments.length, 2);
    dt.style("width:80px;text-align:center");
     return Q("tr").add(Q("td")
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .klass("frame2")
            .style("text-align:center;vertical-align:bottom;")
            .html("<big><b>" + tp + "</big></b>"))
          .add(Q("td")
            .add(Q("table")
              .att("align", "right")
              .add(Q("tr")
                .add(Q("td").style("text-align:center;")
                  .html(II("Date"))))
              .add(Q("tr")
                .add(Q("td")
                .add(datePicker.mkText(dtPck, dt)))))))))
    ;
  };

  
   function mkFields(Fields)  {sys.$params(arguments.length, 1);
     return Q("table")
      .klass("main")
      .add(Q("tr")
        .adds(arr.map(Fields, function(F)  {sys.$params(arguments.length, 1);  return Q("td")
            .style("text-align:center;")
            .html(F.name)
          ;})))
      .add(Q("tr")
        .adds(arr.map(Fields, function(F)  {sys.$params(arguments.length, 1);  return Q("td")
            .style("text-align:center;")
            .add(F.wg.style("width:" + F.width + "px"))
          ;})))
  ;};

  
   function mkCancelAccept(f)  {sys.$params(arguments.length, 1);
     return Q("tr")
      .add(Q("td")
        .style("text-align:right;")
        .add(ui.link(function(e)  {sys.$params(arguments.length, 1); ShowSelector[0]();})
          .klass("link")
          .text(II("Cancel")))
        .add(Q("span").html("&nbsp;&nbsp;&nbsp;"))
        .add(ui.link(function(e)  {sys.$params(arguments.length, 1); f();})
          .klass("link")
          .text(II("Accept"))))
  ;};

  
   function sell()  {sys.$params(arguments.length, 0);
    
     function v(field)  {sys.$params(arguments.length, 1);  return str.trim(field.getValue());};
    const dtf =sys.$checkNull( Q("input").att("type", "text"));
    const nkf =sys.$checkNull( Q("input").att("type", "text"));
    const stf =sys.$checkNull( Q("input").att("type", "text"));
    const prf =sys.$checkNull( Q("input").att("type", "text"));
    ui.changePoint(prf);
    
     function f()  {sys.$params(arguments.length, 0);
      const R =sys.$checkNull( [checkEmpty(II("Date"), v(dtf))]);
      if (sys.asBool(!sys.asBool(R[0]))) R[0] =sys.$checkExists(R[0],sys.$checkNull( checkEmpty(II("Nick"), v(nkf))));
      if (sys.asBool(!sys.asBool(R[0]))) R[0] =sys.$checkExists(R[0],sys.$checkNull( checkInt(II("Stocks"), v(stf))));
      if (sys.asBool(!sys.asBool(R[0]))) R[0] =sys.$checkExists(R[0],sys.$checkNull( checkFloat(II("Price"), v(prf))));
      if (sys.asBool(!sys.asBool(R[0])))
        sendAnn(ann.mk( -1, dt2s(v(dtf)), opr.mkSe(
          v(nkf), math.fromStr(v(stf))[0], math.fromIso(v(prf))[0]
        )));
      else
        ui.alert(R[0]);
    };

    editor
      .removeAll().
      add(Q("table")
        .klass("main")
        .add(mkTypeDate(II("Sell"), dtf))
        .add(mkFields([
          mkField(II("Nick"), nkf, 50),
          mkField(II("Stocks"), stf, 40),
          mkField(II("Price"), prf, 75)
        ]))
        .add(mkCancelAccept(f)));
  };

  
   function buy()  {sys.$params(arguments.length, 0);
    
     function v(field)  {sys.$params(arguments.length, 1);  return str.trim(field.getValue());};
    const dtf =sys.$checkNull( Q("input").att("type", "text"));
    const nkf =sys.$checkNull( Q("input").att("type", "text"));
    const stf =sys.$checkNull( Q("input").att("type", "text"));
    const prf =sys.$checkNull( Q("input").att("type", "text"));
    ui.changePoint(prf);
    
     function f()  {sys.$params(arguments.length, 0);
      const R =sys.$checkNull( [checkEmpty(II("Date"), v(dtf))]);
      if (sys.asBool(!sys.asBool(R[0]))) R[0] =sys.$checkExists(R[0],sys.$checkNull( checkEmpty(II("Nick"), v(nkf))));
      if (sys.asBool(!sys.asBool(R[0]))) R[0] =sys.$checkExists(R[0],sys.$checkNull( checkInt(II("Stocks"), v(stf))));
      if (sys.asBool(!sys.asBool(R[0]))) R[0] =sys.$checkExists(R[0],sys.$checkNull( checkFloat(II("Price"), v(prf))));
      if (sys.asBool(!sys.asBool(R[0])))
        sendAnn(ann.mk( -1, dt2s(v(dtf)), opr.mkBu(
          v(nkf), math.fromStr(v(stf))[0], math.fromIso(v(prf))[0]
        )));
      else
        ui.alert(R[0]);
    };

    editor
      .removeAll().
      add(Q("table")
        .klass("main")
        .add(mkTypeDate(II("Buy"), dtf))
        .add(mkFields([
          mkField(II("Nick"), nkf, 50),
          mkField(II("Stocks"), stf, 40),
          mkField(II("Price"), prf, 75)
        ]))
        .add(mkCancelAccept(f)));
  };

  
   function income()  {sys.$params(arguments.length, 0);
    
     function v(field)  {sys.$params(arguments.length, 1);  return str.trim(field.getValue());};
    const dtf =sys.$checkNull( Q("input").att("type", "text"));
    const amf =sys.$checkNull( Q("input").att("type", "text"));
    ui.changePoint(amf);
    
     function f()  {sys.$params(arguments.length, 0);
      const R =sys.$checkNull( [checkEmpty(II("Date"), v(dtf))]);
      if (sys.asBool(!sys.asBool(R[0]))) R[0] =sys.$checkExists(R[0],sys.$checkNull( checkFloat(II("Amount"), v(amf))));
      if (sys.asBool(!sys.asBool(R[0])))
        sendAnn(ann.mk( -1, dt2s(v(dtf)), opr.mkIn(
          math.fromIso(v(amf))[0]
        )));
      else
        ui.alert(R[0]);
    };

    editor
      .removeAll().
      add(Q("table")
        .klass("main")
        .add(mkTypeDate(II("Income"), dtf))
        .add(mkFields([
          mkField(II("Amount"), amf, 80)
        ]))
        .add(mkCancelAccept(f)));
  };

  
   function withdrawal()  {sys.$params(arguments.length, 0);
    
     function v(field)  {sys.$params(arguments.length, 1);  return str.trim(field.getValue());};
    const dtf =sys.$checkNull( Q("input").att("type", "text"));
    const amf =sys.$checkNull( Q("input").att("type", "text"));
    ui.changePoint(amf);
    
     function f()  {sys.$params(arguments.length, 0);
      const R =sys.$checkNull( [checkEmpty(II("Date"), v(dtf))]);
      if (sys.asBool(!sys.asBool(R[0]))) R[0] =sys.$checkExists(R[0],sys.$checkNull( checkFloat(II("Amount"), v(amf))));
      if (sys.asBool(!sys.asBool(R[0])))
        sendAnn(ann.mk( -1, dt2s(v(dtf)), opr.mkWi(
          math.fromIso(v(amf))[0]
        )));
      else
        ui.alert(R[0]);
    };

    editor
      .removeAll().
      add(Q("table")
        .klass("main")
        .add(mkTypeDate(II("Withdrawal"), dtf))
        .add(mkFields([
          mkField(II("Amount"), amf, 80)
        ]))
        .add(mkCancelAccept(f)));
  };

  
   function profits()  {sys.$params(arguments.length, 0);
    
     function v(field)  {sys.$params(arguments.length, 1);  return str.trim(field.getValue());};
    const dtf =sys.$checkNull( Q("input").att("type", "text"));
    const amf =sys.$checkNull( Q("input").att("type", "text"));
    ui.changePoint(amf);
    const def =sys.$checkNull( Q("input").att("type", "text"));
    
     function f()  {sys.$params(arguments.length, 0);
      const R =sys.$checkNull( [checkEmpty(II("Date"), v(dtf))]);
      if (sys.asBool(!sys.asBool(R[0]))) R[0] =sys.$checkExists(R[0],sys.$checkNull( checkFloat(II("Amount"), v(amf))));
      if (sys.asBool(!sys.asBool(R[0]))) R[0] =sys.$checkExists(R[0],sys.$checkNull( checkEmpty(II("Description"), v(def))));
      if (sys.asBool(!sys.asBool(R[0])))
        sendAnn(ann.mk( -1, dt2s(v(dtf)), opr.mkPr(
          math.fromIso(v(amf))[0], v(def)
        )));
      else
        ui.alert(R[0]);
    };

    editor
      .removeAll().
      add(Q("table")
        .klass("main")
        .add(mkTypeDate(II("Profits"), dtf))
        .add(mkFields([
          mkField(II("Amount"), amf, 80),
          mkField(II("Description"), def, 220)
        ]))
        .add(mkCancelAccept(f)));
  };

  
   function fees()  {sys.$params(arguments.length, 0);
    
     function v(field)  {sys.$params(arguments.length, 1);  return str.trim(field.getValue());};
    const dtf =sys.$checkNull( Q("input").att("type", "text"));
    const amf =sys.$checkNull( Q("input").att("type", "text"));
    ui.changePoint(amf);
    const def =sys.$checkNull( Q("input").att("type", "text"));
    
     function f()  {sys.$params(arguments.length, 0);
      const R =sys.$checkNull( [checkEmpty(II("Date"), v(dtf))]);
      if (sys.asBool(!sys.asBool(R[0]))) R[0] =sys.$checkExists(R[0],sys.$checkNull( checkFloat(II("Amount"), v(amf))));
      if (sys.asBool(!sys.asBool(R[0]))) R[0] =sys.$checkExists(R[0],sys.$checkNull( checkEmpty(II("Description"), v(def))));
      if (sys.asBool(!sys.asBool(R[0])))
        sendAnn(ann.mk( -1, dt2s(v(dtf)), opr.mkFe(
          math.fromIso(v(amf))[0], v(def)
        )));
      else
        ui.alert(R[0]);
    };

    editor
      .removeAll().
      add(Q("table")
        .klass("main")
        .add(mkTypeDate(II("Fees"), dtf))
        .add(mkFields([
          mkField(II("Amount"), amf, 80),
          mkField(II("Description"), def, 220)
        ]))
        .add(mkCancelAccept(f)));
  };

  
   function diffP()  {sys.$params(arguments.length, 0);
    
     function v(field)  {sys.$params(arguments.length, 1);  return str.trim(field.getValue());};
    const dtf =sys.$checkNull( Q("input").att("type", "text"));
    const amf =sys.$checkNull( Q("input").att("type", "text"));
    ui.changePoint(amf);
    const def =sys.$checkNull( Q("input").att("type", "text"));
    
     function f()  {sys.$params(arguments.length, 0);
      const R =sys.$checkNull( [checkEmpty(II("Date"), v(dtf))]);
      if (sys.asBool(!sys.asBool(R[0]))) R[0] =sys.$checkExists(R[0],sys.$checkNull( checkFloat(II("Amount"), v(amf))));
      if (sys.asBool(!sys.asBool(R[0]))) R[0] =sys.$checkExists(R[0],sys.$checkNull( checkEmpty(II("Diff. +"), v(def))));
      if (sys.asBool(!sys.asBool(R[0])))
        sendAnn(ann.mk( -1, dt2s(v(dtf)), opr.mkPd(
          math.fromIso(v(amf))[0], v(def)
        )));
      else
        ui.alert(R[0]);
    };

    editor
      .removeAll().
      add(Q("table")
        .klass("main")
        .add(mkTypeDate(II("Fees"), dtf))
        .add(mkFields([
          mkField(II("Amount"), amf, 80),
          mkField(II("Diff. +"), def, 220)
        ]))
        .add(mkCancelAccept(f)));
  };

  
   function diffN()  {sys.$params(arguments.length, 0);
    
     function v(field)  {sys.$params(arguments.length, 1);  return str.trim(field.getValue());};
    const dtf =sys.$checkNull( Q("input").att("type", "text"));
    const amf =sys.$checkNull( Q("input").att("type", "text"));
    ui.changePoint(amf);
    const def =sys.$checkNull( Q("input").att("type", "text"));
    
     function f()  {sys.$params(arguments.length, 0);
      const R =sys.$checkNull( [checkEmpty(II("Date"), v(dtf))]);
      if (sys.asBool(!sys.asBool(R[0]))) R[0] =sys.$checkExists(R[0],sys.$checkNull( checkFloat(II("Amount"), v(amf))));
      if (sys.asBool(!sys.asBool(R[0]))) R[0] =sys.$checkExists(R[0],sys.$checkNull( checkEmpty(II("Description"), v(def))));
      if (sys.asBool(!sys.asBool(R[0])))
        sendAnn(ann.mk( -1, dt2s(v(dtf)), opr.mkNd(
          math.fromIso(v(amf))[0], v(def)
        )));
      else
        ui.alert(R[0]);
    };

    editor
      .removeAll().
      add(Q("table")
        .klass("main")
        .add(mkTypeDate(II("Diff. -"), dtf))
        .add(mkFields([
          mkField(II("Amount"), amf, 80),
          mkField(II("Description"), def, 220)
        ]))
        .add(mkCancelAccept(f)));
  };

  
  ShowSelector[0] =sys.$checkExists(ShowSelector[0], function()  {sys.$params(arguments.length, 0);
    
     function td()  {sys.$params(arguments.length, 0);  return Q("td").style("width:50%;");};
    
     function lk(tx, f)  {sys.$params(arguments.length, 2);  return ui.link(f).klass("link").html(tx);};
    editor
      .removeAll()
      .add(Q("table").klass("main")
        .add(Q("tr")
          .add(td().add(lk(II("Sell"), function(e)  {sys.$params(arguments.length, 1); sell();})))
          .add(td().add(lk(II("Buy"), function(e)  {sys.$params(arguments.length, 1); buy();}))))
        .add(Q("tr")
          .add(td().add(lk(II("Income"), function(e)  {sys.$params(arguments.length, 1); income();})))
          .add(td().add(lk(II("Withdrawal"), function(e)  {sys.$params(arguments.length, 1); withdrawal();}))))
        .add(Q("tr")
          .add(td().add(lk(II("Profits"), function(e)  {sys.$params(arguments.length, 1); profits();})))
          .add(td().add(lk(II("Fees"), function(e)  {sys.$params(arguments.length, 1); fees();}))))
        .add(Q("tr")
          .add(td().add(lk(II("Diff. +"), function(e)  {sys.$params(arguments.length, 1); diffP();})))
          .add(td().add(lk(II("Diff. -"), function(e)  {sys.$params(arguments.length, 1); diffN();})))))
    ;
  });

  const Lopts =sys.$checkNull( []);
  arr.eachIx(Years, function(y, i)  {sys.$params(arguments.length, 2);
    if (sys.asBool(i > 0)) arr.push(Lopts, menu.separator());
    arr.push(Lopts, menu.toption(y, y, function()  {sys.$params(arguments.length, 0); changeYear(y);}));
  });
  const menuWg =sys.$checkNull( menu.mk(Lopts, [], year, false));

  const annsWg =sys.$checkNull( Q("div"));
  if (sys.asBool(sys.$eq(year , Years[0]))) annotationsWg.mk(annsWg, Anns, [del]);
  else annotationsWg.mk(annsWg, Anns, []);

  ShowSelector[0]();

  wg
    .removeAll()
    .add(menuWg)
    .add(Q("div")
      .add(Q("div")
        .klass("head")
        .html(II("Annotations")))
      .add(Q("table")
        .att("align", "center")
        .klass("frame3")
          .add(Q("tr")
            .add(Q("td")
              .add(editor)))
          .add(Q("tr")
            .add(Q("td")
              .add(Q("hr"))))
        .add(Q("tr")
          .add(Q("td")
            .add(Q("table")
              .att("align", "right")
              .add(Q("tr")
                .add(Q("td")
                  .klass("rlabel")
                  .add(Q("span")
                    .html(II("Cash:"))))
                .add(Q("td")
                  .klass("number")
                  .text(math.toIso(cash, 2)))
                .add(Q("td"))))))
        .add(Q("tr")
          .add(Q("td").klass("frame")
            .add(annsWg)))))
  ;
};


export  function mk(wg, inv)  {sys.$params(arguments.length, 2); mk2(wg, inv, "");};
