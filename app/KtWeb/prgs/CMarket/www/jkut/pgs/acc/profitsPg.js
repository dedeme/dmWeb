import * as iter from '../../_js/iter.js';import * as str from '../../_js/str.js';import * as bytes from '../../_js/bytes.js';import * as cryp from '../../_js/cryp.js';import * as dic from '../../_js/dic.js';import * as timer from '../../_js/timer.js';import * as js from '../../_js/js.js';import * as storage from '../../_js/storage.js';import * as sys from '../../_js/sys.js';import * as math from '../../_js/math.js';import * as domo from '../../_js/domo.js';import * as ui from '../../_js/ui.js';import * as arr from '../../_js/arr.js';import * as time from '../../_js/time.js';import * as client from '../../_js/client.js';import * as b64 from '../../_js/b64.js';




import * as menu from  "../../libdm/menu.js";
import * as lineChart from  "../../libdm/lineChart.js";
import * as cts from  "../../data/cts.js";
import * as profitsEntry from  "../../data/chart/profitsEntry.js";
import * as i18n from  "../../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


 function mkBigTd()  {sys.$params(arguments.length, 0);  return Q("td").style("font-size:large;text-align:right");};


 function dfFmt(n)  {sys.$params(arguments.length, 1);
  const color =sys.$checkNull(sys.asBool( n < 0) ? "aa2800" :sys.asBool( n > 0) ? "0041aa" : "000000");
   return "<font color='" + color + "'>" + math.toIso(n, 2) + "</font>";
};


 function mkTr(color, val)  {sys.$params(arguments.length, 2);  return Q("tr")
    .add(Q("td")
      .style("vertical-align:middle;width:5px")
      .add(ui.led(color, 6)))
    .add(Q("td")
      .style(
          "width: 80px;text-align:right;color:" +
          (sys.asBool( val > 0)
              ? "rgba(0, 129, 255)"
              :sys.asBool( val < 0)
                ? "rgba(255, 40, 0)"
                : "rgba(0, 0, 0)"
          )
        )
      .text(math.toIso(val, 2)))
  ;};


 function mkIncrement(tt, acc, rk)  {sys.$params(arguments.length, 3);  return Q("table")
    .klass("frame")
    .style("border-collapse : collapse;")
    .add(mkTr("rgba(0, 129, 255)", tt))
    .add(mkTr("rgba(0, 0, 0)", acc))
    .add(mkTr("rgba(255, 40, 0)", rk))
  ;};



export  function mk2(wg, allEs, invEs, mSel)  {sys.$params(arguments.length, 4);
  const Lops =sys.$checkNull( [
    menu.toption("All", II("All"), function()  {sys.$params(arguments.length, 0); mk2(wg, allEs, invEs, "All");})
  ]);
  for (let i = 0;i < arr.size(invEs); ++i) {
    arr.push(Lops, menu.separator());
    arr.push(
      Lops,
      menu.toption("" + i, II("Inv") + "-" + i, function()  {sys.$params(arguments.length, 0); mk2(wg, allEs, invEs, "" + i);})
    );
  }

  const Es =sys.$checkNull(  
    sys.$eq(mSel,"All")? allEs:
     invEs[math.fromStr(mSel)[0]]
  );
  const CEs =sys.$checkNull( []); 
  const Corrects =sys.$checkNull( []); 
  const CorrectsSum =sys.$checkNull( [0]);
  const I =sys.$checkNull( [0]);
  for (let E  of sys.$forObject( Es)) {
    const i =sys.$checkNull( I[0]);
    I[0] +=sys.$checkExists(I[0],sys.$checkNull( 1));
    if (sys.asBool(sys.$eq(i , 0))) {
      arr.push(CEs, E);
      continue;
    }
    const E1 =sys.$checkNull( Es[i - 1]);
    if (sys.asBool(sys.$neq(sys.$slice(E.date,null,4) , sys.$slice(E1.date,null,4)))) {
      CorrectsSum[0] +=sys.$checkExists(CorrectsSum[0],sys.$checkNull( E1.acc));
      arr.push(Corrects, E1.acc);
    }
    const sum =sys.$checkNull( CorrectsSum[0]);
    arr.push(CEs, profitsEntry.mk(
      E.date, E.total + sum, E.acc + sum, E.risk + sum
    ));
  }

  const e =sys.$checkNull( arr.peek(Es));
  const now =sys.$checkNull( time.fromStr(e.date));
  const today =sys.$checkNull( time.toStr(now));
  const e1 =sys.$checkNull( Es[arr.size(Es) - 2]);
  const summary =sys.$checkNull( Q("table")
    .att("align", "center")
    .klass("frame")
    .add(Q("tr")
      .add(Q("td")
        .att("rowspan", 2)
        .style("font-size:large;vertical-align:top")
        .text(time.toIso(now) + " :"))
      .add(mkBigTd()
        .text(" [ "))
      .add(mkBigTd()
        .html("<font color='0041aa'>" + math.toIso(e.total, 2) + "</font>"))
      .add(mkBigTd()
        .text(" | "))
      .add(mkBigTd()
        .html("<font color='000000'>" + math.toIso(e.acc, 2) + "</font>"))
      .add(mkBigTd()
        .text(" | "))
      .add(mkBigTd()
        .html("<font color='aa2800'>" + math.toIso(e.risk, 2) + "</font>"))
      .add(mkBigTd()
        .text(" | "))
      .add(mkBigTd()
        .html(
          "<font color='00aa41'>" +
          math.toIso(e.total - e.risk, 2) +
          "</font>"))
      .add(mkBigTd()
        .text(" ]")))
    .add(Q("tr")
      .add(mkBigTd()
        .text(" [ "))
      .add(mkBigTd()
        .html(dfFmt(e.total - e1.total)))
      .add(mkBigTd()
        .text(" | "))
      .add(mkBigTd()
        .html(dfFmt(e.acc - e1.acc)))
      .add(mkBigTd()
        .text(" | "))
      .add(mkBigTd()
        .html(dfFmt(e.risk - e1.risk)))
      .add(mkBigTd()
        .text(" | "))
      .add(mkBigTd()
        .html(dfFmt(e.total + e1.risk - e1.total - e.risk)))
      .add(mkBigTd()
        .text(" ]"))))
    ;

  const Chart =sys.$checkNull( lineChart.mkExample());
  Chart.ExArea.width =sys.$checkExists(Chart.ExArea.width,sys.$checkNull( 600));
  Chart.ExArea.height =sys.$checkExists(Chart.ExArea.height,sys.$checkNull( 200));
  Chart.ExArea.Atts.Border.width =sys.$checkExists(Chart.ExArea.Atts.Border.width,sys.$checkNull( 0));
  Chart.InPadding.top =sys.$checkExists(Chart.InPadding.top,sys.$checkNull( 10));
  Chart.InPadding.right =sys.$checkExists(Chart.InPadding.right,sys.$checkNull( 10));
  Chart.InPadding.bottom =sys.$checkExists(Chart.InPadding.bottom,sys.$checkNull( 20));
  Chart.InPadding.left =sys.$checkExists(Chart.InPadding.left,sys.$checkNull( 85));
  Chart.ChartPadding.top =sys.$checkExists(Chart.ChartPadding.top,sys.$checkNull( 4));
  Chart.ChartPadding.right =sys.$checkExists(Chart.ChartPadding.right,sys.$checkNull( 2));
  Chart.ChartPadding.bottom =sys.$checkExists(Chart.ChartPadding.bottom,sys.$checkNull( 4));
  Chart.ChartPadding.left =sys.$checkExists(Chart.ChartPadding.left,sys.$checkNull( 2));

  

  const month =sys.$checkNull( sys.$slice(today,null,6));
  const E00s =sys.$checkNull( arr.dropWhile(Es, function(E)  {sys.$params(arguments.length, 1);  return sys.$slice(E.date,null,6) <  month;}));
  const E0s =sys.$checkNull(sys.asBool( arr.size(E00s) > 1) ? E00s : sys.$slice(Es, -2,null));
  const Ef0 =sys.$checkNull( E0s[0]);
  const El0 =sys.$checkNull( arr.peek(E0s));
  const total0 =sys.$checkNull( El0.total - Ef0.total);
  const acc0 =sys.$checkNull( El0.acc - Ef0.acc);
  const risk0 =sys.$checkNull( El0.risk - Ef0.risk);

  const back0 =sys.$checkNull(sys.asBool( total0 > 0)
    ? "#e9e9f2"
    :sys.asBool( total0 < 0)
      ? "#f2e9e9"
      : "#e9e9e9")
    ;
  Chart.ExArea.Atts.background =sys.$checkExists(Chart.ExArea.Atts.background,sys.$checkNull( back0));

  const Lb0s =sys.$checkNull( []);
  const Val0s =sys.$checkNull( [[], [], []]);
  for (let E  of sys.$forObject( E0s)) {
    arr.push(Lb0s, time.toIso(time.fromStr(E.date)));
    arr.push(Val0s[0], [E.total]);
    arr.push(Val0s[1], [E.acc]);
    arr.push(Val0s[2], [E.risk]);
  }
  const Att0s =sys.$checkNull( [
    lineChart.mkLine(2, "#0041aa", false),
    lineChart.mkLine(2, "#414141", false),
    lineChart.mkLine(2, "#aa4100", false)
  ]);
  const Data0 =sys.$checkNull( lineChart.mkData(Lb0s, Val0s, Att0s));
  Data0.round =sys.$checkExists(Data0.round,sys.$checkNull( 0));
  Data0.Labels =sys.$checkExists(Data0.Labels,sys.$checkNull( Lb0s));
  Data0.SetValues =sys.$checkExists(Data0.SetValues,sys.$checkNull( [Val0s[0], Val0s[1], Val0s[2]]));
  Data0.SetAtts =sys.$checkExists(Data0.SetAtts,sys.$checkNull( Att0s));
  Data0.drawGrid =sys.$checkExists(Data0.drawGrid, function(lb, i)  {sys.$params(arguments.length, 2);
    if (sys.asBool(sys.$eq(i , 0)))  return false;
     return sys.$eq(i % 3 , 0);
  });
  Data0.drawLabel =sys.$checkExists(Data0.drawLabel,sys.$checkNull( Data0.drawGrid));
  Data0.mapLabel =sys.$checkExists(Data0.mapLabel, function(lb, i)  {sys.$params(arguments.length, 2);  return sys.$slice(lb,null,2);});


  const lastMonth =sys.$checkNull( Q("table")
    .att("align", "center")
    .add(Q("tr")
      .add(Q("td")
        .klass("frame0")
        .style("background-color:" + back0)
        .add(lineChart.mkWg(Chart, Data0)))
      .add(Q("td")
        .add(mkIncrement(total0, acc0, risk0)))))
  ;

  

  const year =sys.$checkNull( sys.$slice(today,null,4));
  const E10s =sys.$checkNull( arr.dropWhile(Es, function(E)  {sys.$params(arguments.length, 1);  return sys.$slice(E.date,null,4) <  year;}));
  const E1s =sys.$checkNull(sys.asBool( arr.size(E10s) > 1) ? E10s : sys.$slice(Es, -2,null));
  const Ef1 =sys.$checkNull( E1s[0]);
  const El1 =sys.$checkNull( arr.peek(E1s));
  const total1 =sys.$checkNull( El1.total - Ef1.total);
  const acc1 =sys.$checkNull( El1.acc - Ef1.acc);
  const risk1 =sys.$checkNull( El1.risk - Ef1.risk);

  Chart.ChartPadding.top =sys.$checkExists(Chart.ChartPadding.top,sys.$checkNull( 2));
  Chart.ChartPadding.bottom =sys.$checkExists(Chart.ChartPadding.bottom,sys.$checkNull( 2));
  const back1 =sys.$checkNull(sys.asBool( total1 > 0)
    ? "#e9e9f2"
    :sys.asBool( total1< 0)
      ? "#f2e9e9"
      : "#e9e9e9")
    ;
  Chart.ExArea.Atts.background =sys.$checkExists(Chart.ExArea.Atts.background,sys.$checkNull( back1));

  const Lb1s =sys.$checkNull( []);
  const Val1s =sys.$checkNull( [[], [], []]);
  for (let E  of sys.$forObject( E1s)) {
    arr.push(Lb1s, time.toIso(time.fromStr(E.date)));
    arr.push(Val1s[0], [E.total]);
    arr.push(Val1s[1], [E.acc]);
    arr.push(Val1s[2], [E.risk]);
  }
  const Att1s =sys.$checkNull( [
    lineChart.mkLine(1.5, "#0041aa", false),
    lineChart.mkLine(1.5, "#414141", false),
    lineChart.mkLine(1.5, "#aa4100", false)
  ]);
  const Data1 =sys.$checkNull( lineChart.mkData(Lb1s, Val1s, Att1s));
  Data1.round =sys.$checkExists(Data1.round,sys.$checkNull( 0));
  Data1.Labels =sys.$checkExists(Data1.Labels,sys.$checkNull( Lb1s));
  Data1.SetValues =sys.$checkExists(Data1.SetValues,sys.$checkNull( [Val1s[0], Val1s[1], Val1s[2]]));
  Data1.SetAtts =sys.$checkExists(Data1.SetAtts,sys.$checkNull( Att1s));
  Data1.drawGrid =sys.$checkExists(Data1.drawGrid, function(lb, i)  {sys.$params(arguments.length, 2);
    if (sys.asBool(sys.$eq(i , 0)))  return false;
    return sys.asBool( sys.$neq(sys.$slice(Lb1s[i - 1],3,5) , sys.$slice(lb,3,5))) ? true : false;
  });
  Data1.drawLabel =sys.$checkExists(Data1.drawLabel,sys.$checkNull( Data1.drawGrid));
  Data1.mapLabel =sys.$checkExists(Data1.mapLabel, function(lb, i)  {sys.$params(arguments.length, 2);  return sys.$slice(lb,3,5);});

  const currentYear =sys.$checkNull( Q("table")
    .att("align", "center")
    .add(Q("tr")
      .add(Q("td")
        .klass("frame0")
        .style("background-color:" + back1)
        .add(lineChart.mkWg(Chart, Data1)))
      .add(Q("td")
        .add(mkIncrement(total1, acc1, risk1)))))
  ;

  

  const year2 =sys.$checkNull( time.toStr(time.addDays(now,  -365)));
  const E20s =sys.$checkNull( arr.dropWhile(CEs, function(E)  {sys.$params(arguments.length, 1);  return E.date < year2;}));
  const E2s =sys.$checkNull(sys.asBool( arr.size(E20s) > 1) ? E20s : sys.$slice(Es, -2,null));
  const Ef2 =sys.$checkNull( E2s[0]);
  const El2 =sys.$checkNull( arr.peek(E2s));
  const total2 =sys.$checkNull( El2.total - Ef2.total);
  const acc2 =sys.$checkNull( El2.acc - Ef2.acc);
  const risk2 =sys.$checkNull( El2.risk - Ef2.risk);

  const back2 =sys.$checkNull(sys.asBool( total2 > 0)
    ? "#e9e9f2"
    :sys.asBool( total2< 0)
      ? "#f2e9e9"
      : "#e9e9e9")
    ;
  Chart.ExArea.Atts.background =sys.$checkExists(Chart.ExArea.Atts.background,sys.$checkNull( back2));

  const Lb2s =sys.$checkNull( []);
  const Val2s =sys.$checkNull( [[], [], []]);
  for (let E  of sys.$forObject( E2s)) {
    arr.push(Lb2s, time.toIso(time.fromStr(E.date)));
    arr.push(Val2s[0], [E.total]);
    arr.push(Val2s[1], [E.acc]);
    arr.push(Val2s[2], [E.risk]);
  }
  const Att2s =sys.$checkNull( [
    lineChart.mkLine(1.5, "#0041aa", false),
    lineChart.mkLine(1.5, "#414141", false),
    lineChart.mkLine(1.5, "#aa4100", false)
  ]);
  const Data2 =sys.$checkNull( lineChart.mkData(Lb2s, Val2s, Att2s));
  Data2.round =sys.$checkExists(Data2.round,sys.$checkNull( 0));
  Data2.Labels =sys.$checkExists(Data2.Labels,sys.$checkNull( Lb2s));
  Data2.SetValues =sys.$checkExists(Data2.SetValues,sys.$checkNull( [Val2s[0], Val2s[1], Val2s[2]]));
  Data2.SetAtts =sys.$checkExists(Data2.SetAtts,sys.$checkNull( Att2s));
  Data2.drawGrid =sys.$checkExists(Data2.drawGrid, function(lb, i)  {sys.$params(arguments.length, 2);
    if (sys.asBool(sys.$eq(i , 0)))  return false;
    return sys.asBool( sys.$neq(sys.$slice(Lb2s[i - 1],3,5) , sys.$slice(lb,3,5))) ? true : false;
  });
  Data2.drawLabel =sys.$checkExists(Data2.drawLabel,sys.$checkNull( Data2.drawGrid));
  Data2.mapLabel =sys.$checkExists(Data2.mapLabel, function(lb, i)  {sys.$params(arguments.length, 2);  return sys.$slice(lb,3,5);});

  const lastYear =sys.$checkNull( Q("table")
    .att("align", "center")
    .add(Q("tr")
      .add(Q("td")
        .klass("frame0")
        .style("background-color:" + back2)
        .add(lineChart.mkWg(Chart, Data2)))
      .add(Q("td")
        .add(mkIncrement(total2, acc2, risk2)))))
  ;

  

  const PrevE =sys.$checkNull( [CEs[0]]);
  const E30s =sys.$checkNull( arr.filter(CEs, function(E)  {sys.$params(arguments.length, 1);
    const r =sys.$checkNull( sys.$neq(sys.$slice(E.date,4,6) , sys.$slice(PrevE[0].date,4,6)));
    PrevE[0] =sys.$checkExists(PrevE[0],sys.$checkNull( E));
     return r;
  }));
  arr.unshift(E30s, CEs[0]);
  if (sys.asBool(sys.$neq(arr.peek(E30s).date , arr.peek(CEs).date)))
    arr.push(E30s, arr.peek(CEs));
  const E3s =sys.$checkNull(sys.asBool( arr.size(E30s) > 1) ? E30s : sys.$slice(Es, -2,null));
  const Ef3 =sys.$checkNull( E3s[0]);
  const El3 =sys.$checkNull( arr.peek(E3s));
  const total3 =sys.$checkNull( El3.total - Ef3.total);
  const acc3 =sys.$checkNull( El3.acc - Ef3.acc);
  const risk3 =sys.$checkNull( El3.risk - Ef3.risk);

  const back3 =sys.$checkNull(sys.asBool( total3 > 0)
    ? "#e9e9f2"
    :sys.asBool( total3< 0)
      ? "#f2e9e9"
      : "#e9e9e9")
    ;
  Chart.ExArea.Atts.background =sys.$checkExists(Chart.ExArea.Atts.background,sys.$checkNull( back3));

  const Lb3s =sys.$checkNull( []);
  const Val3s =sys.$checkNull( [[], [], []]);
  for (let E  of sys.$forObject( E3s)) {
    arr.push(Lb3s, sys.$slice(time.toIso(time.fromStr(E.date)),3,null));
    arr.push(Val3s[0], [E.total]);
    arr.push(Val3s[1], [E.acc]);
    arr.push(Val3s[2], [E.risk]);
  }
  const Att3s =sys.$checkNull( [
    lineChart.mkLine(1.5, "#0041aa", false),
    lineChart.mkLine(1.5, "#414141", false),
    lineChart.mkLine(1.5, "#aa4100", false)
  ]);
  const Data3 =sys.$checkNull( lineChart.mkData(Lb3s, Val3s, Att3s));
  Data3.round =sys.$checkExists(Data3.round,sys.$checkNull( 0));
  Data3.Labels =sys.$checkExists(Data3.Labels,sys.$checkNull( Lb3s));
  Data3.SetValues =sys.$checkExists(Data3.SetValues,sys.$checkNull( [Val3s[0], Val3s[1], Val3s[2]]));
  Data3.SetAtts =sys.$checkExists(Data3.SetAtts,sys.$checkNull( Att3s));
  Data3.drawGrid =sys.$checkExists(Data3.drawGrid, function(lb, i)  {sys.$params(arguments.length, 2);
    if (sys.asBool(sys.$eq(i , 0)))  return false;
    return sys.asBool( sys.$neq(sys.$slice(Lb3s[i - 1], -2,null) , sys.$slice(lb, -2,null))) ? true : false;
  });
  Data3.drawLabel =sys.$checkExists(Data3.drawLabel,sys.$checkNull( Data3.drawGrid));
  Data3.mapLabel =sys.$checkExists(Data3.mapLabel, function(lb, i)  {sys.$params(arguments.length, 2);  return sys.$slice(lb, -2,null);});

  const all =sys.$checkNull( Q("table")
    .att("align", "center")
    .add(Q("tr")
      .add(Q("td")
        .klass("frame0")
        .style("background-color:" + back3)
        .add(lineChart.mkWg(Chart, Data3)))
      .add(Q("td")
        .add(mkIncrement(total3, acc3, risk3)))))
  ;

  wg
    .removeAll()
    .add(menu.mk(Lops, [], mSel, false))
    .add(summary)
    .add(Q("div").klass("head").html(II("Last Month")))
    .add(lastMonth)
    .add(Q("div").klass("head").html(II("Current Year")))
    .add(currentYear)
    .add(Q("div").klass("head").html(II("Last Year")))
    .add(lastYear)
    .add(Q("div").klass("head").html(II("All")))
    .add(all)
  ;
};



export  async  function mk(wg)  {sys.$params(arguments.length, 1);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    module: "Acc",
    source: "ProfitsPg",
    rq: "idata"
  }));
  const allEntries =sys.$checkNull( arr.map(Rp.allEntries, profitsEntry.fromJs));
  const invEntries =sys.$checkNull( arr.map(
    Rp.invEntries,
    function(Es)  {sys.$params(arguments.length, 1);  return arr.map(Es, profitsEntry.fromJs);}
  ));
  mk2(wg, allEntries, invEntries, "All");
};
