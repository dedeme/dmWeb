import * as iter from '../../_js/iter.js';import * as str from '../../_js/str.js';import * as bytes from '../../_js/bytes.js';import * as cryp from '../../_js/cryp.js';import * as dic from '../../_js/dic.js';import * as timer from '../../_js/timer.js';import * as js from '../../_js/js.js';import * as storage from '../../_js/storage.js';import * as sys from '../../_js/sys.js';import * as math from '../../_js/math.js';import * as domo from '../../_js/domo.js';import * as ui from '../../_js/ui.js';import * as arr from '../../_js/arr.js';import * as time from '../../_js/time.js';import * as client from '../../_js/client.js';import * as b64 from '../../_js/b64.js';




import * as menu from  "../../libdm/menu.js";
import * as lineChart from  "../../libdm/lineChart.js";
import * as cts from  "../../data/cts.js";
import * as profitsEntry from  "../../data/chart/profitsEntry.js";
import * as msg from  "../../wgs/msg.js";
import * as i18n from  "../../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);



export  async  function mk(wg)  {sys.$params(arguments.length, 1);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    module: "Acc",
    source: "CompaniesPg",
    rq: "idata"
  }));
  const List =sys.$checkNull( arr.map(
    Rp.list,
    function(E)  {sys.$params(arguments.length, 1);  return {nick: E[0], bought: E[1], url: E[2]};}
  ));

  const ShowAll =sys.$checkNull( [false]);
  const Show =sys.$checkNull( [[]]);

  

   function changeShowAll()  {sys.$params(arguments.length, 0);
    ShowAll[0] =sys.$checkExists(ShowAll[0],sys.$checkNull( !sys.asBool(ShowAll[0])));
    Show[0]();
  };

  

  
   function separator()  {sys.$params(arguments.length, 0);  return Q("tr")
    .add(Q("td")
      .att("colspan", 3)
      .html("<hr>"))
  ;};

  
   function backColor(prevQ, Refs, nextQ)  {sys.$params(arguments.length, 3);
    return sys.asBool( arr.any(Refs, function(ref)  {sys.$params(arguments.length, 1);  return sys.asBool(prevQ < ref) && sys.asBool(ref < nextQ);}))
      ? "#fff0f0"
      :sys.asBool( arr.any(Refs, function(ref)  {sys.$params(arguments.length, 1);  return sys.asBool(prevQ > ref) && sys.asBool(ref > nextQ);}))
        ? "#f0f0ff"
        : "#c9c9c9"
    ;};

   function leds(invsN, Invs)  {sys.$params(arguments.length, 2);
     return Q("table")
      .style("border-collapse : collapse;")
      .add(Q("tr")
        .adds(arr.fromIter(iter.map(iter.$range(0,invsN), function(inv)  {sys.$params(arguments.length, 1);
           return Q("td")
            .add(Q("div")
              .style("padding:5px;" +
               "border: 1px solid #002040;border-radius: 6px;" +
               "cursor:pointer;" +
               "background: " +
               (sys.asBool(arr.any(Invs, function(i)  {sys.$params(arguments.length, 1);  return sys.$eq(i , inv);}))
                  ? cts.toBuyColors[inv]
                  : "#e9e9e9"
                ) +
               ";"));}))))
  ;};

  
   async  function mkChart(wg, nick, url)  {sys.$params(arguments.length, 3);
    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center")
            .add(Q("img")
              .att("src", "img/wait2.gif")
              .klass("frame")))))
    ;

    const Rp =sys.$checkNull( await  client.send({
      prg: cts.appName,
      module: "Acc",
      source: "CompaniesPg",
      rq: "nickData",
      nick:nick
    }));

    
    const error =sys.$checkNull( Rp.error);
    if (sys.asBool(error)) {
      wg.removeAll()
        .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .klass("frame")
            .style("width:300px")
            .text(error))))
      ;
      return;
    }

    
    const invsN =sys.$checkNull( Rp.investorsN);
    
    const price =sys.$checkNull( Rp.price);
    
    const Invs =sys.$checkNull( Rp.investors); 
    
    const profits =sys.$checkNull( Rp.profits);
    
    const Dates =sys.$checkNull( Rp.dates); 
    
    const Closes =sys.$checkNull( Rp.closes); 
    
    const Refs =sys.$checkNull( Rp.refs); 

    const back =sys.$checkNull( backColor(
      sys.$slice(Closes, -2,null)[0],
      arr.map(Refs, function(Rs)  {sys.$params(arguments.length, 1);  return sys.$slice(Rs, -2,null)[0];}),
      arr.peek(Closes)
    ));

    
     function mkSmallGr()  {sys.$params(arguments.length, 0);
      const Ch =sys.$checkNull( lineChart.mkExample());
      Ch.ExArea.width =sys.$checkExists(Ch.ExArea.width,sys.$checkNull( 300));
      Ch.ExArea.height =sys.$checkExists(Ch.ExArea.height,sys.$checkNull( 150));
      Ch.ExArea.Atts.background =sys.$checkExists(Ch.ExArea.Atts.background,sys.$checkNull( back));
      Ch.ExArea.Atts.Border.width =sys.$checkExists(Ch.ExArea.Atts.Border.width,sys.$checkNull( 0));
      Ch.InPadding.top =sys.$checkExists(Ch.InPadding.top,sys.$checkNull( 5));
      Ch.InPadding.right =sys.$checkExists(Ch.InPadding.right,sys.$checkNull( 4));
      Ch.InPadding.bottom =sys.$checkExists(Ch.InPadding.bottom,sys.$checkNull( 4));
      Ch.InPadding.left =sys.$checkExists(Ch.InPadding.left,sys.$checkNull( 50));
      Ch.ChartPadding.top =sys.$checkExists(Ch.ChartPadding.top,sys.$checkNull( 2));
      Ch.ChartPadding.right =sys.$checkExists(Ch.ChartPadding.right,sys.$checkNull( 2));
      Ch.ChartPadding.bottom =sys.$checkExists(Ch.ChartPadding.bottom,sys.$checkNull( 2));
      Ch.ChartPadding.left =sys.$checkExists(Ch.ChartPadding.left,sys.$checkNull( 2));
      Ch.Labels.show =sys.$checkExists(Ch.Labels.show,sys.$checkNull( false));

      const Lbs =sys.$checkNull( []);
      const Vals =sys.$checkNull( [[], [], []]); 
      const Dates2 =sys.$checkNull( sys.$slice(Dates, -250,null));
      const Closes2 =sys.$checkNull( sys.$slice(Closes, -250,null));
      const Refs2 =sys.$checkNull( sys.$slice(Refs[0], -250,null));
      for (let i = 0;i < arr.size(Dates2); ++i) {
        const cl =sys.$checkNull( Closes2[i]);
        const rf =sys.$checkNull( Refs2[i]);
        arr.push(Lbs, time.toIso(time.fromStr(Dates2[i])));
        arr.push(Vals[0], [cl]);
        arr.push(Vals[1],sys.asBool( cl > rf) ? [rf] : []);
        arr.push(Vals[2],sys.asBool( cl < rf) ? [rf] : []);
      }
      const Atts =sys.$checkNull( [
        lineChart.mkLine(1.2, "#414141", false),
        lineChart.mkLine(1.2, "#aa4100", false),
        lineChart.mkLine(1.2, "#0041aa", false)
      ]);
      const Data =sys.$checkNull( lineChart.mkData(Lbs, Vals, Atts));
      Data.round =sys.$checkExists(Data.round,sys.$checkNull( 2));
      Data.Labels =sys.$checkExists(Data.Labels,sys.$checkNull( Lbs));
      Data.SetValues =sys.$checkExists(Data.SetValues,sys.$checkNull( [Vals[0], Vals[1], Vals[2]]));
      if (sys.asBool(price >= 0))
        Data.UnarySets =sys.$checkExists(Data.UnarySets,sys.$checkNull( [lineChart.mkUnarySet(
            II("Price"),
            price,
            lineChart.mkLine(1.2, "#c9c9c9", false)
          )]));
      Data.SetAtts =sys.$checkExists(Data.SetAtts,sys.$checkNull( Atts));
      Data.maxMinRound =sys.$checkExists(Data.maxMinRound, function(mx, mn)  {sys.$params(arguments.length, 2); return sys.asBool( mx > 10) ? 0 :  -1;});
      Data.drawGrid =sys.$checkExists(Data.drawGrid, function(lb, i)  {sys.$params(arguments.length, 2);  return false;});
      Data.drawLabel =sys.$checkExists(Data.drawLabel,sys.$checkNull( Data.drawGrid));

       return Q("table")
        .add(Q("tr")
          .add(Q("td")
            .klass("frame0")
            .style("background-color:" + back)
            .add(lineChart.mkWg(Ch, Data))))
      ;
    };

    
     function mkBigGr()  {sys.$params(arguments.length, 0);
      const Ch =sys.$checkNull( lineChart.mkExample());
      Ch.ExArea.width =sys.$checkExists(Ch.ExArea.width,sys.$checkNull( 600));
      Ch.ExArea.height =sys.$checkExists(Ch.ExArea.height,sys.$checkNull( 300));
      Ch.ExArea.Atts.background =sys.$checkExists(Ch.ExArea.Atts.background,sys.$checkNull( back));
      Ch.ExArea.Atts.Border.width =sys.$checkExists(Ch.ExArea.Atts.Border.width,sys.$checkNull( 0));
      Ch.ExArea.Atts.background =sys.$checkExists(Ch.ExArea.Atts.background,sys.$checkNull( back));
      Ch.InPadding.top =sys.$checkExists(Ch.InPadding.top,sys.$checkNull( 10));
      Ch.InPadding.right =sys.$checkExists(Ch.InPadding.right,sys.$checkNull( 5));
      Ch.InPadding.bottom =sys.$checkExists(Ch.InPadding.bottom,sys.$checkNull( 20));
      Ch.InPadding.left =sys.$checkExists(Ch.InPadding.left,sys.$checkNull( 80));
      Ch.ChartPadding.top =sys.$checkExists(Ch.ChartPadding.top,sys.$checkNull( 2));
      Ch.ChartPadding.right =sys.$checkExists(Ch.ChartPadding.right,sys.$checkNull( 4));
      Ch.ChartPadding.bottom =sys.$checkExists(Ch.ChartPadding.bottom,sys.$checkNull( 2));
      Ch.ChartPadding.left =sys.$checkExists(Ch.ChartPadding.left,sys.$checkNull( 2));
      Ch.Labels.onPopup =sys.$checkExists(Ch.Labels.onPopup,sys.$checkNull( true));

       function mk0(chartDiv, zoomDiv, zoom)  {sys.$params(arguments.length, 3);
        const Lbs =sys.$checkNull( []);
        const Vals =sys.$checkNull( [[], [], []]); 
        const Dates2 =sys.$checkNull(sys.asBool( zoom) ? sys.$slice(Dates, -30,null) : Dates);
        const Closes2 =sys.$checkNull(sys.asBool( zoom) ? sys.$slice(Closes, -30,null) : Closes);
        const Refs2 =sys.$checkNull(sys.asBool( zoom) ? sys.$slice(Refs[0], -30,null) : Refs[0]);

        for (let i = 0;i < arr.size(Dates2); ++i) {
          const cl =sys.$checkNull( Closes2[i]);
          const rf =sys.$checkNull( Refs2[i]);
          arr.push(Lbs, time.toIso(time.fromStr(Dates2[i])));
          arr.push(Vals[0], [cl]);
          arr.push(Vals[1],sys.asBool( cl > rf) ? [rf] : []);
          arr.push(Vals[2],sys.asBool( cl < rf) ? [rf] : []);
        }
        const wln =sys.$checkNull(sys.asBool( zoom) ? 1.8 : 1);
        const Atts =sys.$checkNull( [
          lineChart.mkLine(wln, "#414141", false),
          lineChart.mkLine(wln, "#aa4100", false),
          lineChart.mkLine(wln, "#0041aa", false)
        ]);
        const Data =sys.$checkNull( lineChart.mkData(Lbs, Vals, Atts));
        Data.round =sys.$checkExists(Data.round,sys.$checkNull( 2));
        Data.Labels =sys.$checkExists(Data.Labels,sys.$checkNull( Lbs));
        Data.SetValues =sys.$checkExists(Data.SetValues,sys.$checkNull( [Vals[0], Vals[1], Vals[2]]));
        if (sys.asBool(sys.asBool(price >= 0) && sys.asBool(!sys.asBool(zoom))))
          Data.UnarySets =sys.$checkExists(Data.UnarySets,sys.$checkNull( [lineChart.mkUnarySet(
              II("Price"),
              price,
              lineChart.mkLine(1.2, "#c9c9c9", false)
            )]));
        Data.SetAtts =sys.$checkExists(Data.SetAtts,sys.$checkNull( Atts));
        Data.maxMinRound =sys.$checkExists(Data.maxMinRound, function(mx, mn)  {sys.$params(arguments.length, 2); return sys.asBool( mx > 10) ? 0 :  -1;});
        Data.drawGrid =sys.$checkExists(Data.drawGrid, function(lb, i)  {sys.$params(arguments.length, 2);
          if (sys.asBool(sys.$eq(i , 0)))  return false;
          return sys.asBool( zoom)
            ? sys.$eq(i % 2 , 0)
            : sys.asBool(sys.$neq(sys.$slice(Lbs[i - 1],3,5) , sys.$slice(lb,3,5))) &&
                sys.asBool(sys.$eq(sys.$slice(lb,3,5),"03")|| sys.$eq(sys.$slice(lb,3,5),"06")|| sys.$eq(sys.$slice(lb,3,5),"09")|| sys.$eq(sys.$slice(lb,3,5),"12")? true:  false)
          ;
        });
        Data.drawLabel =sys.$checkExists(Data.drawLabel,sys.$checkNull( Data.drawGrid));
        Data.mapLabel =sys.$checkExists(Data.mapLabel, function(lb, i)  {sys.$params(arguments.length, 2); return sys.asBool( zoom) ? sys.$slice(lb,null,2) : sys.$slice(lb,3,5);});

        chartDiv
          .removeAll()
          .add(lineChart.mkWg(Ch, Data))
        ;

        zoomDiv
          .removeAll()
          .add(ui.link(function(e)  {sys.$params(arguments.length, 1); mk0(chartDiv, zoomDiv, !sys.asBool(zoom));})
              .klass("frame")
              .add(ui.img(sys.asBool(zoom) ? "rk-new" : "minus2")
                  .style("vertical-align:top")))
        ;
      };

      const chartDiv =sys.$checkNull( Q("div"));
      const zoomDiv =sys.$checkNull( Q("div"));
      mk0(chartDiv, zoomDiv, true);
       return Q("table")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center")
            .text(nick))
          .add(Q("td")))
        .add(Q("tr")
          .add(Q("td")
            .klass("frame0")
            .style("background-color:" + back)
            .add(chartDiv))
          .add(Q("td")
            .style("width:80px;vertical-align:middle;")
            .add(zoomDiv)))


      ;
    };

    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:left;width:40%")
            .add(Q("a")
              .klass("link")
              .att("href", url)
              .text(nick)))
          .add(Q("td")
            .add(leds(invsN, Invs)))
          .add(Q("td")
            .style("text-align:right;width:40%")
            .add(Q("span")
              .html(math.toIso(profits, 2) + "&nbsp;&nbsp;"))
            .add(ui.img(sys.asBool(
              profits > 0) ? "profits" :sys.asBool( profits < 0) ? "losses" : "noresult"
            )
              .style("vertical-align:middle"))))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 3)
            .add(mkSmallGr()
              .setStyle("cursor", "pointer")
              .on("click", function(e)  {sys.$params(arguments.length, 1); msg.showWg(mkBigGr(), function(){sys.$params(arguments.length, 0);});})))))
    ;
  };

  
  Show[0] =sys.$checkExists(Show[0], function()  {sys.$params(arguments.length, 0);
    const Ls =sys.$checkNull(sys.asBool( ShowAll[0])
      ? List
      : arr.filter(List, function(E)  {sys.$params(arguments.length, 1);  return E.bought;}))
    ;
    arr.sort(Ls, function(E1, E2)  {sys.$params(arguments.length, 2);  return E1.nick < E2.nick;});

    const chs =sys.$checkNull( Q("table")
      .att("align", "center")
      .klass("frame"))
    ;

    const n =sys.$checkNull( arr.size(Ls));
    const Tr =sys.$checkNull( [Q("tr")]);
    for (let i = 0;i < n; ++i) {
      const chart =sys.$checkNull( Q("div"));
      mkChart(chart, Ls[i].nick, Ls[i].url);
      switch (i % 3) {
        case 0:{ {
          chs.add(separator());
          Tr[0] =sys.$checkExists(Tr[0],sys.$checkNull( Q("tr")));
          Tr[0].add(Q("td").add(chart));
        }break;}
        case 2:{ {
          Tr[0].add(Q("td").add(chart));
          chs.add(Tr[0]);
        }break;}
        default:{
          Tr[0].add(Q("td").add(chart));}
      }
    }

    switch (n % 3) {
      case 1:{ chs.add(Tr[0].add(Q("td")).add(Q("td")));break;}
      case 2:{ chs.add(Tr[0].add(Q("td")));break;}
    }
    chs.add(separator());

    wg
      .removeAll()
      .add(Q("div")
        .style("text-align:center")
        .add(ui.link(function(e)  {sys.$params(arguments.length, 1); changeShowAll();})
          .klass("link")
          .html(sys.asBool(ShowAll[0])
              ? II("Portfolio")
              : II("All Companies")
            )))
      .add(chs)
    ;
  });

  Show[0]();

};
