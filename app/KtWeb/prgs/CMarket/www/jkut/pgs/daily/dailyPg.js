import * as iter from '../../_js/iter.js';import * as str from '../../_js/str.js';import * as bytes from '../../_js/bytes.js';import * as cryp from '../../_js/cryp.js';import * as dic from '../../_js/dic.js';import * as timer from '../../_js/timer.js';import * as js from '../../_js/js.js';import * as storage from '../../_js/storage.js';import * as sys from '../../_js/sys.js';import * as math from '../../_js/math.js';import * as domo from '../../_js/domo.js';import * as ui from '../../_js/ui.js';import * as arr from '../../_js/arr.js';import * as time from '../../_js/time.js';import * as client from '../../_js/client.js';import * as b64 from '../../_js/b64.js';




import * as menu from  "../../libdm/menu.js";
import * as clock from  "../../libdm/clock.js";
import * as cts from  "../../data/cts.js";
import * as dailyChart from  "../../data/chart/dailyChart.js";
import * as dmenu from  "../../wgs/dmenu.js";
import * as i18n from  "../../i18n.js";
import * as cosSummaryChart from  "../../pgs/daily/cosSummaryChart.js";
import * as ixsSummaryChart from  "../../pgs/daily/ixsSummaryChart.js";
import * as coChart from  "../../pgs/daily/coChart.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


 function color(v)  {sys.$params(arguments.length, 1);
   return "color:" + (sys.asBool(v > 0) ? "#00AAFF" :sys.asBool( v < 0) ? "#FF8100" : "#404040");};








export  async  function mk2(wg, dbmenu, mSel, order, isReverse)  {sys.$params(arguments.length, 5);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    module: "DailyPg",
    source: "Daily",
    rq: "idata"
  }));
  const CosData =sys.$checkNull( arr.map(Rp.CosData, dailyChart.fromJs));
  const IxsData =sys.$checkNull( arr.map(Rp.IxsData, dailyChart.fromJs));
  const CosSel =sys.$checkNull( Rp.CosSel); 
  const Rebuys =sys.$checkNull( Rp.Rebuys); 
  const serverName =sys.$checkNull( Rp.serverName);
  const activity =sys.$checkNull( Rp.activity);
  const Order =sys.$checkNull( [order]);
  const IsReverse =sys.$checkNull( [isReverse]);

  const Profitss =sys.$checkNull( arr.reduce(
    CosData,
    [0, 0],
    function(R, D)  {sys.$params(arguments.length, 2);
      const cl =sys.$checkNull( D.close);
      const q =sys.$checkNull( arr.peek(D.quotes));
      const Total =sys.$checkNull( arr.reduce(
        D.invs,
        [0, 0],
        function(R, I)  {sys.$params(arguments.length, 2);  return [
            R[0] + I.stocks * (q - (sys.asBool(I.isNew) ? I.price : cl)),
            R[1] + I.stocks * (q - I.price)
          ];}
      ));
       return [R[0] + Total[0], R[1] + Total[1]];
    }
  ));
  const dayProfits =sys.$checkNull( Profitss[0]);
  const totalProfits =sys.$checkNull( Profitss[1]);

  const serverWg =sys.$checkNull( Q("span").text(serverName));
  const activityWg =sys.$checkNull( Q("span")
    .text(sys.asBool(sys.$eq(activity , cts.active)) ? "· · · ·" : ""))
  ;
  const Show =sys.$checkNull( [[]]);

  

    
     async  function reactivate()  {sys.$params(arguments.length, 0);
      if (sys.asBool(!sys.asBool(ui.confirm(II("Reactivate daily charts?"))))) return;

      serverWg
        .removeAll()
        .add(ui.img("wait.gif")
          .style("vertical-align:middle"))
      ;
      await client.send({
        prg: cts.appName,
        module: "DailyPg",
        source: "Daily",
        rq: "reactivate"
      });
      mk2(wg, dbmenu, mSel, Order[0], IsReverse[0]);
    };

    
     function newServer()  {sys.$params(arguments.length, 0); ui.alert(II("Currently this functions is deactivated"));};

    
     function updateActivityWg(tic)  {sys.$params(arguments.length, 1);
      activityWg
        .removeAll()
        .text(sys.asBool(tic < 0)
            ? ""
            : iter.reduce(iter.$range(0,tic), "", function(r, i)  {sys.$params(arguments.length, 2);   return r + " ·";})
          )
      ;
    };

    
     async  function setSelected(nick, isSel)  {sys.$params(arguments.length, 2);
      await client.send({
        prg: cts.appName,
        module: "DailyPg",
        source: "Daily",
        rq: "setSelected",
        nick:nick,
        isSel:isSel
      });

      mk2(wg, dbmenu, mSel, Order[0], IsReverse[0]);
    };

  

   function mkCoHead(D)  {sys.$params(arguments.length, 1);
    
     function separator()  {sys.$params(arguments.length, 0);  return Q("span").html("&nbsp;&nbsp;");};

    
     function color(v)  {sys.$params(arguments.length, 1);
       return "color:" + (sys.asBool(v > 0) ? "#00aaff" :sys.asBool( v < 0) ? "#ff8100" : "#a9a9a9")
    ;};

    const nick =sys.$checkNull( D.nick);
    const close =sys.$checkNull( D.close);
    const quote =sys.$checkNull( arr.peek(D.quotes));
    const ref =sys.$checkNull( D.invs[0].ref);
    const StDayTt =sys.$checkNull( arr.reduce(
      D.invs,
      [0, 0, 0],
      function(R, I)   {sys.$params(arguments.length, 2);
        const stocks =sys.$checkNull( I.stocks);
        const price =sys.$checkNull( I.price);
         return [
          R[0] + stocks,
          R[1] + stocks * (quote - (sys.asBool(I.isNew) ? price : close)),
          R[2] + stocks * (quote - price)
        ];
      }
    ));
    const stocks =sys.$checkNull( StDayTt[0]);
    const dailyProfits =sys.$checkNull( StDayTt[1]);
    const totalProfits =sys.$checkNull( StDayTt[2]);
    const isSel =sys.$checkNull( arr.any(CosSel, function(c)  {sys.$params(arguments.length, 1);  return sys.$eq(c , nick);}));
    const isRebuy =sys.$checkNull( arr.any(Rebuys, function(r)  {sys.$params(arguments.length, 1);  return sys.$eq(r , nick);}));
    const dif =sys.$checkNull( (quote - close) / close);
    const rdif =sys.$checkNull(sys.asBool( close > ref) ? ref / quote : quote / ref);


     return Q("tr")
        .add(Q("td").klass("chartLeft")
          .add(Q("span")
            .text(nick))
          .add(separator())
          .add(sys.asBool(isSel)
            ? ui.link(function(e)  {sys.$params(arguments.length, 1); setSelected(nick, false);})
              .add(ui.img("unlink"))
            : ui.link(function(e)  {sys.$params(arguments.length, 1); setSelected(nick, true);})
              .add(ui.img("link")))
          .add(separator())
          .add(Q("span")
            .style("font-size:small")
            .text(math.toIso(quote,sys.asBool( quote >= 10) ? 2 : 3)))
          .add(separator())
          .add(sys.asBool(dif > 0)
            ? ui.img("money_plus")
            :sys.asBool( dif < 0)
              ? ui.img("money_minus")
              : ui.img("money"))
          .add(separator())
          .add(Q("span")
            .style("font-size:small;" + color(dif))
            .text(math.toIso(dif * 100, 2) + "%"))
          .add(Q("br"))
          .add(Q("span")
            .style("font-size:small;" + color(sys.asBool(close > ref)
                ?  -1
                :sys.asBool( isRebuy) ? 0 : 1
              ))
            .text(math.toIso(rdif * 100, 2) + "%")
          ))
        .add(sys.asBool(stocks > 0)
          ? Q("td")
            .klass("chartRight")
            .add(Q("span")
              .style(color(dailyProfits))
              .text(math.toIso(dailyProfits, 2)))
            .add(Q("br"))
            .add(Q("span")
              .style(color(totalProfits))
              .text(math.toIso(totalProfits, 2)))
          : Q("td").klass("chartRight"))
    ;
  };

  

  
   function mkSummary()  {sys.$params(arguments.length, 0);
    const YesterdayValue =sys.$checkNull( [0.0]);
    const TodayValue =sys.$checkNull( [0,0]);
    const Labels =sys.$checkNull( CosData[0].hours);
    const size =sys.$checkNull( arr.size(Labels));
    const Values =sys.$checkNull( arr.mk(size, 0));
    for (let CD  of sys.$forObject( CosData)) {
      const cl =sys.$checkNull( CD.close);
      const Quotes =sys.$checkNull( CD.quotes);
      const quote =sys.$checkNull( arr.peek(Quotes));
      const StYsTt =sys.$checkNull( arr.reduce(
        CD.invs,
        [0, 0, 0],
        function(R, I)  {sys.$params(arguments.length, 2);
          const stocks =sys.$checkNull( I.stocks);
          const price =sys.$checkNull( I.price);
           return [
            R[0] + stocks,
            R[1] + stocks * (sys.asBool(I.isNew) ? price : cl),
            R[2] + stocks * price
          ];
        }
      ));
      const stocks =sys.$checkNull( StYsTt[0]);
      YesterdayValue[0] +=sys.$checkExists(YesterdayValue[0],sys.$checkNull( StYsTt[1]));
      const ttPrice =sys.$checkNull( StYsTt[2]);
      for (let i = 0;i < size; ++i) Values[i] +=sys.$checkExists(Values[i],sys.$checkNull( stocks * Quotes[i] - ttPrice));
      TodayValue[0] +=sys.$checkExists(TodayValue[0],sys.$checkNull( stocks * quote));
    }
    const yesterdayValue =sys.$checkNull( YesterdayValue[0]);
    const dailyProfits =sys.$checkNull( TodayValue[0] - yesterdayValue);
    const ratio =sys.$checkNull( (dailyProfits) / yesterdayValue);
    const CosChartValues =sys.$checkNull( [arr.map(Values, function(v)  {sys.$params(arguments.length, 1);  return [v];})]);

    const MeData =sys.$checkNull( IxsData[0]);
    const meYesterday =sys.$checkNull( MeData.close);
    const MeRatios =sys.$checkNull( arr.map(
      MeData.quotes,
      function(q)  {sys.$params(arguments.length, 1);  return (q - meYesterday) / meYesterday;}
    ));
    const IxsChartValues =sys.$checkNull( arr.fromIter(iter.map(
      iter.$range(1,arr.size(IxsData)),
      function(i)  {sys.$params(arguments.length, 1);
        const Data =sys.$checkNull( IxsData[i]);
        const yesterDay =sys.$checkNull( Data.close);
        const Quotes =sys.$checkNull( Data.quotes);
        const Values =sys.$checkNull( arr.mk(size, [0]));
        const usaZeroesV =sys.$checkNull( [true]);
        for (let j = 0;j < size; ++j) {
          if (sys.asBool(sys.asBool(sys.$eq(i , 3)) && sys.asBool(usaZeroesV[0]))) {
            if (sys.asBool(sys.$eq(Quotes[j] , Quotes[0]))) {
              Values[j] =sys.$checkExists(Values[j],sys.$checkNull( []));
              continue;
            }
            usaZeroesV[0] =sys.$checkExists(usaZeroesV[0],sys.$checkNull( false));
          }
          Values[j] =sys.$checkExists(Values[j],sys.$checkNull(sys.asBool( j < 3)
            ? [0]
            :[(MeRatios[j] - (Quotes[j] - yesterDay) / yesterDay)*100]))
          ;
        }
         return Values;
      }
    )));

    
     function ixsText(index)  {sys.$params(arguments.length, 1);
      const text =sys.$checkNull(sys.asBool( sys.$eq(index , cts.meNick))
        ? "DEMEX"
        :sys.asBool( sys.$eq(index , cts.ibexNick))
          ? "IBEX"
          :sys.asBool( sys.$eq(index , cts.euroNick))
            ? "EUROSTOXX"
            : "SP-500")
      ;
      const txColor =sys.$checkNull(sys.asBool( sys.$eq(index , cts.meNick))
        ? "#000000"
        :sys.asBool( sys.$eq(index , cts.ibexNick))
          ? "#000080"
          :sys.asBool( sys.$eq(index , cts.euroNick))
            ? "#008000"
            : "#800000")
      ;

      const Data =sys.$checkNull( arr.find(IxsData, function(D)  {sys.$params(arguments.length, 1);  return sys.$eq(D.nick , index);})[0]);
      const v0 =sys.$checkNull( Data.close);
      const vf =sys.$checkNull( arr.peek(Data.quotes));
      const nmColor =sys.$checkNull(sys.asBool( vf > v0) ? "#00AAFF" :sys.asBool( vf < v0) ? "#FF8100" : "#000000");
       return Q("span")
        .add(Q("span")
          .style("color:" + txColor)
          .html(text + ":&nbsp;"))
        .add(Q("span")
          .style("color:" + nmColor)
          .text(math.toIso(vf, 2) + "[" + math.toIso((vf-v0)*100/v0, 2) + "%]"))
      ;
    };



    wg
      .removeAll()
      .add(Q("div").style("text-align:center;")
        .add(Q("div")
          .klass("head")
          .style("padding-bottom:8px")
          .html(II("Summary")))
        .add(Q("div")
          .add(Q("span")
            .klass("frame")
            .style(
              'font-size:x-large;color:' +
              (sys.asBool(ratio > 0) ? "#00AAFF" :sys.asBool( ratio < 0) ? "#FF8100" : "#000000")
            )
            .html(
              " " + math.toIso(ratio * 100, 2) + "% | " +
              math.toIso(dailyProfits, 2) + "€ "
            )))
        .add(cosSummaryChart.mk(Labels, CosChartValues))
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .style("text-align:left")
              .add(ixsText(cts.meNick)))
            .add(Q("td")
              .style("text-align:right")
              .add(ixsText(cts.euroNick))))
          .add(Q("tr")
            .add(Q("td")
              .style("text-align:left")
              .add(ixsText(cts.ibexNick)))
            .add(Q("td")
              .style("text-align:right")
              .add(ixsText(cts.usaNick))))
          .add(Q("tr")
            .add(Q("td")
              .att("colspan", 2)
              .add(ixsSummaryChart.mk(Labels, IxsChartValues)))))
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .style("vertical-align:bottom")
              .add(clock.wg(clock.mk())
                .klass("frame")
                .style(
                  "background:radial-gradient(#000333,#e6f6f6);" +
                  "margin-top: 8px;"
                )))
            .add(Q("td")
              .style("vertical-align:bottom")
              .add(Q("iframe")
                .klass("frame")
                .att("width", "450px")
                .att("height", "133px")
                .att(
                  "src",
                  "http://www.aemet.es/es/eltiempo/prediccion/" +
                  "municipios/mostrarwidget/rivas-vaciamadrid-id28123?" +
                  "w=g4p01110001ohmffffffw450z133x4f86d9t95b6e9r0s4n1"
                ))))))
    ;
  };

  

  
   function mkCos()  {sys.$params(arguments.length, 0);
    const CosData2 =sys.$checkNull(sys.asBool(
      sys.$eq(mSel , "portfolio"))
        ? arr.filter(
            CosData,
            function(D)  {sys.$params(arguments.length, 1);  return arr.any(D.invs, function(i)  {sys.$params(arguments.length, 1);  return i.stocks > 0;});}
          )
        :sys.asBool( sys.$eq(mSel , "sel"))
          ? arr.filter(
              CosData,
              function(D)  {sys.$params(arguments.length, 1);  return arr.any(CosSel, function(c)  {sys.$params(arguments.length, 1);  return sys.$eq(c , D.nick);});}
            )
          : CosData)
    ;
    switch (Order[0]) {
      case cts.chartOrderSignal:{
        arr.sort(
          CosData2,
          function(D1, D2)  {sys.$params(arguments.length, 2);
             function fn(D)  {sys.$params(arguments.length, 1);
              const cl =sys.$checkNull( D.close);
              const q =sys.$checkNull( arr.peek(D.quotes));
               return arr.reduce(D.invs, 0,
                function(r, i)  {sys.$params(arguments.length, 2);  return Math.max(r,sys.asBool( cl > i.ref) ? i.ref / q : q / i.ref);}
              );
            };
             return fn(D1) > fn(D2);
          });break;}
      case cts.chartOrderDay:{
        arr.sort(
          CosData2,
          function(D1, D2)  {sys.$params(arguments.length, 2);  return (arr.peek(D1.quotes) - D1.close) / D1.close >
            (arr.peek(D2.quotes) - D2.close) / D2.close;}
        );break;}
      case cts.chartOrderNick:{
        arr.sort(CosData2, function(D1, D2)  {sys.$params(arguments.length, 2);  return D1.nick < D2.nick;});break;}
    }
    if (sys.asBool(IsReverse[0])) arr.reverseIn(CosData2);

    const menu =sys.$checkNull( Q("table")
      .att("align", "center")
      .style("padding-bottom:6px")
      .add(Q("tr")
        .add(Q("td")
          .add(Q("span")
            .html(II("Order by") + ":&nbsp;&nbsp;&nbsp;"))
          .add(ui.link(function(e)  {sys.$params(arguments.length, 1); Order[0] =sys.$checkExists(Order[0],sys.$checkNull( cts.chartOrderNick)); Show[0](); })
            .klass(sys.asBool(sys.$eq(Order[0] , cts.chartOrderNick)) ? "link frame" : "link")
            .text(II("Nick")))
          .add(Q("span")
            .html("&nbsp;&nbsp;&nbsp;"))
          .add(ui.link(function(e)  {sys.$params(arguments.length, 1); Order[0] =sys.$checkExists(Order[0],sys.$checkNull( cts.chartOrderDay)); Show[0](); })
            .klass(sys.asBool(sys.$eq(Order[0] , cts.chartOrderDay)) ? "link frame" : "link")
            .text(II("Day")))
          .add(Q("span")
            .html("&nbsp;&nbsp;&nbsp;"))
          .add(ui.link(function(e)  {sys.$params(arguments.length, 1); Order[0] =sys.$checkExists(Order[0],sys.$checkNull( cts.chartOrderSignal)); Show[0](); })
            .klass(sys.asBool(sys.$eq(Order[0] , cts.chartOrderSignal)) ? "link frame" : "link")
            .text(II("Signal")))
          .add(Q("span")
            .html("&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;"))
          .add(ui.link(function(e)  {sys.$params(arguments.length, 1); IsReverse[0] =sys.$checkExists(IsReverse[0],sys.$checkNull( !sys.asBool(IsReverse[0]))); Show[0]();})
            .klass(sys.asBool(IsReverse[0]) ? "link frame" : "link")
            .text(II("Reverse"))))))
    ;

    const size =sys.$checkNull( arr.size(CosData2));
    const table =sys.$checkNull( Q("table")
      .att("align", "center")
      .klass("frame")
      .adds(sys.asBool(sys.$eq(size , 0))
        ? [Q("tr")
          .add(Q("td")
            .text(II("No selected company")))
        ]
        : arr.fromIter(iter.map(
          iter.$range(0,math.toInt((size - 1) / 3) + 1),
          function(row)  {sys.$params(arguments.length, 1);
             return Q("tr")
              .adds(arr.fromIter(iter.map(
                iter.$range(0,3),
                function(col)  {sys.$params(arguments.length, 1);
                  const ix =sys.$checkNull( row * 3 + col);
                  if (sys.asBool(ix >= size))  return Q("td");
                  const D =sys.$checkNull( CosData2[ix]);
                  const Labels =sys.$checkNull( D.hours);
                  const Values =sys.$checkNull( [arr.map(D.quotes, function(q)  {sys.$params(arguments.length, 1);  return [q];})]);
                  const isRebuy =sys.$checkNull( arr.any(Rebuys, function(c)  {sys.$params(arguments.length, 1);  return sys.$eq(c , D.nick);}));
                  const ref =sys.$checkNull( D.invs[0].ref);
                  const isToSell =sys.$checkNull( D.close > ref);
                  const limRef =sys.$checkNull(sys.asBool( isToSell) ? ref * 1.01 : ref * 0.99);
                  const withRef0 =sys.$checkNull(sys.asBool( isToSell)
                    ? arr.any(D.quotes, function(q)  {sys.$params(arguments.length, 1);  return q < limRef;})
                    : arr.any(D.quotes, function(q)  {sys.$params(arguments.length, 1);  return q > limRef;}))
                  ;
                  const withRef =sys.$checkNull(sys.asBool( withRef0)
                    ?sys.asBool( isToSell) ?  -1 : 1
                    : 0)
                  ;
                   return Q("td")
                    .add(Q("table")
                      .klass("main")
                      .add(mkCoHead(D))
                      .add(Q("tr")
                        .add(Q("td")
                          .att("colspan", 2)
                          .add(Q("span")
                            .add(coChart.mk(
                                Labels, Values, isRebuy, withRef, ref
                              ))))))
                  ;
                }
              )));}
        ))))
    ;
    wg
      .removeAll()
      .add(menu)
      .add(table)
    ;
  };

  
  Show[0] =sys.$checkExists(Show[0], function()  {sys.$params(arguments.length, 0);
    const Lopts =sys.$checkNull( [
      dmenu.mkHiddenButton(dbmenu),
      menu.separator2(),
      menu.tlink("summary", II("Summary"), ["daily"]),
      menu.separator2(),
      menu.tlink("portfolio", II("Portfolio"), ["daily"]),
      menu.separator(),
      menu.tlink("all", II("All CO's"), ["daily"]),
      menu.separator(),
      menu.tlink("sel", II("Selection"), ["daily"])
    ]);

    const Ropts =sys.$checkNull( [
      menu.mkEntry([], activityWg),sys.asBool(
      sys.asBool(sys.$eq(activity , cts.active)) || sys.asBool(time.hour(time.now()) > 12)) ?sys.asBool(
        sys.$eq(activity , cts.active))
          ? menu.toption(activity, "[·]", reactivate)
          : menu.toption(activity, II("Sleeping"), reactivate)
        : menu.mkEntry([], activityWg),
      menu.separator2(),
      menu.mkEntry(
        [],
        Q("span")
          .style(color(dayProfits))
          .text(math.toIso(dayProfits, 2))
      ),
      menu.separator(),
      menu.mkEntry(
        [],
        Q("span")
          .style(color(totalProfits))
          .text(math.toIso(totalProfits, 2))
      ),
      menu.separator2(),
      menu.mkEntry([], serverWg),
      menu.separator(),
      menu.toption(">>", ">>", newServer)
    ]);
    dmenu.setDownMenu(dbmenu, menu.mk(Lopts, Ropts, mSel, false));

    switch (mSel) {
      case "portfolio": case "all": case "sel":{ mkCos();break;}
      default:{ mkSummary();}
    }
  });

  Show[0]();

  const Tic =sys.$checkNull( [0]);
  const tm =sys.$checkNull( timer.mk(15000));
  timer.run(tm, function()  {sys.$params(arguments.length, 0);
    const tic =sys.$checkNull( Tic[0]);

    if (sys.asBool(sys.$eq(tic , 3))) {
      timer.stop(tm);
      mk2(wg, dbmenu, mSel, Order[0], IsReverse[0]);
       return 0;
    }

    Tic[0] +=sys.$checkExists(Tic[0],sys.$checkNull( 1));
    if (sys.asBool(sys.$eq(activity , cts.active))) {
      updateActivityWg(3 - tic);
    } else {
      updateActivityWg( -1);
    }

     return 1;
  });
};






export  async  function mk(wg, dbmenu, LcPath)  {sys.$params(arguments.length, 3);
  mk2(wg, dbmenu,sys.asBool( LcPath) ? LcPath[0] : "summary", cts.chartOrderSignal, false);};
