import * as iter from '../../_js/iter.js';import * as str from '../../_js/str.js';import * as bytes from '../../_js/bytes.js';import * as cryp from '../../_js/cryp.js';import * as dic from '../../_js/dic.js';import * as timer from '../../_js/timer.js';import * as js from '../../_js/js.js';import * as storage from '../../_js/storage.js';import * as sys from '../../_js/sys.js';import * as math from '../../_js/math.js';import * as domo from '../../_js/domo.js';import * as ui from '../../_js/ui.js';import * as arr from '../../_js/arr.js';import * as time from '../../_js/time.js';import * as client from '../../_js/client.js';import * as b64 from '../../_js/b64.js';




import * as menu from  "../../libdm/menu.js";
import * as modalBox from  "../../libdm/modalBox.js";
import * as cts from  "../../data/cts.js";
import * as broker from  "../../data/broker.js";
import * as invOperation from  "../../data/invOperation.js";
import * as msg from  "../../wgs/msg.js";
import * as i18n from  "../../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


 function invTd(inv)  {sys.$params(arguments.length, 1);
   return Q("td")
    .klass("borderWhite")
    .text("" + inv)
  ;};


 function ciaTd(nick)  {sys.$params(arguments.length, 1);
   return Q("td")
    .klass("borderWhite")
    .style("text-align:left")
    .text(nick)
  ;};


 function stocksTd(stocks)  {sys.$params(arguments.length, 1);
   return Q("td")
    .klass("borderWhite")
    .style("text-align:right")
    .text(math.toIso(stocks, 0))
  ;};


 function quoteTd(q)  {sys.$params(arguments.length, 1);
   return Q("td")
    .klass("borderWhite")
    .style("text-align:right")
    .text(math.toIso(q, 4))
  ;};


 function moneyTd(q)  {sys.$params(arguments.length, 1);
   return Q("td")
    .klass("borderWhite")
    .style("text-align:right")
    .text(math.toIso(q, 2))
  ;};



export  async  function mk(wg)  {sys.$params(arguments.length, 1);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    module: "Acc",
    source: "TradingPg",
    rq: "idata"
  }));

  if (sys.asBool(!sys.asBool(Rp.ok))) {
    msg.error(cts.failMsg, function(){sys.$params(arguments.length, 0);});
    return;
  }

  const InvOperations =sys.$checkNull( arr.map(Rp.invOperations, invOperation.fromJs));
  const Portfolios =sys.$checkNull( Rp.portfolios); 
  const Closes =sys.$checkNull( Rp.closes); 
  const Rebuys =sys.$checkNull( Rp.rebuys); 

  const msgWait =sys.$checkNull( Q("div"));
  const bentry =sys.$checkNull( ui.field("pentry")
    .style("width:80px")
    .value(math.toIso(cts.bet, 0)));
  ui.changePoint(bentry);
  const pentry =sys.$checkNull( ui.field("calcBuyBt")
    .style("width:80px")
    .att("id", "pentry"));
  ui.changePoint(pentry);
  const price2 =sys.$checkNull( Q("div").klass("frame").style("text-align:right"));
  const result =sys.$checkNull( Q("div").klass("frame").style("text-align:right"));
  const result2 =sys.$checkNull( Q("div").klass("frame").style("text-align:right"));

  const sentry =sys.$checkNull( ui.field("calcSellBt")
    .style("width:80px")
    .att("id", "sentry"));
  ui.changePoint(sentry);
  const sprice2 =sys.$checkNull( Q("div").klass("frame").style("text-align:right"));

  

  
   async  function update()  {sys.$params(arguments.length, 0);
    const box =sys.$checkNull( modalBox.mk(
      Q("div")
        .add(Q("div")
          .style("text-align:center")
          .add(ui.img("wait2.gif").klass("frame"))),
      false
    ));
    msgWait.add(box.wg);
    modalBox.show(box, true);
    await client.send({
      prg: cts.appName,
      module: "Acc",
      source: "TradingPg",
      rq: "update"
    });
    modalBox.show(box, false);
    mk(wg);
  };

  
   function calculateBuy()  {sys.$params(arguments.length, 0);
    const bs =sys.$checkNull( bentry.getValue());
    const ps =sys.$checkNull( pentry.getValue());
    const B =sys.$checkNull( math.fromIso(bs));
    if (sys.asBool(!sys.asBool(B))) {
      ui.alert(i18n.fmt(II("'%0' is not a valid number."), [bs]));
      return;
    }
    const b =sys.$checkNull( B[0]);
    const b2 =sys.$checkNull( b + b - broker.buy(1, b));

    const P =sys.$checkNull( math.fromIso(ps));
    if (sys.asBool(!sys.asBool(P))) {
      ui.alert(i18n.fmt(II("'%0' is not a valid number."), [ps]));
      return;
    }
    const p =sys.$checkNull( P[0]);
    if (sys.asBool(sys.$eq(p , 0))) {
      ui.alert(II("Price is 0"));
      return;
    }

    const rs =sys.$checkNull( math.toInt(b2 / p));
    const p2 =sys.$checkNull( p * 0.99);
    const rs2 =sys.$checkNull( math.toInt(b2 / p2));

    price2.text(math.toIso(p2, 2));
    result.text(math.toIso(rs, 0));
    result2.text(math.toIso(rs2, 0));
  };

  
   function calculateSell()  {sys.$params(arguments.length, 0);
    const ps =sys.$checkNull( sentry.getValue());
    const P =sys.$checkNull( math.fromIso(ps));
    if (sys.asBool(!sys.asBool(P))) {
      ui.alert(i18n.fmt(II("'%0' is not a valid number."), [ps]));
      return;
    }
    sprice2.text(math.toIso(P[0] * 1.01, 2));
  };

  

  
   function buyWg()  {sys.$params(arguments.length, 0);
     return Q("table")
      .klass("frame4")
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "2")
          .add(Q("div")
            .klass("head")
            .text(II("Buy")))))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "2")
          .add(Q("hr"))))
      .add(Q("tr")
        .add(Q("td")
          .style("text-align:right")
          .add(Q("div")
            .html("<b>" + II("Invest") + ":</b>")))
        .add(Q("td")
          .add(bentry)))
      .add(Q("tr")
        .add(Q("td")
          .style("text-align:right")
          .add(Q("div")
            .html("<b>" + II("Price") + ":</b>")))
        .add(Q("td")
          .add(pentry)))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "2")
          .style("text-align:center")
          .add(Q("button")
            .att("id", "calcBuyBt")
            .text(II("Calculate"))
            .on("click", function(e)  {sys.$params(arguments.length, 1); calculateBuy();}))))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "2")
          .add(Q("hr"))))
      .add(Q("tr")
        .add(Q("td")
          .style("text-align:right")
          .add(Q("div")
            .html("<b>" + II("Stocks") + ":</b>")))
        .add(Q("td")
          .add(result)))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "2")
          .add(Q("hr"))))
      .add(Q("tr")
        .add(Q("td")
          .style("text-align:right;white-space:nowrap")
          .add(Q("div")
            .html("<b>" + II("Price") + " + :</b>")))
        .add(Q("td")
          .add(price2)))
      .add(Q("tr")
        .add(Q("td")
          .style("text-align:right;white-space:nowrap")
          .add(Q("div")
            .html("<b>" + II("Stocks") + " + :</b>")))
        .add(Q("td")
          .add(result2)))
  ;};

  
   function sellWg()  {sys.$params(arguments.length, 0);
     return Q("table")
      .klass("frame3")
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "2")
          .add(Q("div")
            .klass("head")
            .text(II("Sell")))))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "2")
          .add(Q("hr"))))
      .add(Q("tr")
        .add(Q("td")
          .style("text-align:right")
          .add(Q("div")
            .html("<b>" + II("Price") + ":</b>")))
        .add(Q("td")
          .add(sentry)))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "2")
          .style("text-align:center")
          .add(Q("button")
            .att("id", "calcSellBt")
            .text(II("Calculate"))
            .on("click", function(e)  {sys.$params(arguments.length, 1); calculateSell();}))))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "2")
          .add(Q("hr"))))
      .add(Q("tr")
        .add(Q("td")
          .style("text-align:right;white-space:nowrap")
          .add(Q("div")
            .html("<b>" + II("Price") + " + :</b>")))
        .add(Q("td")
          .add(sprice2)))
    ;};

  const RebuysArr =sys.$checkNull( dic.toArr(Rebuys));
  arr.sort(RebuysArr, function(Tp1, Tp2)  {sys.$params(arguments.length, 2);
    return sys.asBool( sys.$eq(Tp1[1] , Tp2[1])) ? Tp1[0] < Tp2[0] : Tp1[1] < Tp2[1];});
  const rebuyTrs =sys.$checkNull(sys.asBool( RebuysArr)
    ? function()  {sys.$params(arguments.length, 0);
      const Trs =sys.$checkNull( []);
      const iV =sys.$checkNull([0]);
      const trV =sys.$checkNull( [Q("tr")]);
      while (sys.asBool(iV[0] < arr.size(RebuysArr))) {
        const Tp =sys.$checkNull( RebuysArr[iV[0]]);
        trV[0]
          .add(ciaTd(Tp[0]))
          .add(ciaTd(sys.asBool(
            sys.$eq(i18n.getLang() , "es"))
              ? time.toIso(time.fromStr(Tp[1]))
              : time.toEn(time.fromStr(Tp[1]))))
        ;
        if (sys.asBool(sys.$neq(iV[0] % 3 , 2)))
          trV[0].add(Q("td").klass("separator"));
        iV[0] +=sys.$checkExists(iV[0],sys.$checkNull( 1));
        if (sys.asBool(sys.asBool(sys.$eq(iV[0] % 3 , 0)) && sys.asBool(iV[0] < arr.size(RebuysArr)))) {
          arr.push(Trs, trV[0]);
          trV[0] =sys.$checkExists(trV[0],sys.$checkNull( Q("tr")));
        }
      }
      if (sys.asBool(sys.$neq(iV[0] % 3 , 0))) arr.push(Trs, trV[0]);
       return Trs;
    }()
    : [ Q("tr")
        .add(Q("td")
          .att("colspan", 2)
          .klass("borderWhite")
          .html(II("Without operations")))
      ])
  ;

  arr.sort(InvOperations, function(O1, O2)  {sys.$params(arguments.length, 2); return sys.asBool(
    sys.$eq(O1.nick , O2.nick)) ? O1.investor < O2.investor : O1.nick < O2.nick;}
  );
  const BuyOps =sys.$checkNull( arr.filter(
    InvOperations,
    function(O)  {sys.$params(arguments.length, 1);  return sys.asBool(O.stocks <= 0) && sys.asBool(sys.$eq(O.investor , 0));}
  ));
  const buyTrs =sys.$checkNull(sys.asBool( BuyOps)
    ? arr.map(
       BuyOps ,
        function(O)  {sys.$params(arguments.length, 1);  return Q("tr")
          .add(ciaTd(O.nick).setStyle(
              "text-decoration",sys.asBool(
              arr.any(RebuysArr, function(Tp)  {sys.$params(arguments.length, 1);  return sys.$eq(Tp[0] , O.nick);}))
              ? "line-through"
              : ""
            ))
          .add(invTd(O.investor))
      ;})
    : [ Q("tr")
        .add(Q("td")
          .att("colspan", 2)
          .klass("borderWhite")
          .html(II("Without operations")))
      ])
  ;

  const SellOps =sys.$checkNull( arr.filter(InvOperations, function(O)  {sys.$params(arguments.length, 1);  return O.stocks > 0;}));
  const sellTrs =sys.$checkNull(sys.asBool( SellOps)
    ? arr.map(
        SellOps,
        function(O)  {sys.$params(arguments.length, 1);
          const pr =sys.$checkNull( Portfolios[O.investor][O.nick][1]);
          const cl =sys.$checkNull( Closes[O.nick]);
          const dif =sys.$checkNull( (cl - pr) * 100 / pr);
          const color =sys.$checkNull(sys.asBool( dif >= 0) ? "#d0d9f0" :sys.asBool( dif >=  -5) ? "#f0f0d9" : "#ffffff");
           return Q("tr")
            .add(ciaTd(O.nick).setStyle("background", color))
            .add(invTd(O.investor).setStyle("background", color))
            .add(stocksTd(O.stocks).setStyle("background", color))
            .add(quoteTd(pr).setStyle("background", color))
            .add(quoteTd(cl).setStyle("background", color))
            .add(quoteTd(dif).setStyle("background", color))
          ;
      })
    : [ Q("tr")
        .add(Q("td")
          .att("colspan", 6)
          .klass("borderWhite")
          .html(II("Without operations")))
      ])
  ;

  wg
    .removeAll()
    .add(msgWait)
    .add(Q("table")
      .klass("main")
      .add(Q("tr")
        .add(Q("td")
          .style("vertical-align:top;width:5px")
          .add(buyWg()))
        .add(Q("td")
          .style("text-align:center")
            .add(Q("table")
              .att("align", "center")
              .add(Q("tr")
                .add(Q("td")
                  .add(ui.link(function(e)  {sys.$params(arguments.length, 1); update();})
                    .klass("link")
                    .text(II("Update")))))
              .add(Q("tr")
                .add(Q("td")
                  .klass("head")
                  .html("&nbsp;"))))
            .add(Q("table")
              .klass("frame")
              .att("align", "center")
              .add(Q("tr")
                .add(Q("td")
                  .add(Q("div")
                    .klass("head")
                    .html(II("Rebuys")))
                  .add(Q("table")
                    .klass("frame2")
                    .style("border-collapse : collapse;")
                    .add(Q("tr")
                      .adds(iter.reduce(
                          iter.$range(0,(sys.asBool(arr.size(RebuysArr) > 2) ? 2 : arr.size(RebuysArr) -1)),
                          [ Q("td").klass("head").html(II("Co.")),
                            Q("td").klass("head").html(II("Date")),
                            Q("td").klass("separator")
                          ],
                          function(R, i)  {sys.$params(arguments.length, 2);
                            arr.push(R, Q("td").klass("head").html(II("Co.")));
                            arr.push(R, Q("td").klass("head").html(II("Date")));
                            if (sys.asBool(sys.$neq(i % 3 , 1)))
                              arr.push(R, Q("td").klass("separator"));
                             return R;
                          }
                        )))
                    .adds(rebuyTrs))
                  .add(Q("div").style("padding:7px"))
                  .add(Q("table")
                    .klass("frame3")
                    .att("align", "center")
                    .style("border-collapse : collapse;")
                    .add(Q("tr")
                      .add(Q("td").klass("head").html(II("Co.")))
                      .add(Q("td").klass("head").html(II("Losses"))))
                    .add(Q("tr")
                      .add(ciaTd("EDR"))
                      .add(moneyTd(1992.17)))
                    .add(Q("tr")
                      .add(ciaTd("MEL"))
                      .add(moneyTd(1296.53)))
                    .add(Q("tr")
                      .add(ciaTd("MTS"))
                      .add(moneyTd(21.73)))
                  ))))
          .add(Q("div")
            .klass("head")
            .html("&nbsp;"))
          .add(Q("div")
            .klass("head")
            .html(II("Buys")))
          .add(Q("table")
            .att("align", "center")
            .klass("buys")
            .add(Q("tr")
              .add(Q("td").klass("head").html(II("Co.")))
              .add(Q("td").klass("head").html(II("Inv"))))
            .adds(buyTrs))
          .add(Q("div")
            .klass("head")
            .html(II("Sells")))
          .add(Q("table")
            .att("align", "center")
            .klass("sells")
            .add(Q("tr")
              .add(Q("td").klass("head").html(II("Co.")))
              .add(Q("td").klass("head").html(II("Inv")))
              .add(Q("td").klass("head").html(II("Stocks")))
              .add(Q("td").klass("head").html(II("Price")))
              .add(Q("td").klass("head").html(II("Quote")))
              .add(Q("td").klass("head").html("%&Delta;"))
              )
            .adds(sellTrs)
            ))
        .add(Q("td")
          .style("vertical-align:top;width:5px")
          .add(sellWg()))))
  ;

};
