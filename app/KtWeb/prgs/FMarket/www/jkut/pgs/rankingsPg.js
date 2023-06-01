import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as menu from  "../libdm/menu.js";
import * as flea from  "../data/flea.js";
import * as ranking from  "../data/ranking.js";
import * as cts from  "../data/cts.js";
import * as fns from  "../data/fns.js";
import * as i18n from  "../i18n.js";

const Q = sys.$checkNull(ui.q);
const II = sys.$checkNull(i18n.tlt);


export async function mk (wg)  {sys.$params(arguments.length, 1);
  const Rp = sys.$checkNull(await  client.send({
    prg: cts.appName,
    source: "RankingsPg",
    rq: "idata"
  }));
  
  const Rankings = sys.$checkNull(arr.map(Rp.rankings, ranking.fromJs));

  const MenuSel = sys.$checkNull(["all"]);

  const Show = sys.$checkNull([[]]);



  
  function last ()  {sys.$params(arguments.length, 0);
      MenuSel[0] =sys.$checkExists(MenuSel[0], sys.$checkNull("last"));
      Show[0]();
    };

  
  function all ()  {sys.$params(arguments.length, 0);
      MenuSel[0] =sys.$checkExists(MenuSel[0], sys.$checkNull("all"));
      Show[0]();
    };

  
  
  function positionImg (rankIx, Flea, fIx)  {sys.$params(arguments.length, 3);
      const rankIx1 = sys.$checkNull(rankIx + 1);
      if (sys.asBool(rankIx1 >= arr.size(Rankings)))  return ui.img("rk-new");
      const fIx1 = sys.$checkNull(arr.index(Rankings[rankIx1].Fleas, function(F)  {sys.$params(arguments.length, 1);  return sys.$eq(F.id , Flea.id);}));
      if (sys.asBool(sys.$eq(fIx1 ,  -1)))  return ui.img("rk-new");
      if (sys.asBool(fIx > fIx1 + 4))  return ui.img("rk-down2");
      if (sys.asBool(fIx > fIx1))  return ui.img("rk-down");
      if (sys.asBool(fIx < fIx1 - 4))  return ui.img("rk-up2");
      if (sys.asBool(fIx < fIx1))  return ui.img("rk-up");
       return ui.img("rk-eq");
    };

  
  function isRemoved (rankIx, Flea)  {sys.$params(arguments.length, 2); return sys.asBool( sys.$eq(rankIx , 0))
      ? false
      : !sys.asBool(arr.any(Rankings[rankIx - 1].Fleas, function(F)  {sys.$params(arguments.length, 1);  return sys.$eq(F.id , Flea.id);}))
    ;};



  
  function showLast ()  {sys.$params(arguments.length, 0);
    
    function mkRow (F, i)  {sys.$params(arguments.length, 2);
      
      function mkModelsWg ()  {sys.$params(arguments.length, 0);  return arr.map(flea.fmtModels(F), function(fmt)  {sys.$params(arguments.length, 1);
           return Q("td")
            .klass("borderWhite")
            .style("text-align:left;font-family:monospace;white-space:pre")
            .text(fmt)
        ;});};

       return Q("tr")
        .add(Q("td")
          .add(positionImg(0, F, i)))
        .add(Q("td")
          .klass("border")
          .style("background:" + (sys.asBool(F.isMale) ? "#a0c0f0": "#f0c0a0"))
          .text(flea.fmtId(F)))
        .add(Q("td")
          .klass("borderWhite")
          .text(flea.fmtCycle(F)))
        .adds(mkModelsWg())
        .add(Q("td")
          .klass("number")
          .text(fns.nFormat(F.assets, 2)))
      ;
    };

    const Ix = sys.$checkNull([ -1]);
    const Trs = sys.$checkNull(arr.map(Rankings[0].Fleas, function(F)  {sys.$params(arguments.length, 1);
      Ix[0] +=sys.$checkExists(Ix[0], sys.$checkNull(1));
       return mkRow(F, Ix[0]);
    }));

    wg
      .add(Q("table")
        .klass("border")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header"))
          .add(Q("td")
            .klass("header")
            .style("text-align: left")
            .text(II("Id")))
          .add(Q("td")
            .klass("header")
            .style("text-align: left")
            .text(II("Cycle")))
          .add(Q("td")
            .klass("header")
            .att("colspan", 3)
            .style("text-align: left")
            .text(II("Models")))
          .add(Q("td")
            .klass("header")
            .style("text-align: right")
            .text(II("Assets"))))
        .adds(Trs))
    ;
  };

  
  function showAll ()  {sys.$params(arguments.length, 0);
    const nRks = sys.$checkNull(arr.size(Rankings));
    const nRks2 = sys.$checkNull(math.toInt(nRks / 2));

    
    function mkRows (start, end)  {sys.$params(arguments.length, 2);
       return arr.cat([[
        Q("tr")
          .adds(arr.fromIter(iter.map(iter.$range(start, end), function(i)  {sys.$params(arguments.length, 1);
             return Q("td")
              .klass("header")
              .att("colspan", 5)
              .text(fns.dFormat(Rankings[i].date))
            ;}))),
        Q("tr")
          .adds(iter.reduce(iter.$range(start, end), [], function(R, i)  {sys.$params(arguments.length, 2);
             return arr.cat([R, [
              Q("td")
                .klass("header"),
              Q("td")
                .klass("header")
                .style("text-align: left")
                .text(II("Id")),
              Q("td")
                .klass("header")
                .style("text-align: left")
                .text("C."),
              Q("td")
                .klass("header")
                .text(II("Ms.")),
              Q("td")
                .klass("header")
                .style("text-align: right")
                .text(II("Assets"))
            ]]);}))
        ], arr.map(arr.fromIter(iter.$range(0,arr.size(Rankings[0].Fleas))), function(row)  {sys.$params(arguments.length, 1);
           return Q("tr")
            .adds(iter.reduce(iter.$range(start , end), [], function(R, col)  {sys.$params(arguments.length, 2);
                  const F = sys.$checkNull(Rankings[col].Fleas[row]);
                   return arr.cat([R, [
                    Q("td")
                      .add(positionImg(col, F, row)),
                    Q("td")
                      .klass("border")
                      .style(
                          "text-align:right;" +
                          "text-decoration:"+ (sys.asBool(
                            isRemoved(col, F))
                              ? "line-through;"
                              : "none;"
                          ) +
                          "background:" + (sys.asBool(
                            sys.$eq(F.id , Rankings[0].Fleas[0].id))
                              ? "#ccad0f"
                              :sys.asBool( sys.$eq(F.id , Rankings[0].Fleas[1].id))
                                ? "#b4b3ad"
                                :sys.asBool( sys.$eq(F.id , Rankings[0].Fleas[2].id))
                                  ? "#9e6a25"
                                  : "rgb(250, 250, 250)"
                          )
                        )
                      .text("" + F.id),
                    Q("td")
                      .klass("borderWhite")
                      .style("text-align: right")
                      .text("" + F.cycle),
                    Q("td")
                      .klass("borderWhite")
                      .att("title", arr.join(flea.fmtModels(F), "\n"))
                      .style("color:#000080")
                      .text(flea.fmtModels2(F)),
                    Q("td")
                      .klass("number")
                      .text(fns.nFormat(F.assets, 2))
                  ]]);
                }));})
        ]);};

    wg
      .add(Q("table")
        .klass("border")
        .att("align", "center")
        .adds(mkRows(0, nRks2))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", nRks2 * 5)
            .add(Q("hr"))))
        .adds(mkRows(nRks2, nRks)))
    ;
  };

  
  function show ()  {sys.$params(arguments.length, 0);
    const menuWg = sys.$checkNull(menu.mk (
      [ menu.toption("last", II("Current"), last),
        menu.separator(),
        menu.toption("all", II("AllRankings"), all)
      ],
      [],
      MenuSel[0],
      false
    ));

    wg
      .removeAll()
      .add(menuWg)
    ;

    if (sys.asBool(sys.$eq(MenuSel[0] , "last"))) {
      showLast();
    } else {
      showAll();
    }
  };
  Show[0] =sys.$checkExists(Show[0], sys.$checkNull(show));

  show();
};
