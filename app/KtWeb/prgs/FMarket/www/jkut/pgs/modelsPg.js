import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as menu from  "../libdm/menu.js";
import * as i18n from  "../i18n.js";
import * as mdStats from  "../data/mdStats.js";
import * as cts from  "../data/cts.js";
import * as fns from  "../data/fns.js";
import * as flea from  "../data/flea.js";

const Q = sys.$checkNull(ui.q);
const II = sys.$checkNull(i18n.tlt);


export async function mk (wg)  {sys.$params(arguments.length, 1);
  const Rp = sys.$checkNull(await  client.send({
    prg: cts.appName,
    source: "ModelsPg",
    rq: "idata"
  }));
  const ModelIds = sys.$checkNull(Rp.modelIds);
  const MdStats = sys.$checkNull(mdStats.fromJs(Rp.mdStats));

  const MenuSel = sys.$checkNull(["ranking"]);

  const Show = sys.$checkNull([[]]);



  
  function ranking ()  {sys.$params(arguments.length, 0);
    MenuSel[0] =sys.$checkExists(MenuSel[0], sys.$checkNull("ranking"));
    Show[0]();
  };

  
  function statistics (modelId)  {sys.$params(arguments.length, 1);
    MenuSel[0] =sys.$checkExists(MenuSel[0], sys.$checkNull(modelId));
    Show[0]();
  };



  
  function showRanking ()  {sys.$params(arguments.length, 0);
    wg
      .add(Q("table")
        .klass("border")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .text(II("Models")))
          .add(Q("td")
            .klass("header")
            .style("text-align: right")
            .text(II("Duplicates"))))
        .adds(arr.map(MdStats.GroupsRanking, function(Gr)  {sys.$params(arguments.length, 1);
           return Q("tr")
            .add(Q("td")
              .klass("border")
              .style("text-align: left;font-family:monospace;white-space:pre")
              .text(mdStats.groupFormatIds(Gr)))
            .add(Q("td")
              .klass("border")
              .style("text-align: right")
              .text(fns.nFormat(Gr.duplicates, 0)))
        ;})))
    ;};

  
  function showStatistics ()  {sys.$params(arguments.length, 0);
    
    function bestsWorstsWg (Ofs)  {sys.$params(arguments.length, 1);
      
      function mkRow  (Of)  {sys.$params(arguments.length, 1);
        const F = sys.$checkNull(Of.Flea);
        
        function mkModelsWg ()  {sys.$params(arguments.length, 0);
           return arr.map(flea.fmtModels(F), function(fmt)  {sys.$params(arguments.length, 1);
             return Q("td")
              .klass("borderWhite")
              .style("text-align:left;font-family:monospace;white-space:pre")
              .text(fmt)
            ;}
          );};

         return Q("tr")
          .add(Q("td")
            .klass("number")
            .text(fns.nFormat(Of.order + 1, 0)))
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

      const Trs = sys.$checkNull([]);
      for (let Of  of sys.$forObject( Ofs)) arr.push(Trs, mkRow(Of));

       return Q("table")
        .klass("border")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .style("text-align: right")
            .text(II("Pos.")))
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
        .adds(Trs)
      ;
    };

    const Mds = sys.$checkNull(MdStats.Models);
    const Md = sys.$checkNull(Mds[arr.index(Mds, function(M)  {sys.$params(arguments.length, 1);  return sys.$eq(M.modelId , MenuSel[0]);})]);
    wg
      .add(Q("table")
        .klass("border")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("rlabel")
            .text(II("Fleas number")))
          .add(Q("td")
            .klass("number")
            .text(fns.nFormat(Md.nfleas, 0))))
        .add(Q("tr")
          .add(Q("td")
            .klass("rlabel")
            .text(II("Position average")))
          .add(Q("td")
            .klass("number")
            .text(fns.nFormat(Md.position, 0))))
        .add(Q("tr")
          .add(Q("td")
            .klass("rlabel")
            .text(II("Assets average")))
          .add(Q("td")
            .klass("number")
            .text(fns.nFormat(Md.assets, 2)))))
      .add(Q("div")
        .klass("head")
        .text(II("Bests")))
      .add(bestsWorstsWg(Md.Bests))
      .add(Q("div")
        .klass("head")
        .text(II("Worsts")))
      .add(bestsWorstsWg(Md.Worsts))
    ;
  };

  
  function show ()  {sys.$params(arguments.length, 0);
    const MenuLeft = sys.$checkNull(arr.reduce(
      ModelIds,
      [menu.toption("ranking", II("Ranking"), ranking)],
      function(R, mId)  {sys.$params(arguments.length, 2);
        arr.push(R, menu.separator());
        arr.push(R, menu.toption(mId, mId, function()  {sys.$params(arguments.length, 0); statistics(mId);}));
         return R;
      }
    ));
    MenuLeft[1] =sys.$checkExists(MenuLeft[1], sys.$checkNull(menu.separator2()));

    const menuWg = sys.$checkNull(menu.mk(
      MenuLeft,
      [],
      MenuSel[0],
      false
    ));

    wg
      .removeAll()
      .add(menuWg)
    ;

    if (sys.asBool(sys.$eq(MenuSel[0] , "ranking"))) showRanking();
    else showStatistics();
  };
  Show[0] =sys.$checkExists(Show[0], sys.$checkNull(show));

  show();
};
