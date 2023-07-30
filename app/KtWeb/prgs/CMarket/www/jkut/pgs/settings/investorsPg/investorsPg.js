import * as iter from '../../../_js/iter.js';import * as str from '../../../_js/str.js';import * as bytes from '../../../_js/bytes.js';import * as cryp from '../../../_js/cryp.js';import * as dic from '../../../_js/dic.js';import * as timer from '../../../_js/timer.js';import * as js from '../../../_js/js.js';import * as storage from '../../../_js/storage.js';import * as sys from '../../../_js/sys.js';import * as math from '../../../_js/math.js';import * as domo from '../../../_js/domo.js';import * as ui from '../../../_js/ui.js';import * as arr from '../../../_js/arr.js';import * as time from '../../../_js/time.js';import * as client from '../../../_js/client.js';import * as b64 from '../../../_js/b64.js';




import * as modalBox from  "../../../libdm/modalBox.js";
import * as menu from  "../../../libdm/menu.js";
import * as msg from  "../../../wgs/msg.js";
import * as cts from  "../../../data/cts.js";
import * as model from  "../../../data/model.js";
import * as investor from  "../../../data/investor/investor.js";
import * as istrategy from  "../../../data/investor/istrategy.js";
import * as fns from  "../../../data/fns.js";
import * as i18n from  "../../../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);





export  async  function mk(wg, investorIx)  {sys.$params(arguments.length, 2);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    module: "Settings",
    source: "InvestorsPg",
    rq: "idata",
    investorIx:investorIx
  }));
  if (sys.asBool(!sys.asBool(Rp.ok))) {
    msg.error(cts.failMsg, function()  {sys.$params(arguments.length, 0);});
    return;
  }
  const Models =sys.$checkNull( arr.map(Rp.models, model.fromJs));
  const Investor =sys.$checkNull( investor.fromJs(Rp.investor));
  const investors =sys.$checkNull( Rp.investors);

  const sinvestor =sys.$checkNull( II("Inv") + "-" + investorIx);
  const waitBox =sys.$checkNull( modalBox.mk(ui.img("wait2.gif"), false));
  const editorDiv =sys.$checkNull( Q("div"));
  const paramsDiv =sys.$checkNull( Q("div"));

  const EditorView =sys.$checkNull( [[]]);

  

  
   function selInvestor(inv)  {sys.$params(arguments.length, 1);
    mk(wg, inv);
  };

  
   async  function update()  {sys.$params(arguments.length, 0);
    modalBox.show(waitBox, true);
    const Rp =sys.$checkNull( await  client.send({
      prg: cts.appName,
      module: "Settings",
      source: "InvestorsPg",
      rq: "updateAll"
    }));
    modalBox.show(waitBox, false);
    if (sys.asBool(!sys.asBool(Rp.ok))) {
      msg.error(cts.failMsg, function()  {sys.$params(arguments.length, 0);});
      return;
    }
    mk(wg, investorIx);
  };

  
   function edit(nick)  {sys.$params(arguments.length, 1);
    EditorView[0](nick);
    window.scroll(0, 0);
  };

  
   function cancel()  {sys.$params(arguments.length, 0); editorDiv.removeAll();};

  
   async  function accept(nickName, modelId, Params)  {sys.$params(arguments.length, 3);
    await client.send({
      prg: cts.appName,
      module: "Settings",
      source: "InvestorsPg",
      rq: "update",
      investorIx:investorIx,
      nickName:nickName,
      modelId:modelId,
      params: Params
    });
    mk(wg, investorIx);
  };
  

  
   function paramsView(nick, modelId, Params)  {sys.$params(arguments.length, 3);
    const Md =sys.$checkNull( arr.find(Models, function(md)  {sys.$params(arguments.length, 1);  return sys.$eq(md.id , modelId);})[0]);
    const sel =sys.$checkNull( Q("select"));
    for (let M  of sys.$forObject( Models)) {
      const op =sys.$checkNull( Q("option").text(M.id));
      if (sys.asBool(sys.$eq(M.id , Md.id))) op.att("selected", true);
      sel.e.add(op.e);
    }
    sel.on(
      "change",
      function(e)  {sys.$params(arguments.length, 1);
          const Md =sys.$checkNull( Models[sel.e.selectedIndex]);
          paramsView(nick, Md.id, Md.paramBases);
        }
    );

    const paramsWg =sys.$checkNull( Q("table").klass("frame")
      .add(Q("tr")
        .adds(arr.fromIter(iter.map(
          iter.$range(0,arr.size(Md.paramNames)),
          function(i)  {sys.$params(arguments.length, 1);  return Q("td")
            .add(Q("div")
              .add(Q("div")
                .style("text-align:center")
                .text(Md.paramNames[i]))
              .add(Q("div")
                .style("text-align:center;color:#c9c9c9;font-style:italic")
                .text(math.toIso(Md.paramBases[i], 6)))
              .add(function() {sys.$params(arguments.length, 0);
                  const inp =sys.$checkNull( ui.field("Zord" + (i + 1))
                    .att("id", "Zord" + i)
                    .style("width:80px")
                    .on("change", function(e)  {sys.$params(arguments.length, 1);
                        const V =sys.$checkNull( math.fromIso(e.target.value));
                        if (sys.asBool(!sys.asBool(V))) {
                          ui.alert(i18n.fmt(
                            II("Bad number [%0]"), [e.target.value]
                          ));
                          paramsView(nick, modelId, Params);
                          return;
                        }
                        const v =sys.$checkNull( V[0]);
                        const min =sys.$checkNull( Md.paramBases[i]);
                        const max =sys.$checkNull( model.maxs(Md)[i]);
                        if (sys.asBool(sys.asBool(v > max) || sys.asBool(v < min))) {
                          ui.alert(i18n.fmt(
                            II("[%0] Value out of range"), [e.target.value]
                          ));
                          paramsView(nick, modelId, Params);
                          return;
                        }
                        Params[i] =sys.$checkExists(Params[i],sys.$checkNull( v));
                      })
                    .value(math.toIso(Params[i], 6)))
                  ;
                  ui.changePoint(inp);
                   return inp;
                }())
              .add(Q("div")
                .style("text-align:center;color:#c9c9c9;font-style:italic")
                .text(math.toIso(model.maxs(Md)[i], 6))))
          ;})))))
    ;

    paramsDiv
      .removeAll()
      .add(Q("table")
        .add(Q("tr")
          .add(Q("td")
            .add(sel))
          .add(Q("td")
            .style("text-align:right")
            .add(ui.link(function(e)  {sys.$params(arguments.length, 1);
                paramsView(
                  nick,
                  Investor.base.modelId,
                  Investor.base.params
                )
              ;}).klass("link")
              .text(II("Default Model")))))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .att("align", "center")
            .add(paramsWg)))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .style("text-align:center")
            .add(Q("button")
              .text(II("Cancel"))
              .on("click", function(e)  {sys.$params(arguments.length, 1); cancel();}))
            .add(Q("span").text(" "))
            .add(Q("button")
              .att("id", "Zord" + arr.size(Md.paramNames))
              .text(II("Accept"))
              .on("click", function(e)  {sys.$params(arguments.length, 1);
                accept(nick, modelId, Params);
              })))))
    ;
  };

  
  EditorView[0] =sys.$checkExists(EditorView[0], function(nick)  {sys.$params(arguments.length, 1);
    const Str =sys.$checkNull(sys.asBool( sys.$eq(nick , "")) ? Investor.base : Investor.nicks[nick]);
    const Md =sys.$checkNull( arr.find(Models, function(md)  {sys.$params(arguments.length, 1);  return sys.$eq(md.id , Str.modelId);})[0]);
    paramsView(nick, Md.id, Str.params);

    editorDiv
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(sys.asBool(sys.$eq(nick , ""))
          ? II("Default Model")
          : i18n.fmt(II("%0 Model"), [nick])
        ))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .add(paramsDiv))))
      .add(Q("hr"))
    ;
  });

  const Lopts =sys.$checkNull( []);
  for (let i = 0;i < investors; ++i) {
    const lb =sys.$checkNull( II("Inv") + "-" + i);
    if (sys.asBool(i > 0)) arr.push(Lopts, menu.separator());
    arr.push(Lopts, menu.toption(lb, lb, function()  {sys.$params(arguments.length, 0); selInvestor(i);}));
  }
  const Ropts =sys.$checkNull( [menu.toption("update", II("Update"), update)]);
  const menuWg =sys.$checkNull( menu.mk(Lopts, Ropts, sinvestor, []));

  const Base =sys.$checkNull( Investor.base);
  const Md =sys.$checkNull( arr.find(Models, function(md)  {sys.$params(arguments.length, 1);  return sys.$eq(md.id , Base.modelId);})[0]);
  const NickStrs =sys.$checkNull( dic.toArr(Investor.nicks));
  arr.sort(NickStrs, function(N1, N2)  {sys.$params(arguments.length, 2);  return N1[0] < N2[0];});
  const maxNParams =sys.$checkNull( arr.reduce(
    NickStrs, 0,
    function(r, S)  {sys.$params(arguments.length, 2); return sys.asBool( arr.size(S[1].params) > r) ? arr.size(S[1].params) : r;}
  ));
  wg
    .removeAll()
    .add(waitBox.wg)
    .add(menuWg)
    .add(editorDiv)
      .add(Q("div")
        .klass("head")
        .html(II("Default Model")))
      .add(Q("table")
        .klass("white")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .html(II("Model")))
          .adds(arr.map(Md.paramNames, function(n)  {sys.$params(arguments.length, 1);  return Q("td")
              .klass("header")
              .html(n)
            ;})))
        .add(Q("tr")
          .add(Q("td")
            .klass("border")
            .add(ui.link(function(e)  {sys.$params(arguments.length, 1); edit("");})
              .klass("link")
              .html(Md.id)))
          .adds(arr.fromIter(iter.map(
            iter.$range(0,arr.size(Base.params)),
            function(ix)  {sys.$params(arguments.length, 1);  return Q("td")
              .klass("number")
              .text(fns.paramFmt(
                  Md.paramTypes[ix],
                  Base.params[ix]
                ))
            ;})))))
      .add(Q("div")
        .klass("head")
        .html(II("Nick models")))
      .add(Q("table")
        .klass("white")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .html(II("Nick")))
          .add(Q("td")
            .klass("header")
            .html(II("Model")))
          .adds(arr.fromIter(iter.map(
            iter.$range(0,maxNParams),
            function(n)  {sys.$params(arguments.length, 1);  return Q("td")
              .klass("header")
              .html("P. " + (n + 1))
            ;})))
          .add(Q("td")
            .klass("header")
            .text("·")))
        .adds(arr.map(NickStrs, function(NS)  {sys.$params(arguments.length, 1);
          const nk =sys.$checkNull( NS[0]);
          const Str =sys.$checkNull( NS[1]);
          const Md =sys.$checkNull( arr.find(Models, function(md)  {sys.$params(arguments.length, 1);  return sys.$eq(md.id , Str.modelId);})[0]);
           return Q("tr")
            .add(Q("td")
              .klass("border")
              .text(nk))
            .add(Q("td")
              .klass("border")
              .add(ui.link(function(e)  {sys.$params(arguments.length, 1); edit(nk);})
                .klass("link")
                .text(Md.id)))
            .adds(arr.fromIter(iter.map(
              iter.$range(0,maxNParams),
              function(ix)  {sys.$params(arguments.length, 1); return sys.asBool(
                ix >= arr.size(Str.params))
                  ? Q("td")
                    .klass("border")
                  : Q("td")
                    .klass("number")
                    .text(fns.paramFmt(Md.paramTypes[ix], Str.params[ix]))
              ;})))
            .add(Q("td")
              .klass("border")
              .add(ui.img(sys.asBool(istrategy.eq(Base, Str)) ? "blank" : "warning")))
          ;
        })))
  ;
};
