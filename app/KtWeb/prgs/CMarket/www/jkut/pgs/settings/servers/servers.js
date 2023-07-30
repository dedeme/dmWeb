import * as iter from '../../../_js/iter.js';import * as str from '../../../_js/str.js';import * as bytes from '../../../_js/bytes.js';import * as cryp from '../../../_js/cryp.js';import * as dic from '../../../_js/dic.js';import * as timer from '../../../_js/timer.js';import * as js from '../../../_js/js.js';import * as storage from '../../../_js/storage.js';import * as sys from '../../../_js/sys.js';import * as math from '../../../_js/math.js';import * as domo from '../../../_js/domo.js';import * as ui from '../../../_js/ui.js';import * as arr from '../../../_js/arr.js';import * as time from '../../../_js/time.js';import * as client from '../../../_js/client.js';import * as b64 from '../../../_js/b64.js';




import * as cts from  "../../../data/cts.js";
import * as modalBox from  "../../../libdm/modalBox.js";
import * as vmenu from  "../../../libdm/vmenu.js";
import * as msg from  "../../../wgs/msg.js";
import * as i18n from  "../../../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);





export  async  function mk(wg, selected)  {sys.$params(arguments.length, 2);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    module: "Settings",
    source: "Servers",
    rq: "idata",
    selected: selected
  }));

  const Svs =sys.$checkNull( arr.map(Rp.servers, svDataFromJs));
  const daily =sys.$checkNull( Rp.dailyServer);
  const historic =sys.$checkNull( Rp.historicServer);
  const Codes =sys.$checkNull( Rp.codes);
  const SvSelected =sys.$checkNull( arr.find(Svs, function(Sv)  {sys.$params(arguments.length, 1);  return sys.$eq(Sv.id , selected);}));
  const Selected =sys.$checkNull(sys.asBool( SvSelected) ? [SvSelected[0].id] : [""]);
  const dailyActive =sys.$checkNull(sys.asBool( SvSelected) ? SvSelected[0].withDiary : false);
  const historicActive =sys.$checkNull(sys.asBool( SvSelected) ? SvSelected[0].withHistoric : false);

  const dailyTestImg =sys.$checkNull( Q("span"));
  const historicTestImg =sys.$checkNull( Q("span"));
  const CodeFields =sys.$checkNull( []);
  const TestSpans =sys.$checkNull( []);
  const msgWait =sys.$checkNull( Q("div"));
  const SetWait =sys.$checkNull( [[]]);

  const Show =sys.$checkNull( [[]]);

  

  
   function select(svId)  {sys.$params(arguments.length, 1); mk(wg, svId);};

   async  function testDiary() {sys.$params(arguments.length, 0);
    dailyTestImg
      .removeAll()
      .add(ui.img("wait.gif"))
    ;
    const Rp =sys.$checkNull( await  client.send({
      prg: cts.appName,
      module: "Settings",
      source: "Servers",
      rq: "testDiary",
      server: SvSelected[0].id
    }));
    if (sys.asBool(sys.asBool(Rp.withErrors) && sys.asBool(Rp.withWarnings)))
      msg.error(II("Errors and warnings found.<br>See log."), function(){sys.$params(arguments.length, 0);});
    else if (sys.asBool(Rp.withErrors))
      msg.error(II("Errors found.<br>See log."), function(){sys.$params(arguments.length, 0);});
    else if (sys.asBool(Rp.withWarnings))
      msg.error(II("Warnings found.<br>See log."), function(){sys.$params(arguments.length, 0);});
    else
      msg.ok(II("Test ok."), function(){sys.$params(arguments.length, 0);});

    dailyTestImg
      .removeAll()
      .add(ui.link(function(ev)  {sys.$params(arguments.length, 1); testDiary();})
        .add(ui.img("unknown")
        .style("vertical-align:top")))
    ;
  };

   function testHistoric() {sys.$params(arguments.length, 0);
    const NksCds =sys.$checkNull( dic.toArr(Codes));
    arr.sort(NksCds, function(e1, e2)  {sys.$params(arguments.length, 2);  return str.great(e1[0], e2[0]);});

    const WithErrors =sys.$checkNull( [false]);
    const WithWarnings =sys.$checkNull( [false]);
     async  function test2(NksCds)  {sys.$params(arguments.length, 1);
      if (sys.asBool(!sys.asBool(NksCds))) {
        SetWait[0]("");
        if (sys.asBool(sys.asBool(WithErrors[0]) && sys.asBool(WithWarnings[0])))
          msg.error(II("Errors and warnings found.<br>See log."), function(){sys.$params(arguments.length, 0);});
        else if (sys.asBool(WithErrors[0]))
          msg.error(II("Errors found.<br>See log."), function(){sys.$params(arguments.length, 0);});
        else if (sys.asBool(WithWarnings[0]))
          msg.error(II("Warnings found.<br>See log."), function(){sys.$params(arguments.length, 0);});
        else
          msg.ok(II("Test ok."), function(){sys.$params(arguments.length, 0);});
        return;
      }

      const NC =sys.$checkNull( arr.pop(NksCds));
      const nick =sys.$checkNull( NC[0]);
      const code =sys.$checkNull( NC[1]);
      SetWait[0](nick);
      const Rp =sys.$checkNull( await  client.send({
        prg: cts.appName,
        module: "Settings",
        source: "Servers",
        rq: "testCo",
        server: SvSelected[0].id,
        nick: nick,
        code: code
      }));
      WithErrors[0] ||=sys.$checkExists(WithErrors[0],sys.$checkNull( Rp.withErrors));
      WithWarnings[0] ||=sys.$checkExists(WithWarnings[0],sys.$checkNull( Rp.withWarnings));
      test2(NksCds);
    };
    test2(NksCds);
  };

  
   async  function testCo(span, nick, field)  {sys.$params(arguments.length, 3);
    const code =sys.$checkNull( str.trim(field.getValue()));
    if (sys.asBool(!sys.asBool(code))) {
      msg.error("Code is empty", function(){sys.$params(arguments.length, 0);});
      return;
    }

    span
      .removeAll()
      .add(ui.img("wait.gif"))
    ;
    const Rp =sys.$checkNull( await  client.send({
      prg: cts.appName,
      module: "Settings",
      source: "Servers",
      rq: "testCo",
      server: SvSelected[0].id,
      nick: nick,
      code: code
    }));
    if (sys.asBool(sys.asBool(Rp.withErrors) && sys.asBool(Rp.withWarnings)))
      msg.error(II("Errors and warnings found.<br>See log."), function(){sys.$params(arguments.length, 0);});
    else if (sys.asBool(Rp.withErrors))
      msg.error(II("Errors found.<br>See log."), function(){sys.$params(arguments.length, 0);});
    else if (sys.asBool(Rp.withWarnings))
      msg.error(II("Warnings found.<br>See log."), function(){sys.$params(arguments.length, 0);});
    else
      msg.ok(II("Test ok."), function(){sys.$params(arguments.length, 0);});

    span
      .removeAll()
      .add(ui.link(function(ev)  {sys.$params(arguments.length, 1); testCo(span, nick, field);})
        .add(ui.img("unknown")
          .style("vertical-align:top")))
    ;
  };

  
   function reset()  {sys.$params(arguments.length, 0); mk(wg, Selected[0]);};

  
   async  function modify()  {sys.$params(arguments.length, 0);
    
    const Codes =sys.$checkNull( {});
    for (let field  of sys.$forObject( CodeFields))
      dic.put(Codes, field.getAtt("id"), str.trim(field.getValue()));
    await client.send({
      prg: cts.appName,
      module: "Settings",
      source: "Servers",
      rq: "setCodes",
      server: Selected[0],
      codes: Codes
    });
    mk(wg, Selected[0]);
  };

  

  Show[0] =sys.$checkExists(Show[0], function()  {sys.$params(arguments.length, 0);
    const Opts =sys.$checkNull( [vmenu.title(II("Servers")), vmenu.separator()]);
    for (let sv  of sys.$forObject( Svs)) {
       function dailyWg() {sys.$params(arguments.length, 0); return sys.asBool( sv.withDiary)
        ?sys.asBool( sys.$eq(sv.id , daily))
          ? ui.img("star")
          : ui.led("#d0ddde", 6)
        : ui.img("stopped")
      ;};

       function historicWg()  {sys.$params(arguments.length, 0); return sys.asBool( sv.withHistoric)
        ?sys.asBool( sys.$eq(sv.id , historic))
          ? ui.img("star")
          : ui.led("#d0ddde", 6)
        : ui.img("stopped")
      ;};

      const normalWg =sys.$checkNull( Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("width:5px")
            .add(dailyWg().setStyle("vertical-align", "top")))
          .add(Q("td")
            .style("width:5px")
            .add(historicWg().setStyle("vertical-align", "top")))
          .add(Q("td")
            .add(ui.link(function(ev)  {sys.$params(arguments.length, 1); select(sv.id);})
            .klass("link")
            .setStyle("text-align", "left")
            .setStyle("padding-left", "4px")
            .text(sv.id)))))
      ;
      const selectWg =sys.$checkNull( Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("width:5px")
            .add(dailyWg().setStyle("vertical-align", "top")))
          .add(Q("td")
            .style("width:5px")
            .add(historicWg().setStyle("vertical-align", "top")))
          .add(Q("td")
            .add(Q("span")
              .style(
                  "padding-left:4px;" +
                  "text-align:left;" +
                  "font-style:italic;" +
                  "color:#803010"
                )
              .text(sv.id)))))
      ;

      arr.push(Opts, vmenu.mkEntry([sv.id], normalWg, selectWg));
    }

    const vmenuWg =sys.$checkNull( vmenu.mk(Opts, Selected[0]));

    const Rows =sys.$checkNull( []);
    const cols =sys.$checkNull( 4);
    if (sys.asBool(Selected[0])) {
      arr.push(
        Rows,
        Q("tr")
          .add(Q("td")
            .att("colspan", cols)
            .add(Q("div")
              .klass("head")
              .text(SvSelected[0].name)))
      );

      dailyTestImg
        .removeAll()
        .add(sys.asBool(dailyActive)
          ? ui.link(function(ev)  {sys.$params(arguments.length, 1); testDiary();})
              .add(ui.img("unknown")
                .style("vertical-align:top"))
          : ui.img("unknown2")
            .style("vertical-align:top"))
      ;
      historicTestImg
        .removeAll()
        .add(sys.asBool(historicActive)
          ? ui.link(function(ev)  {sys.$params(arguments.length, 1); testHistoric();})
              .add(ui.img("unknown")
                .style("vertical-align:top"))
          : ui.img("unknown2")
            .style("vertical-align:top"))
      ;

      arr.push(
        Rows,
        Q("tr")
          .add(Q("td")
            .att("colspan", cols)
            .style("padding-top:4px;padding-bottom: 4px;" +
                   "border-bottom: 1px solid rgb(110,130,150);")
            .add(Q("table")
              .att("align", "center")
              .add(Q("tr")
                .add(Q("td")
                  .style(
                      "white-space:nowrap;text-align:right;" +
                      "width:50%;padding-rigth:4px"
                    )
                  .add(Q("span")
                    .add(dailyTestImg)
                    .add(Q("span")
                      .style("padding-left:4px")
                      .text(II("Daily Test")))))
                .add(Q("td")
                  .style(
                      "white-space:nowrap;text-align:left;" +
                      "width:50%;padding-left:4px"
                    )
                  .add(Q("span")
                    .add(historicTestImg)
                    .add(Q("span")
                      .style("padding-left:4px")
                      .text(II("Historic Test"))))))))
      );

      arr.push(
        Rows,
        Q("tr")
          .add(Q("td")
            .att("colspan", cols)
            .style("padding-top:4px;padding-bottom: 4px;" +
                   "border-bottom: 1px solid rgb(110,130,150);" +
                   "text-align:right;")
            .add(Q("button")
              .text(II("Reset"))
              .on("click", function(e)  {sys.$params(arguments.length, 1); reset();}))
            .add(Q("span")
              .text(" "))
            .add(Q("button")
              .text(II("Modify"))
              .on("click", function(e)  {sys.$params(arguments.length, 1); modify();})))
      );

      const Tds =sys.$checkNull( []);
      const len =sys.$checkNull( dic.size(Codes));
      const Nicks =sys.$checkNull( dic.keys(Codes));
      arr.sort(Nicks, str.less);
      for (let i = 0;i < len; ++i) {
        const nk =sys.$checkNull( Nicks[i]);
        const i1 =sys.$checkNull( i + 1);
        const nextNk =sys.$checkNull(sys.asBool( i1 < len) ? Nicks[i1] : Nicks[0]);

        const field =sys.$checkNull( ui.field(nextNk)
          .att("id", nk)
          .setStyle("width", "125px")
          .value(Codes[nk]));
        CodeFields.push(field);

        const span =sys.$checkNull( Q("span")
          .add(sys.asBool(historicActive)
            ? ui.link(function(ev)  {sys.$params(arguments.length, 1); testCo(span, nk, field);})
              .add(ui.img("unknown")
                .style("vertical-align:top"))
            : ui.img("unknown2")
                .style("vertical-align:top")));
        TestSpans.push(span);

        Tds.push(Q("td")
          .style("text-align:right;white-space: nowrap;")
          .add(Q("span")
            .text(nk + ": "))
          .add(field)
          .add(span)
        );
      }

      const rowsLen =sys.$checkNull( Math.ceil(len / cols));
      if (sys.asBool(sys.$eq(rowsLen , 0))) {
        Rows.push(Q("tr").add(Q("td").text(II("Without Nicks"))));
      } else {
        for (let i = 0;i < rowsLen; ++i) {
          const RowTds =sys.$checkNull( []);
          for (let j = 0;j < cols; ++j) {
            const tdix =sys.$checkNull( i + j * rowsLen);
            RowTds.push(sys.asBool(
              tdix < len)
                ?sys.asBool( sys.$eq(j , 0))
                  ? Tds[tdix]
                  : Tds[tdix].setStyle("border-left", "1px solid rgb(110,130,150)")
                : Q("td").setStyle("border-left", "1px solid rgb(110,130,150)")
            );
          }
          Rows.push(Q("tr").adds(RowTds));
        }
      }
    }

    const body =sys.$checkNull( Q("table")
      .att("align", "center")
      .style("border-top: 1px solid rgb(110,130,150);" +
             "border-bottom: 1px solid rgb(110,130,150);" +
             "border-collapse: collapse;")
      .adds(Rows))
    ;

    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("width:5px;vertical-align:top")
            .add(vmenuWg))
          .add(Q("td")
            .add(body))))
      .add(msgWait)
    ;
  });

  
  SetWait[0] =sys.$checkExists(SetWait[0], function(nick)  {sys.$params(arguments.length, 1);
    msgWait.removeAll();

    if (sys.asBool(sys.$neq(nick , ""))) {
      const box =sys.$checkNull( modalBox.mk(
        Q("div")
          .add(Q("div")
            .style("text-align:center")
            .add(ui.img("wait2.gif").klass("frame")))
          .add(Q("div").style("text-align:center").html(nick)),
        false
      ));
      msgWait.add(box.wg);
      modalBox.show(box, true);
    }
  });

  Show[0]();
};



 function mkSvData(id, name, withDiary, withHistoric)  {sys.$params(arguments.length, 4);
   return {id:id, name:name, withDiary:withDiary, withHistoric:withHistoric};};


 function svDataFromJs(A)  {sys.$params(arguments.length, 1);  return mkSvData(A[0], A[1], A[2], A[3]);};
