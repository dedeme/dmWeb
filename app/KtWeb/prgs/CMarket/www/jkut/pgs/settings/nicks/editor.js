import * as iter from '../../../_js/iter.js';import * as str from '../../../_js/str.js';import * as bytes from '../../../_js/bytes.js';import * as cryp from '../../../_js/cryp.js';import * as dic from '../../../_js/dic.js';import * as timer from '../../../_js/timer.js';import * as js from '../../../_js/js.js';import * as storage from '../../../_js/storage.js';import * as sys from '../../../_js/sys.js';import * as math from '../../../_js/math.js';import * as domo from '../../../_js/domo.js';import * as ui from '../../../_js/ui.js';import * as arr from '../../../_js/arr.js';import * as time from '../../../_js/time.js';import * as client from '../../../_js/client.js';import * as b64 from '../../../_js/b64.js';




import * as menu from  "../../../libdm/menu.js";
import * as modalBox from  "../../../libdm/modalBox.js";
import * as cts from  "../../../data/cts.js";
import * as quote from  "../../../data/quote.js";
import * as msg from  "../../../wgs/msg.js";
import * as i18n from  "../../../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);

const UpdatingServer =sys.$checkNull( [false]);


export  async  function mk(wg, nicks, mainNick, nick)  {sys.$params(arguments.length, 4);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    module: "Settings",
    source: "Nicks/Editor",
    rq: "idata",
    mainNick: mainNick,
    nick: nick
  }));
  if (sys.asBool(!sys.asBool(Rp.ok))) {
    msg.error(cts.failMsg, function()  {sys.$params(arguments.length, 0);});
    return;
  }
  arr.sort(nicks, str.less);
  const Quotes =sys.$checkNull( arr.reverse(arr.map(Rp.quotes, quote.fromJs)));
  const manuals =sys.$checkNull( Rp.manuals);
  const Mquotes =sys.$checkNull( arr.reverse(arr.map(Rp.mquotes, quote.fromJs)));
  const SIdCodes =sys.$checkNull( arr.map(
    Rp.sIdCodes,
    
    function(A)  {sys.$params(arguments.length, 1);  return {id: A[0], withHistoric: A[1], code: A[2]};}
  ));

  const QEdit =sys.$checkNull( [[]]);
  const QCancel =sys.$checkNull( [[]]);
  const QModify =sys.$checkNull( [[]]);

  const serverTestSpan =sys.$checkNull( Q("span"));
  const editBt =sys.$checkNull( Q("button")
    .text(II("Edit"))
    .on("click", function(e)  {sys.$params(arguments.length, 1); QEdit[0]();}));
  const cancelBt =sys.$checkNull( Q("button")
    .text(II("Cancel"))
    .disabled(true)
    .on("click", function(e)  {sys.$params(arguments.length, 1); QCancel[0]();}));
  const modifyBt =sys.$checkNull( Q("button")
    .text(II("Modify"))
    .disabled(true)
    .on("click", function(e)  {sys.$params(arguments.length, 1); QModify[0]();}));
  const leftArea =sys.$checkNull( Q("textarea")
    .att("spellcheck", false)
    .att("rows", 25)
    .att("cols", 60)
    .disabled(true));
  const rightArea =sys.$checkNull( Q("textarea")
    .att("spellcheck", false)
    .att("rows", 25)
    .att("cols", 60)
    .disabled(true))
  ;
  const msgWait =sys.$checkNull( Q("div"));
  const SetWait =sys.$checkNull( [[]]);

  

  QEdit[0] =sys.$checkExists(QEdit[0], function() {sys.$params(arguments.length, 0);
    editBt.disabled(true);
    cancelBt.disabled(false);
    modifyBt.disabled(false);
    leftArea.disabled(false);
  });

  QCancel[0] =sys.$checkExists(QCancel[0], function() {sys.$params(arguments.length, 0);
    mk(wg, nicks, mainNick, nick);
  });

  QModify[0] =sys.$checkExists(QModify[0], async  function() {sys.$params(arguments.length, 0);
    const Rp =sys.$checkNull( await  client.send({
      prg: cts.appName,
      module: "Settings",
      source: "Nicks/Editor",
      rq: "qModify",
      mainNick: mainNick,
      nick: nick,
      qs: str.trim(leftArea.getValue())
    }));
    if (sys.asBool(Rp.ok)) {
      msg.ok(II("Quotes were successfully modified"), function(){sys.$params(arguments.length, 0);});
      mk(wg, nicks, mainNick, nick);
    } else {
      msg.error(II("No modification was performed.<br>See Log."), function(){sys.$params(arguments.length, 0);});
    }
  });

  
   async  function download()  {sys.$params(arguments.length, 0);
    SetWait[0](II("Downloading..."));
    const Rp =sys.$checkNull( await  client.send({
      prg: cts.appName,
      module: "Settings",
      source: "Nicks/Editor",
      rq: "download",
      mainNick: mainNick,
      nick: nick
    }));
    SetWait[0]("");

    switch (Rp.result) {
      case "error" :{ msg.error(II("Some error was found.<br>See Log."), function(){sys.$params(arguments.length, 0);});break;}
      case "warnings":{ msg.info(II("Some quote was modified.<br>See Log."), function(){sys.$params(arguments.length, 0);});break;}
      default:{ msg.ok(II("Download ok."), function(){sys.$params(arguments.length, 0);});}
    }
    mk(wg, nicks, mainNick, nick);
  };

  
   async  function test()  {sys.$params(arguments.length, 0);
    const Rp =sys.$checkNull( await  client.send({
      prg: cts.appName,
      module: "Settings",
      source: "Nicks/Editor",
      rq: "test",
      mainNick: mainNick,
      nick: nick,
      qs: str.trim(leftArea.getValue())
    }));
    switch (Rp.result) {
      case "error" :{ msg.error(II("An error was found.<br>See Log."), function(){sys.$params(arguments.length, 0);});break;}
      case "warnings":{ msg.info(II("Some warnings were found.<br>See Log."), function(){sys.$params(arguments.length, 0);});break;}
      default:{ msg.ok(II("Test ok."), function(){sys.$params(arguments.length, 0);});}
    }
  };

  
   async  function updateCode(sId, code)  {sys.$params(arguments.length, 2);
    if (sys.asBool(sys.$eq(code , "")))
      msg.error(i18n.fmt(II("Nick code of %0 is missing"), [sId]), function()  {sys.$params(arguments.length, 0);});
    else
      UpdatingServer[0] =sys.$checkExists(UpdatingServer[0],sys.$checkNull( true));
      await client.send({
        prg: cts.appName,
        module: "Settings",
        source: "Nicks/Editor",
        rq: "updateCode",
        nick: nick,
        server: sId,
        code: code
      });
      mk(wg, nicks, mainNick, nick);
  };

  
   async  function serverTests() {sys.$params(arguments.length, 0);
    await timer.delay(timer.mk(100), function()  {sys.$params(arguments.length, 0);});
    if (sys.asBool(UpdatingServer[0])) {
      ui.alert("Updating server. Try again.");
      UpdatingServer[0] =sys.$checkExists(UpdatingServer[0],sys.$checkNull( false));
      return;
    }

    const WithErrors =sys.$checkNull( [false]);
    const WithWarnings =sys.$checkNull( [false]);
     async  function test(SICs)  {sys.$params(arguments.length, 1);
      if (sys.asBool(!sys.asBool(SICs))) {
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

      const SIC =sys.$checkNull( arr.pop(SICs));
      if (sys.asBool(SIC.withHistoric)) {
        SetWait[0](SIC.id);
        const Rp =sys.$checkNull( await  client.send({
          prg: cts.appName,
          module: "Settings",
          source: "Nicks/Editor",
          rq: "serverTest",
          nick: nick,
          server: SIC.id,
          code: SIC.code
        }));
        WithErrors[0] ||=sys.$checkExists(WithErrors[0],sys.$checkNull( Rp.withErrors));
        WithWarnings[0] ||=sys.$checkExists(WithWarnings[0],sys.$checkNull( Rp.withWarnings));
      }
      test(SICs);
    };
    test(arr.reverse(SIdCodes));
  };

  
   async  function setRightArea(nick)  {sys.$params(arguments.length, 1);
    const Rp =sys.$checkNull( await  client.send({
      prg: cts.appName,
      module: "Settings",
      source: "Nicks/Editor",
      rq: "getQuotes",
      nick: nick
    }));
    if (sys.asBool(!sys.asBool(Rp.ok))) {
      msg.error(i18n.fmt(
        II("Quotes of %0 can not be loaded.<br>See Log."), [nick]
      ));
      mk(wg, nicks, mainNick, nick);
    }
    const Qs =sys.$checkNull( arr.reverse(arr.map(Rp.quotes, quote.fromJs)));
    rightArea.text(arr.join(arr.map(Qs, quote.toStr), "\n"));
  };

  

  
   function serversDiv()  {sys.$params(arguments.length, 0);
    
     function svTd(S)  {sys.$params(arguments.length, 1);
      const color =sys.$checkNull(sys.asBool( S.withHistoric) ? "#000000" : "#909090");
      const field =sys.$checkNull( Q("input")
        .att("type", "text")
        .style("width:125px")
        .value(S.code));
      field.on("change", function(e)  {sys.$params(arguments.length, 1); updateCode(S.id, str.trim(field.getValue()));});

       return Q("td").add(Q("table")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center;color:" + color)
            .text(S.id)))
        .add(Q("tr")
          .add(Q("td")
            .add(field))))
      ;
    };

    arr.sort(SIdCodes, function(S1, S2)  {sys.$params(arguments.length, 2);  return S1.id < S2.id;});
    const sz =sys.$checkNull( arr.size(SIdCodes));
    const cols =sys.$checkNull( 4);
    const rows =sys.$checkNull( Math.ceil(sz / cols));
    const trs =sys.$checkNull( []); 
    for (let r = 0;r < rows; ++r) {
      const tr =sys.$checkNull( Q("tr"));
      for (let c = 0;c < cols; ++c) {
        const i =sys.$checkNull( r * cols + c);
        tr.add(sys.asBool(i < sz)
          ? svTd(SIdCodes[i])
          : Q("td")
        );
      }
      arr.push(trs, tr);
    }

     return Q("div")
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .style("vertical-align:middle")
            .add(ui.link(function(e)  {sys.$params(arguments.length, 1); serverTests();})
              .klass("link")
              .text(II("Test"))))
          .add(Q("td"))
          .add(Q("td")
            .add(serverTestSpan
              .removeAll()
              .add(ui.img("unknown"))))))
      .add(Q("table")
        .klass("frame")
        .att("align", "center")
        .adds(trs))
    ;
  };

  
   function leftMenu()  {sys.$params(arguments.length, 0);
    const lopts =sys.$checkNull( [
      menu.mkEntry([], editBt)
    ]);
    const ropts =sys.$checkNull( [
      menu.mkEntry([], cancelBt),
      menu.mkEntry([], Q("span").html("&nbsp;")),
      menu.mkEntry([], modifyBt)
    ]);
     return menu.mk(lopts, ropts, "", false);
  };

  
   function rightMenu()  {sys.$params(arguments.length, 0);
    const sel =sys.$checkNull( ui.select("nks", arr.map(
      nicks, function(n)  {sys.$params(arguments.length, 1);  return (sys.asBool(sys.$eq(n , mainNick)) ? "+" : "") + n;}
    )));
    const selEl =sys.$checkNull( sel.e);
    sel.on("change", function(e)  {sys.$params(arguments.length, 1); setRightArea(nicks[selEl.selectedIndex]);});
    const lopts =sys.$checkNull( [
      menu.mkEntry([], sel)
    ]);
     return menu.mk(lopts, [], "", false);
  };

  
   function textAreaHeader()  {sys.$params(arguments.length, 0);  return Q("td")
      .klass("frame")
      .text(
        II("Date") + ":" +
        II("Open") + ":" +
        II("CloseN") + ":" +
        II("Max") + ":" +
        II("Min") + ":" +
        II("Vol") + ":" +
        II("State")
      )
    ;};

  
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

  wg
    .removeAll()
    .add(msgWait)
    .add(Q("table")
      .klass("main")
      .add(Q("tr")
        .add(Q("td")
          .style("font-size: 24px;")
          .text(str.fmt("%v [%v](%v)", [nick, arr.size(Quotes), manuals])))
        .add(Q("td")
          .style("text-align:right")
          .add(ui.link(function(ev)  {sys.$params(arguments.length, 1); download();})
            .klass("link")
            .text(II("Download")))
          .add(Q("span").text(" · "))
          .add(ui.link(function(ev)  {sys.$params(arguments.length, 1); test();})
            .klass("link")
            .text(II("Test")))))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", 2)
          .add(Q("hr"))))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", 2)
          .add(ui.hrule(II("Servers"), 50))))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", 2)
          .add(serversDiv())))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", 2)
          .add(ui.hrule(II("Quotes"), 50))))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", 2)
        .add(Q("table").att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .add(Q("table")
                .style("width:100%")
                .add(Q("tr")
                  .add(Q("td")
                    .add(leftMenu())))
                .add(Q("tr")
                  .add(textAreaHeader()))
                .add(Q("tr")
                  .add(Q("td")
                    .add(leftArea
                      .text(arr.join(arr.map(Quotes, quote.toStr), "\n")))))))
            .add(Q("td")
              .add(Q("table")
                .style("width:100%")
                .add(Q("tr")
                  .add(Q("td")
                    .add(rightMenu())))
                .add(Q("tr")
                  .add(textAreaHeader()))
                .add(Q("tr")
                  .add(Q("td")
                    .add(rightArea
                      .text(arr.join(arr.map(Mquotes, quote.toStr), "\n")))))))))))
  );

};
