import * as iter from '../../../_js/iter.js';import * as str from '../../../_js/str.js';import * as bytes from '../../../_js/bytes.js';import * as cryp from '../../../_js/cryp.js';import * as dic from '../../../_js/dic.js';import * as timer from '../../../_js/timer.js';import * as js from '../../../_js/js.js';import * as storage from '../../../_js/storage.js';import * as sys from '../../../_js/sys.js';import * as math from '../../../_js/math.js';import * as domo from '../../../_js/domo.js';import * as ui from '../../../_js/ui.js';import * as arr from '../../../_js/arr.js';import * as time from '../../../_js/time.js';import * as client from '../../../_js/client.js';import * as b64 from '../../../_js/b64.js';




import * as menu from  "../../../libdm/menu.js";
import * as modalBox from  "../../../libdm/modalBox.js";
import * as cts from  "../../../data/cts.js";
import * as co from  "../../../data/co.js";
import * as msg from  "../../../wgs/msg.js";
import * as editor from  "../../../pgs/settings/nicks/editor.js";
import * as i18n from  "../../../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);




export  async  function mk(wg)  {sys.$params(arguments.length, 1);
  const msgWait =sys.$checkNull( Q("div"));

  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    module: "Settings",
    source: "Nicks",
    rq: "idata"
  }));
  const Main =sys.$checkNull( [Rp.main]); 
  const Cos =sys.$checkNull( arr.map(Rp.cos, co.fromJs));
  const volumes =sys.$checkNull( Rp.vol);

  const Option =sys.$checkNull(["*v"]);
  const cosSize =sys.$checkNull( arr.size(Cos));

  const selectedsSize =sys.$checkNull( arr.reduce(Cos, 0, function(r, c)  {sys.$params(arguments.length, 2); return sys.asBool( c.isSelected) ? r + 1 : r;}));
  const stats =sys.$checkNull( Q("span")
    .text(i18n.fmt(
        II("Total: %0. Selected: %1."),
        [cosSize, selectedsSize])))
  ;
  const SetWait =sys.$checkNull( [[]]);
  const Show =sys.$checkNull( [[]]);

  

   function showVolume()  {sys.$params(arguments.length, 0);
    Option[0] =sys.$checkExists(Option[0],sys.$checkNull( "*v"));
    Show[0]();
  };

   function showList()  {sys.$params(arguments.length, 0);
    Option[0] =sys.$checkExists(Option[0],sys.$checkNull( "*l"));
    Show[0]();
  };

   async  function setMain(nk)  {sys.$params(arguments.length, 1);
    await client.send({
      prg: cts.appName,
      module: "Settings",
      source: "Nicks",
      rq: "setMain",
      nick: nk
    });
    Main[0] =sys.$checkExists(Main[0],sys.$checkNull( nk));
    Show[0]();
  };

   async  function select(nk)  {sys.$params(arguments.length, 1);
    const C =sys.$checkNull( arr.find(Cos, function(C)  {sys.$params(arguments.length, 1);  return sys.$eq(C.nick , nk);}));
    if (sys.asBool(C)) {
      await client.send({
        prg: cts.appName,
        module: "Settings",
        source: "Nicks",
        rq: "select",
        nick: nk,
        value: !sys.asBool(C[0].isSelected)
      });

      C[0].isSelected =sys.$checkExists(C[0].isSelected,sys.$checkNull( !sys.asBool(C[0].isSelected)));
      const selectedsSize =sys.$checkNull(
        arr.reduce(Cos, 0, function(r, c)  {sys.$params(arguments.length, 2); return sys.asBool( c.isSelected) ? r + 1 : r;}));
      stats
        .removeAll()
        .text(i18n.fmt(
            II("Total: %0. Selected: %1."),
            [cosSize, selectedsSize]))
      ;
      Show[0]();
    }
  };

   function edit(nk)  {sys.$params(arguments.length, 1);
    Option[0] =sys.$checkExists(Option[0],sys.$checkNull( nk));
    Show[0]();
  };

   function download()  {sys.$params(arguments.length, 0);
    const WithErrors =sys.$checkNull( [false]);
    const WithWarnings =sys.$checkNull( [false]);
     async  function download2(Nks)  {sys.$params(arguments.length, 1);
      if (sys.asBool(!sys.asBool(Nks))) {
        SetWait[0]("");
        if (sys.asBool(sys.asBool(WithErrors[0]) && sys.asBool(WithWarnings[0])))
          msg.error(II("Errors and warnings found.<br>See log."), function(){sys.$params(arguments.length, 0);});
        else if (sys.asBool(WithErrors[0]))
          msg.error(II("Errors found.<br>See log."), function(){sys.$params(arguments.length, 0);});
        else if (sys.asBool(WithWarnings[0]))
          msg.error(II("Warnings found.<br>See log."), function(){sys.$params(arguments.length, 0);});
        else
          msg.ok(II("Download ok."), function(){sys.$params(arguments.length, 0);});
        return;
      }
      const nk =sys.$checkNull( arr.pop(Nks));
      SetWait[0](nk);
      const Rp =sys.$checkNull( await  client.send({
        prg: cts.appName,
        module: "Settings",
        source: "Nicks/Editor", 
        rq: "download",
        mainNick: Main[0],
        nick: nk
      }));
      if (sys.asBool(sys.$eq(Rp.result , "error"))) WithErrors[0] =sys.$checkExists(WithErrors[0],sys.$checkNull( true));
      else if (sys.asBool(sys.$eq(Rp.result , "warnings"))) WithWarnings[0] =sys.$checkExists(WithWarnings[0],sys.$checkNull( true));
      download2(Nks);
    };
    const Nicks =sys.$checkNull( arr.filter(
      arr.map(Cos, function(C)  {sys.$params(arguments.length, 1);  return C.nick;}),
      function(nk)  {sys.$params(arguments.length, 1);  return sys.$neq(nk , Main[0]);}
    ));
    arr.unshift(Nicks, Main[0]);
    download2(arr.reverse(Nicks));
  };

   function test()  {sys.$params(arguments.length, 0);
    const WithErrors =sys.$checkNull( [false]);
    const WithWarnings =sys.$checkNull( [false]);
     async  function test2(Cs)  {sys.$params(arguments.length, 1);
      if (sys.asBool(!sys.asBool(Cs))) {
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

      const C =sys.$checkNull( arr.pop(Cs));
      SetWait[0](C.nick);
      const Rp =sys.$checkNull( await  client.send({
        prg: cts.appName,
        module: "Settings",
        source: "Nicks",
        rq: "test",
        nick: C.nick
      }));
      WithErrors[0] ||=sys.$checkExists(WithErrors[0],sys.$checkNull( Rp.withErrors));
      WithWarnings[0] ||=sys.$checkExists(WithWarnings[0],sys.$checkNull( Rp.withWarnings));
      test2(Cs);
    };
    test2(arr.reverse(Cos));
  };

  

   function coTd(co)  {sys.$params(arguments.length, 1);  return Q("td")
      .style(
          "border-left: 1px solid rgb(110,130,150);" +
          "border-right: 1px solid rgb(110,130,150);" +
          "width:100px;white-space:nowrap;"
        )
      .add(Q("table")
        .add(Q("tr")
          .add(Q("td")
            .att("title", II("Main"))
            .add(ui.link(function(ev)  {sys.$params(arguments.length, 1); setMain(co.nick);})
              .add(ui.img(sys.asBool(sys.$eq(Main[0] , co.nick)) ? "star" : "star2"))))
          .add(Q("td")
            .att("title", II("Selection"))
            .add(ui.link(function(ev)  {sys.$params(arguments.length, 1); select(co.nick);})
              .add(sys.asBool(co.isSelected)
                  ? ui.img(sys.asBool(volumes[co.nick] < cts.trading) ? "flag2" : "flag1")
                  : ui.led("#d0ddde", 6).setStyle("padding", "5px")
                )))
          .add(Q("td")
            .att(
                "title",sys.asBool(
                sys.$eq(i18n.getLang() , "es"))
                  ? math.toIso(volumes[co.nick], 0)
                  : math.toEn(volumes[co.nick], 0)
              )
            .add(ui.link(function(ev) {sys.$params(arguments.length, 1); edit(co.nick);})
              .klass("link")
              .text(co.nick)))))
    ;};

   function list()  {sys.$params(arguments.length, 0);
    if (sys.asBool(sys.$eq(Option[0] , "*l"))) arr.sort(Cos, function(C1, C2)  {sys.$params(arguments.length, 2);  return C1.nick < C2.nick;});
    else arr.sort(Cos, function(C1, C2)  {sys.$params(arguments.length, 2);  return volumes[C1.nick] > volumes[C2.nick];});

    const Rows =sys.$checkNull( []);
    const ncols =sys.$checkNull( 6);
    const nrows =sys.$checkNull( math.toInt(Math.ceil(cosSize / ncols)));
    for (let i = 0;i < nrows; ++i) {
      const Tds =sys.$checkNull( []);
      for (let j = 0;j < ncols; ++j) {
        const ico =sys.$checkNull( j * nrows + i);
        arr.push(
          Tds,sys.asBool(
          ico >= cosSize)
            ? Q("td")
            : coTd(Cos[ico])
        );
      }
      arr.push(
        Rows,
        Q("tr")
          .adds(Tds)
      );
    }

     return Q("table")
      .klass("main")
      .add(Q("tr")
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
          .add(Q("hr"))))
      .add(Q("tr")
        .add(Q("td")
          .add(Q("table")
            .klass("white")
            .att("align", "center")
            .adds(Rows))))
    ;
  };

  Show[0] =sys.$checkExists(Show[0], function()  {sys.$params(arguments.length, 0);
    if (sys.asBool(sys.$eq(Main[0] , ""))) {
      wg
        .removeAll()
        .add(Q("table")
          .att("align", "center")
          .klass("frame")
          .add(Q("tr")
            .add(Q("td")
              .text(II("Without companies")))))
      ;
      return;
    }

    if (sys.asBool(sys.asBool(!sys.asBool(arr.any(Cos, function(C)  {sys.$params(arguments.length, 1);  return sys.$eq(C.nick , Option[0]);}))) && sys.asBool(sys.$neq(Option[0] , "*l"))))
      Option[0] =sys.$checkExists(Option[0],sys.$checkNull( "*v"));

    const menuWg =sys.$checkNull( menu.mk(
      [ menu.mkEntry([], stats)],
      [ menu.toption("*v", II("Volume"), showVolume),
        menu.separator(),
        menu.toption("*l", II("List"), showList)
      ],
      Option[0],
      false
    ));

    const body =sys.$checkNull( Q("div"));

    if (sys.asBool(sys.$eq(Option[0][0] , "*"))) body.add(list());
    else editor.mk(body, arr.map(Cos, function(C)  {sys.$params(arguments.length, 1);  return C.nick;}), Main[0], Option[0]);

    wg
      .removeAll()
      .add(menuWg)
      .add(body)
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
