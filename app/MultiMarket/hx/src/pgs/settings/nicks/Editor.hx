// Copyright 20-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings.nicks;

using StringTools;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Dec;
import dm.Menu;
import dm.ModalBox;
import data.Nick;
import data.Cts;
import data.Quote;
import wgs.Msg;
import wgs.Wrule;
import I18n._;
import I18n._args;

/// Nicks editor.
class Editor {
  var inputDiv: Domo;
  var menu2: Domo;
  var bodyDiv: Domo;
  var nicks: Array<Nick>;
  var nick: Nick;
  var nickModel: Nick;
  var quotes: Array<Quote>;
  var manuals: Int;
  var mquotes: Array<Quote>;
  var sIdNameCodes: Array<ServerData>;

  final nickIn = Ui.field("inBt").style("width:100px");

  final serverTestSpan = Q("span");

  var editBt: Domo;
  var cancelBt: Domo;
  var modifyBt: Domo;
  var leftArea: Domo;

  final splitDiv = Q("div");
  final splitBt: Domo;
  final splitMul: Domo;
  final splitSeeWg: Domo;
  final splitCancelWg: Domo;
  final splitAcceptWg: Domo;

  final rightArea = Q("textarea")
    .att("spellcheck", false)
    .att("rows", 25)
    .att("cols", 60)
    .disabled(true)
  ;

  final msgWait = Q("div");

  function new (
    inputDiv: Domo,
    menu2: Domo,
    bodyDiv: Domo,
    nicks: Array<Nick>,
    nick: Nick,
    nickModel: Nick,
    quotes: Array<Quote>,
    manuals: Int,
    mquotes: Array<Quote>,
    sIdNameCodes: Array<ServerData>
  ) {
    nicks.sort((e1, e2) -> e1.name > e2.name ? 1 : -1);
    this.inputDiv = inputDiv;
    this.menu2 = menu2;
    this.bodyDiv = bodyDiv;
    this.nicks = nicks;
    this.nick = nick;
    this.nickModel = nickModel;
    this.quotes = quotes;
    this.manuals = manuals;
    this.mquotes = mquotes;
    this.sIdNameCodes = sIdNameCodes;

    editBt = Q("button")
      .text(_("Edit"))
      .on(CLICK, e -> qEdit());
    cancelBt = Q("button")
      .text(_("Cancel"))
      .disabled(true)
      .on(CLICK, e -> qCancel());
    modifyBt = Q("button")
      .text(_("Modify"))
      .disabled(true)
      .on(CLICK, e -> qModify());
    leftArea = Q("textarea")
      .att("spellcheck", false)
      .att("rows", 25)
      .att("cols", 60)
      .disabled(true);

    splitBt = Q("button")
      .text(_("Split"))
      .on(CLICK, e -> splitMenu());
    splitMul = Ui.field("splitSee")
      .style("width:40px")
      .value("1");
    splitSeeWg = Q("button")
      .att("id", "splitSee")
      .style("width:75px")
      .text(_("See"))
      .on(CLICK, e -> splitSee());
    splitCancelWg = Q("button")
      .text(_("Cancel"))
      .on(CLICK, e -> splitCancel());
    splitAcceptWg = Q("button")
      .text(_("Accept"))
      .on(CLICK, e -> splitAccept());

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    nickIn.value(nick.name);
    inputDiv
      .removeAll()
      .add(Q("table")
        .klass("home")
        .add(Q("tr")
          .add(Q("td")
            .add(nickIn)))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center")
            .add(Q("button")
              .att("id", "inBt")
              .text(_("Modify"))
              .on(
                CLICK,
                e -> modifyNick(cast(nickIn.getValue(), String).trim())
              )))))
    ;

    final lopts = [
      new MenuEntry(
        None,
        Q("span")
          .style("font-size: 24px;")
          .text('${nick.name} [${quotes.length}](${manuals})')
      )
    ];
    final ropts = [
      Menu.toption("download", _("Download"), () -> download()),
      Menu.separator(),
      Menu.toption("test", _("Test"), () -> test())
    ];
    final menu = new Menu(lopts, ropts, "");
    menu2
      .removeAll()
      .add(menu.wg)
    ;

    leftArea.text(quotes.map(q -> q.toString()).join("\n"));
    rightArea.text(mquotes.map(q -> q.toString()).join("\n"));
    bodyDiv
      .removeAll()
      .add(msgWait)
      .add(Wrule.mkBig(_("Servers")))
      .add(serversDiv())
      .add(Wrule.mkBig(_("Quotes")))
      .add(Q("table").att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(Q("table")
              .style("width:100%")
              .add(Q("tr")
                .add(Q("td")
                  .add(leftMenu().wg)))
              .add(Q("tr")
                .add(textAreaHeader()))
              .add(Q("tr")
                .add(Q("td")
                  .add(leftArea)))))
          .add(Q("td")
            .add(Q("table")
              .style("width:100%")
              .add(Q("tr")
                .add(Q("td")
                  .add(splitDiv
                    .removeAll()
                    .add(this.rightMenu().wg))))
              .add(Q("tr")
                .add(textAreaHeader()))
              .add(Q("tr")
                .add(Q("td")
                  .add(rightArea)))))))
    ;
  }

  function serversDiv (): Domo {
    final tds = sIdNameCodes.map(inc -> {
      final field = Q("input")
        .att("type", "text")
        .style("width:125px")
        .value(inc.code);
      field.on(
        CHANGE,
        e -> updateCode(inc.id, cast(field.getValue(), String).trim())
      );
      return Q("td")
        .style("text-align:center")
        .add(Q("span")
          .html('${inc.name}<br>'))
        .add(field)
      ;
    });

    final trs = [];
    var tr = Q("tr");
    var i = 0;
    for (td in tds) {
      if (i == 0) {
        tr = Q("tr");
      }
      tr.add(td);
      ++i;
      if (i == 5) {
        trs.push(tr);
        i = 0;
      }
    };
    if (i != 0) {
      for (j in i...5) {
        tr.add(Q("td"));
      }
      trs.push(tr);
    }

    return Q("div")
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .style("vertical-align:middle")
            .add(Ui.link(e -> serverTests())
              .klass("link")
              .text(_("Test"))))
          .add(Q("td"))
          .add(Q("td")
            .add(serverTestSpan
              .removeAll()
              .add(Ui.img("unknown"))))))
      .add(Q("table")
        .klass("frame")
        .att("align", "center")
        .adds(trs))
    ;
  }

  function leftMenu (): Menu {
    final lopts = [
      new MenuEntry(None, editBt),
    ];
    final ropts = [
      new MenuEntry(None, cancelBt),
      new MenuEntry(None, Q("span").html("&nbsp;")),
      new MenuEntry(None, modifyBt),
    ];
    return new Menu(lopts, ropts, "");
  }

  function rightMenu (): Menu {
    final sel = Ui.select("nks", nicks.map(n ->
      (n.id == nickModel.id ? "+" : "") + n.name
    ));
    final selEl: js.html.SelectElement = cast(sel.e);
    sel.on(CHANGE, e -> setRightArea(nicks[selEl.selectedIndex]));
    final lopts = [
      new MenuEntry(None, sel)
    ];
    final ropts = [
      new MenuEntry(None, splitBt)
    ];
    return new Menu(lopts, ropts, "");
  }

  function setWait (nickName: String) {
    msgWait.removeAll();

    if (nickName != "") {
      final box = new ModalBox(
        Q("div")
          .add(Q("div")
            .style("text-align:center")
            .add(Ui.img("wait2.gif").klass("frame")))
          .add(Q("div").style("text-align:center").html(nickName)),
        false
      );
      msgWait.add(box.wg);
      box.show(true);
    }
  }

  // Control -------------------------------------------------------------------

  function modifyNick(nickName: String) {
    if (nickName == "") {
      Msg.error(_("Nick name is missing"), () -> view());
      return;
    }
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("nicks/editor"),
      "rq" => Js.ws("modifyNick"),
      "nickId" => Js.wi(nick.id),
      "name" => Js.ws(nickName)
    ], rp -> {
      if (!rp["ok"].rb()) {
        Msg.error(
          Cts.failMsg,
          () -> Editor.mk(inputDiv, menu2, bodyDiv, nicks, nick, nickModel)
        );
      } else {
        Msg.ok(
          Cts.okMsg,
          () -> js.Browser.location.assign("?settings&nicks")
        );
      }
    });
  }

  function download () {
    setWait(_("Downloading..."));
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("nicks/editor"),
      "rq" => Js.ws("download"),
      "nickId" => Js.wi(nick.id),
    ], rp ->{
      setWait("");

      final result = rp["result"].rs();
      if (result == "error") {
        Msg.error(_("Some error was found.<br>See Log."));
        return;
      }
      if (result == "warning") {
        Msg.info(_("Some quote was modified.<br>See Log."));
      } else {
        Msg.ok(_("Download ok."));
      }

      Editor.mk(inputDiv, menu2, bodyDiv, nicks, nick, nickModel);
    });
  }

  function test () {
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("nicks/editor"),
      "rq" => Js.ws("test"),
      "qs" => Js.ws(cast(leftArea.getValue(), String).trim())
    ], rp ->{
      final result = rp["result"].rs();
      if (result == "error") {
        Msg.error(_("Some error was found.<br>See Log."));
        return;
      }
      if ( result == "warning") {
        Msg.info(_("Some quote needs modification.<br>See Log."));
      } else {
        Msg.ok(_("Test ok."));
      }
    });
  }

  function updateCode (serverId: Int, code: String) {
    if (code == "") {
      Msg.error(_("Nick code is missing"), () -> this.view());
      return;
    }
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("nicks/editor"),
      "rq" => Js.ws("updateCode"),
      "serverId" => Js.wi(serverId),
      "nickId" => Js.wi(nick.id),
      "code" => Js.ws(code)
    ], rp -> {
    });
  }

  function serverTests () {
    var ok = true;
    It.from(sIdNameCodes).eachSync(
      (e, fn) -> {
        setWait(e.code);
        Cts.client.ssend([
          "module" => Js.ws("settings"),
          "source" => Js.ws("nicks/editor"),
          "rq" => Js.ws("serverTests"),
          "serverId" => Js.wi(e.id),
          "nickId" => Js.wi(nick.id)
        ], rp -> {
          fn(rp);
        });
      },
      rp -> {
        ok = ok && rp["ok"].rb();
      },
      () -> {
        setWait("");
        serverTestSpan.removeAll().add(Ui.img(ok ? "well" : "error"));
      }
    );
  }

  function qEdit () {
    editBt.disabled(true);
    cancelBt.disabled(false);
    modifyBt.disabled(false);
    leftArea.disabled(false);
    splitBt.disabled(true);
  }

  function qCancel () {
    editBt.disabled(false);
    cancelBt.disabled(true);
    modifyBt.disabled(true);
    leftArea.disabled(true);
    splitBt.disabled(false);
    view();
}

  function qModify () {
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("nicks/editor"),
      "rq" => Js.ws("qModify"),
      "nickId" => Js.wi(nick.id),
      "qs" => Js.ws(cast(leftArea.getValue(), String).trim())
    ], rp -> {
      if (rp["result"].rs() == "error") {
        Msg.error(_("No modification was performed.<br>See Log."));
        return;
      }
      if (rp["result"].rs() == "warning") {
        Msg.info(_("Quotes were modified with corrections.<br>See Log."));
      } else {
        Msg.ok(_("Quotes were successfully modified"));
      }
      Editor.mk(inputDiv, menu2, bodyDiv, nicks, nick, nickModel);
    });
  }

  function setRightArea (nick: Nick) {
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("nicks/editor"),
      "rq" => Js.ws("getQuotes"),
      "nickName" => Js.ws(nick.name),
    ], rp -> {
      final qs = rp["quotes"].ra().map(e -> Quote.fromJs(e));
      rightArea.text(qs.map(q -> q.toString()).join("\n"));
    });
  }

  function splitMenu () {
    final lopts = [
      new MenuEntry(None, Q("span").text(_("Mult.") + ":")),
      new MenuEntry(None, Q("span").html("&nbsp;")),
      new MenuEntry(None, splitMul),
      new MenuEntry(None, Q("span").html("&nbsp;")),
      new MenuEntry(None, splitSeeWg)
    ];
    final ropts = [
      new MenuEntry(None, splitCancelWg),
      new MenuEntry(None, Q("span").html("&nbsp;")),
      new MenuEntry(None, splitAcceptWg),
    ];
    final menu = new Menu(lopts, ropts, "");

    splitDiv
      .removeAll()
      .add(menu.wg)
    ;

    editBt.disabled(true);
    rightArea.text(leftArea.getText());
  }

  function splitSee () {
    final mul = cast(splitMul.getValue(), String).trim();
    var m = 0.0;
    switch (Dec.from(mul)) {
      case None:
        Ui.alert(_args(_("'%0' is not a number"), [mul]));
        return false;
      case Some(v):
        m = v;
    }

    final quotes = cast(leftArea.getValue(), String).trim().split("\n")
      .map(s -> Quote.fromString(s))
      .map(q -> new Quote(
        q.date, q.open * m, q.close * m, q.max * m, q.min * m,
        Math.round(q.vol / m), q.error
      ).toString());

    splitAcceptWg.disabled(true);
    rightArea.value(quotes.join("\n"));
    return true;
  }

  function splitCancel () {
    Editor.mk(inputDiv, menu2, bodyDiv, nicks, nick, nickModel);
  }

  function splitAccept () {
    if (Ui.confirm(_args(_("Modify quotes of '%0'?"), [nick.name]))) {
      if (splitSee()) {
        leftArea.value(cast(rightArea.getValue(), String).trim());
        qModify();
      }
    }
  }

  // Static --------------------------------------------------------------------

  static function textAreaHeader (): Domo {
    return Q("td")
      .klass("frame")
      .text(
        _("Date") + ":" +
        _("Open") + ":" +
        _("CloseN") + ":" +
        _("Max") + ":" +
        _("Min") + ":" +
        _("Vol") + ":" +
        _("State")
      )
    ;
  }

  /// Constructor.
  ///   inputDiv : Area from input.
  ///   menu2    : Submenu.
  ///   bodyDiv  : Area to show list.
  ///   nicks    : Nicks list.
  ///   nick     : Nick to edit.
  ///   nickModel: Nick model.
  public static function mk (
    inputDiv: Domo,
    menu2: Domo,
    bodyDiv: Domo,
    nicks: Array<Nick>,
    nick: Nick,
    nickModel: Nick
  ) {
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("nicks/editor"),
      "rq" => Js.ws("idata"),
      "nickId" => Js.wi(nick.id),
      "modelId" => Js.wi(nickModel.id)
    ], rp -> {
      if (!rp["ok"].rb()) {
        Msg.error(Cts.failMsg, () -> {});
        return;
      }

      new Editor(
        inputDiv,
        menu2,
        bodyDiv,
        nicks,
        nick,
        nickModel,
        rp["quotes"].ra().map(e -> Quote.fromJs(e)),
        rp["manuals"].ri(),
        rp["mquotes"].ra().map(e -> Quote.fromJs(e)),
        rp["sIdNameCodes"].ra().map(e -> ServerData.fromJs(e))
      );
    });
  }

}

private class ServerData {
  public var id(default, null): Int;
  public var name(default, null): String;
  public var code(default, null): String;
  public function new (id: Int, name: String, code: String) {
    this.id = id;
    this.name = name;
    this.code = code;
  }

  public static function fromJs (js: Js) {
    final a = js.ra();
    return new ServerData(
      a[0].ri(),
      a[1].rs(),
      a[2].rs()
    );
  }
}
