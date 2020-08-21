// Copyright 20-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings.nicks;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Menu;
import data.Nick;
import data.Cts;
import data.Quote;
import wgs.Msg;
import wgs.Wrule;
import I18n._;

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
    this.inputDiv = inputDiv;
    this.menu2 = menu2;
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
              .on(CLICK, e -> modifyNick(nickIn.getValue().trim()))))))
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
      field.on(CHANGE, e -> updateCode(inc.id, field.getValue().trim()));
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

  // Control -------------------------------------------------------------------

  function modifyNick(nickName: String) {
  }

  function updateCode (serverId: Int, code: String) {
  }

  function download () {
  }

  function test () {
  }

  function serverTests () {
  }

  function qEdit () {
  }

  function qCancel () {
  }

  function qModify () {
  }

  function setRightArea (nick: Nick) {
  }

  function splitMenu () {
  }

  function splitSee () {
  }

  function splitCancel () {
  }

  function splitAccept () {
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
  ///   fnFail   : Function to do if 'nick' can not be edited.
  public static function mk (
    inputDiv: Domo,
    menu2: Domo,
    bodyDiv: Domo,
    nicks: Array<Nick>,
    nick: Nick,
    nickModel: Nick,
    fnFail: () -> Void
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
