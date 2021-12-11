// Copyright 03-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings.investorsPg;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Dec;
import dm.It;
import dm.Js;
import dm.B64;
import dm.Menu;
import data.Cts;
import data.Investors;
import wgs.Params;
import I18n._;
import I18n._args;

/// Investors management.
class InvestorsPg {
  var wg: Domo;
  final investors: Investors;
  final qlevel: Int;

  final editorDiv = Q("div");
  final paramsDiv = Q("div");

  /// Constructor.
  ///   wg: Container.
  ///   investors: Investors data
  function new (wg: Domo, investors: Investors, qlevel: Int) {
    this.wg = wg;
    this.investors = investors;
    this.qlevel = qlevel;

    view();
  }

  // View ----------------------------------------------------------------------

  function paramsView (
    nick: String, ?value: Float
  ): Void {
    final base = investors.base;

    final params = value == null
      ? new Params(
        _("Jump"), "ps", "accept"
      )
      : new Params(
        _("Jump"), "ps", "accept", value
      )
    ;

    paramsDiv
      .removeAll()
      .add(Q("table")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center")
            .add(Ui.link(e -> paramsView(nick, base / Cts.rangesToParam))
              .klass("link")
              .text(_("Default Model")))))
        .add(Q("tr")
          .add(Q("td")
            .add(Q("table")
              .att("align", "center")
              .add(Q("tr")
                .add(Q("td")
                .add(params.wg))))))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center")
            .add(Q("button")
              .text(_("Cancel"))
              .on(CLICK, e -> cancel()))
            .add(Q("span").text(" "))
            .add(Q("button")
              .att("id", "accept")
              .text(_("Accept"))
              .on(CLICK, e -> {
                accept(nick, params.value);
              })))))
    ;
  }

  function editorView (nick: String): Void {
    final base = investors.base;
    final nicks = investors.list;
    final param = (nick == "" ? base : nicks[nick][qlevel]) / Cts.rangesToParam;

    this.paramsView(nick, param);

    editorDiv
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(nick == "" ? _("Default Model") : _args(_("%0 Model"), [nick])))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .add(paramsDiv))))
      .add(Q("hr"))
    ;
  }

  function view () {
    final base = investors.base;
    final nicks = investors.list;
    final knicks = It.fromMap(nicks).map(tp -> tp.e1).to();
    knicks.sort((k1, k2) -> k1 > k2 ? 1 : -1);

    final lopts = [];
    for (i in 0...Cts.qlevels) {
      final lb = '${_("Inv")}-${i}';
      if (i > 0) lopts.push(Menu.separator());
      lopts.push(Menu.toption("" + i, lb, () -> selManager(i)));
    }
    final menu = new Menu(lopts, [], "" + qlevel);

    wg
      .removeAll()
      .add(menu.wg)
      .add(editorDiv)
      .add(Q("div")
        .klass("head")
        .text(_("Default Qlevel")))
      .add(Q("table")
        .klass("white")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .text(_("Jump")))
          .add(Q("td")
            .klass("header")))
        .add(Q("tr")
          .add(Q("td")
            .klass("border")
            .add(Ui.link(e -> edit(""))
              .klass("link")
              .text(Fns.nformat(base / Cts.rangesToParam, Cts.paramDecs))))
          .add(Q("td")
            .add(Q("a")
              .att("href", "?models&-1&charts&true&" + base)
              .html("&nbsp;" + _("Charts") + "&nbsp;")))))
      .add(Q("div")
        .klass("head")
        .html(_("Nick models")))
      .add(Q("table")
        .klass("white")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .html(_("Nick")))
          .add(Q("td")
            .klass("header")
            .html(_("Jump")))
          .add(Q("td")
            .klass("header")
            .text("·"))
          .add(Q("td")
            .klass("header")))
        .adds(knicks.map(k -> {
          final param = nicks[k][qlevel];
          return Q("tr")
            .add(Q("td")
              .klass("border")
              .text(k))
            .add(Q("td")
              .klass("number")
              .add(Ui.link(e -> edit(k))
                .klass("link")
                .text(Fns.nformat(param / Cts.rangesToParam, Cts.paramDecs))))
            .add(Q("td")
              .klass("border")
              .add(Ui.img(param == base ? "blank" : "warning")))
            .add(Q("td")
              .klass("border")
              .add(Q("a")
                .att("href", "?models&" + qlevel + "&charts&true" + param)
                .html("&nbsp;" + _("Charts") + "&nbsp;")))
          ;
        })))

    ;
  }

  // Controls ------------------------------------------------------------------

  function selManager (managerIx: Int): Void {
    mk(wg, "" + managerIx);
  }

  function edit (nick: String): Void {
    editorView(nick);
    js.Browser.window.scroll(0, 0);
  }

  function cancel (): Void {
    editorDiv.removeAll();
  }

  function accept (nick: String, param: Float): Void {
    final mdBox = new dm.ModalBox(Ui.img("wait2.gif"), false);
    wg.add(mdBox.wg);
    mdBox.show(true);
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("investors"),
      "rq" => Js.ws("update"),
      "qlevel" => Js.wi(qlevel),
      "nickName" => Js.ws(nick), // "" -> change base
      "rangeValue" => Js.wi(Math.round(param * Cts.rangesToParam))
    ], rp -> {
      final changed = rp["changed"].rb();
      mdBox.show(false);
      if (!changed) {
        wgs.Msg.info(_("No change was done after regularization"));
      }
      InvestorsPg.mk(wg, "" + qlevel);
    });
  }

  // Static --------------------------------------------------------------------

  /// Constructor.
  ///   wg       : Container.
  ///   investorIx: Investor index.
  public static function mk (wg: Domo, investorIx: String) {
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("investors"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final investors = Investors.fromJs(rp["investors"]);
      var qlevel = investorIx == "" || !Dec.digits(investorIx)
        ? 0
        : Std.parseInt(investorIx)
      ;
      if (qlevel < 0 || qlevel >= Cts.qlevels) {
        qlevel = 0;
      }
      new InvestorsPg(wg, investors, qlevel);
    });
  }

}
