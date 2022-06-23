// Copyright 20-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.acc;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import wgs.Msg;
import I18n._;
import pgs.acc.wgs.Chart;

/// Companies accounting charts.
class Companies {
  var wg: Domo;
  var cos: Array<ListEntry>;
  var showAll:Bool;

  function new (wg: Domo, cos: Array<ListEntry>) {
    this.wg = wg;
    this.cos = cos;
    showAll = false;

    view();
  }

  // View ----------------------------------------------------------------------

  function view (): Void {
    final ls = It.from(showAll ? cos : (cos).filter(e -> e.bought))
      .sort((e1, e2) -> e1.nick > e2.nick ? 1 : -1)
      .to()
    ;

    function separator () {
      return Q("tr")
        .add(Q("td")
          .att("colspan", 3)
          .html("<hr>"))
      ;
    }

    final chs = Q("table")
      .att("align", "center")
      .klass("frame");

    final n = ls.length;
    var tr = Q("tr");
    for (i in 0...n) {
      final chart = Chart.mk(!showAll, ls[i].nick, ls[i].url);
      switch (i % 3) {
      case 0:
        chs.add(separator());
        tr = Q("tr");
        tr.add(Q("td").add(chart));
      case 2:
        tr.add(Q("td").add(chart));
        chs.add(tr);
      default:
        tr.add(Q("td").add(chart));
      }
    }

    switch (n % 3) {
      case 1: chs.add(tr.add(Q("td")).add(Q("td")));
      case 2: chs.add(tr.add(Q("td")));
    }
    chs.add(separator());

    wg
      .removeAll()
      .add(Q("div")
        .style("text-align:center")
        .add(Ui.link(e -> changeShowAll())
          .klass("link")
          .html(showAll ? _("Portfolio") : _("All Companies"))))
      .add(chs)
    ;
  }

  // Control -------------------------------------------------------------------

  function changeShowAll (): Void {
    showAll = !showAll;
    view();
  }

  // Static --------------------------------------------------------------------

  /// Constructor.
  ///   wg: Container
  public static function mk (wg: Domo): Void {
    Cts.client.send([
      "module" => Js.ws("acc"),
      "source" => Js.ws("companies"),
      "rq" => Js.ws("list")
    ], rp -> {
      if (!rp["ok"].rb()) {
        Msg.error(
          Cts.failMsg,
          () -> {}
        );
        return;
      }

      final list = rp["list"].ra().map(e -> ListEntry.fromJs(e));
      new Companies(wg, list);
    });
  }
}

private class ListEntry {
  public var nick(default, null): String;
  public var bought(default, null): Bool;
  public var url(default, null): String;

  function new (nick: String, bought: Bool, url: String) {
    this.nick = nick;
    this.bought = bought;
    this.url =url;
  }

  public static function fromJs (js: Js): ListEntry {
    final a = js.ra();
    return new ListEntry(
      a[0].rs(),
      a[1].rb(),
      a[2].rs()
    );
  }
}
