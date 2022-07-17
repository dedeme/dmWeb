// Copyright 19-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.acc;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Menu;
import data.JailLossesEntry;
import I18n._;
import pgs.acc.wgs.JailWg;

/// Jail losses.
class Jail {
  var wg: Domo;
  var data: Array<Array<JailLossesEntry>>;
  var mSel: Int;

  function new (wg: Domo, data: Array<Array<JailLossesEntry>>) {
    this.wg = wg;
    this.data = data;
    mSel = -1;

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final wg = Q("div");
    final data = data;
    final sel = mSel == -1 ? "All" : _("Inv-") + Std.string(mSel);

    final lopts = [
      Menu.toption("All", _("All"), () -> setMenu(-1))
    ];
    for (i in 0...data.length) {
      final op = _("Inv-") + Std.string(i);
      lopts.push(Menu.separator());
      lopts.push(Menu.toption(op, op, () -> setMenu(i)));
    }
    final ropts = [];
    final menu = new Menu(lopts, ropts, sel);

    var d = [];
    if (mSel == -1) {
      final entriesLength = data[0].length;
      for (i in 0...entriesLength) {
        var losses = 0.0;
        for (man in 0...data.length) {
            losses += data[man][i].losses;
        }
        d.push(new JailLossesEntry(data[0][i].date, losses));
      }
    } else {
      d = data[mSel];
    }
    JailWg.mk(wg, d);

    this.wg
      .removeAll()
      .add(menu.wg)
      .add(wg)
    ;
  }

  // Control -------------------------------------------------------------------

  function setMenu (manager: Int) {
    mSel = manager;
    view();
  }

  // Static --------------------------------------------------------------------

  /// Constructor.
  ///   wg: Container.
  public static function mk (wg: Domo) {
    Cts.client.send([
      "module" => Js.ws("acc"),
      "source" => Js.ws("jail"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final data = rp["data"].ra()
        .map(mn -> mn.ra().map(e -> JailLossesEntry.fromJs(e)))
      ;
      new Jail(wg, data);
    });
  }
}
