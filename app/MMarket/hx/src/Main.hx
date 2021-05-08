// Copyright 12-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import data.Cts;
import wgs.Msg;

class Main {

  // STATIC

  // Main constructor.
  //   wg: Container.
  //   fn: Function to call at end of function.
  static function mk (wg: Domo, fn: () -> Void) {
    fn();
  }

  /// Application entry
  static public function main (): Void {
    var wg = Q("div");
    mk(wg, () -> {
      Q("@body")
        .removeAll()
        .add(wg)
        .add(Cts.foot)
        .add(Ui.upTop("up"))
        .add(Msg.wg)
      ;

      var fc = Q("#autofocus");
      if (fc.e != null) fc.e.focus();
    });
  }

}
