// Copyright 17-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.home;

import dm.Domo;
import dm.Js;
import data.LogRow;
import data.Cts;
import wgs.Log;

/// Home page.
class Home {
  /// Constructor.
  ///   wg: Container.
  public static function mk (wg: Domo) {
    function load (fn: Array<LogRow> -> Void) {
      Cts.client.send([
        "module" => Js.ws("home"),
        "rq" => Js.ws("log")
      ], rp -> {
        final log = rp["log"].ra();
        fn(log.map(js -> LogRow.fromJs(js)));
      });
    }

    function reset (fn: () -> Void) {
      Cts.client.send([
        "module" => Js.ws("home"),
        "rq" => Js.ws("reset")
      ], rp -> {
        fn();
      });
    }

    Log.mk(wg, load, reset);
  }
}
