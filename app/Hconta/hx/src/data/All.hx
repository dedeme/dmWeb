// Copyright 25-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Ui.Q;
import dm.Js;
import Cts;
import I18n._;

import data.Acc;

/// Every application data.
class All {
  public var timeStamp: String;
  public var conf(default, null): Conf;
  public var acc(default, null): Acc;

  function new () {}

  /// Sends data to server.
  /// If data is out of date no changes is made and a "expired" page is shown.
  public function send (fn: Void -> Void): Void {
    Cts.client.send([
      "source" => Js.ws("Main"),
      "rq" => Js.ws("write"),
      "timeStamp" => Js.ws(this.timeStamp),
      "year" => Js.ws(conf.currentYear),
      "lang" => Js.ws(conf.language),
      "data" => acc.toJs()
    ], rp -> {
      final timeStamp = rp["timeStamp"].rs();
      if (timeStamp == "") {
        MsgPage.mk(
          Q("@body"), Cts.app, _("Data base is out of date.")
        );
      } else {
        this.timeStamp = timeStamp;
        fn();
      }
    });
  }

  public static function request (fn: All -> Void): Void {
    Cts.client.send([
      "source" => Js.ws("Main"),
      "rq" => Js.ws("read")
    ], rp -> {
      final timeStamp = rp["timeStamp"].rs();
      final year = rp["year"].rs();
      final lang = rp["lang"].rs();
      final years = rp["years"].rArray(e -> e.rs());
      final r = new All();
      r.timeStamp = timeStamp;
      r.conf = new Conf(lang, years, year);
      r.acc = Acc.fromJs(rp["data"]);
      fn(r);
    });
  }

}
