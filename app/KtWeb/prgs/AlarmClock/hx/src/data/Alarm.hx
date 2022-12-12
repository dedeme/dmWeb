// Copyright 25-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.Dt;
import I18n._;

/// Alarm data.
class Alarm {
  /// Identifier
  public final id: String;
  /// Time with minute precision.
  public final time: Float;

  function new (id: String, time: Float) {
    this.id = id;
    this.time = time;
  }

  public function timeToStr (): String {
    final now = Date.now();
    final d = Date.fromTime(time);
    return (
        now.getDay() == d.getDay() ? _("Today") : _("Tomorrow")
      ) + " " +
      _("at") + " " +
      DateTools.format(d, "%H:%M")
    ;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(id),
      Js.wf(time)
    ]);
  }

  public static function fromJs(js: Js): Alarm {
    final a = js.ra();
    return new Alarm(
      a[0].rs(),
      a[1].rf()
    );
  }
}
