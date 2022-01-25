// Copyright 14-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.Dt;

/// Time types.
enum AnnTimeType { Ann_PERIODIC; Ann_FIX; Ann_INIT; Ann_NOTE; }

/// Text types.
enum AnnTextType { Ann_COMMAND; Ann_MESSAGE; }

/// Annotation data.
class Ann {
  public final id: Int;
  public final timeType: AnnTimeType;
  final data: Js;
  public final textType: AnnTextType;
  public final text: String;

  public function new (
    id: Int, timeType: AnnTimeType, data: Js,
    textType: AnnTextType, text: String
  ) {
    this.id = id;
    this.timeType = timeType;
    this.data = data;
    this.textType = textType;
    this.text = text;
  }

  /// Returns Array<'data.ri()'->(0-6)> if typeType is Ann_PERIODIC.
  public function days (): Array<Int> {
    if (timeType != Ann_PERIODIC)
      throw new haxe.Exception(
        "Expected timeType Ann_PERIODIC, but it is " + data
      );
    final a = data.ra();
    return a[1].ra().map(e -> e.ri());
  }

  ///  Returns 'Date.fromTime(data.rf() * 1000)' if typeType is Ann_FIX.
  public function date (): Date {
    if (timeType == Ann_INIT || timeType == Ann_NOTE)
      throw new haxe.Exception("timeType must not be Ann_INIT or Ann_NOTE");
    if (timeType == Ann_FIX)
      return Date.fromTime(data.rf() * 1000);
    final a = data.ra();
    return Date.fromTime(a[0].rf() * 1000);
  }

  /// Returns Notes group if time_type is Ann_NOTE.
  public function group (): String {
    if (timeType != Ann_NOTE)
      throw new haxe.Exception(
        "Expected timeType Ann_NOTE, but it is " + data
      );
    return data.rs();
  }

  public function toJs () : Js {
    return Js.wa([
      Js.wi(id),
      Js.wi(switch (timeType) {
        case Ann_PERIODIC: 0;
        case Ann_FIX: 1;
        case Ann_INIT: 2;
        case Ann_NOTE: 3;
      }),
      data,
      Js.wi(switch (textType) {
        case Ann_COMMAND: 0;
        case Ann_MESSAGE: 1;
      }),
      Js.ws(text)
    ]);
  }

  public static function fromJs (js: Js): Ann {
    final a = js.ra();
    return new Ann(
      a[0].ri(),
      switch (a[1].ri()) {
        case 0: Ann_PERIODIC;
        case 1: Ann_FIX;
        case 2: Ann_INIT;
        default: Ann_NOTE;
      },
      a[2],
      switch (a[3].ri()) {
        case 0: Ann_COMMAND;
        default: Ann_MESSAGE;
      },
      a[4].rs()
    );
  }
}
