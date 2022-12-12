// Copyright 21-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.Dt;

/// Time types.
enum AnnType { Ann_PERIODIC; Ann_FIX; Ann_MANUAL; }

/// Annotation data.
class Ann {
  public final id: Int;
  public final type: AnnType;
  final data: Js;
  public final text: String;

  /// id  : Annotation identifier.
  /// type: Type of annotation.
  /// data: JSON array with the following fields:
  ///       if type == PERIODIC: 0 -> Float (time in seconds),
  ///                            1 -> Array<Int (Weekday 0[Monday]-6)>
  ///       if type == FIX: 0 -> Float (time in seconds)
  ///       if type == MANUAL: No fields.
  /// text: Command to execute.
  public function new (
    id: Int, type: AnnType, data: Js, text: String
  ) {
    this.id = id;
    this.type = type;
    this.data = data;
    this.text = text;
  }

  /// Returns Array<'data.ri()'->(0-6)> if type is Ann_PERIODIC.
  public function days (): Array<Int> {
    if (type != Ann_PERIODIC)
      throw new haxe.Exception(
        "Expected type Ann_PERIODIC, but it is " + data
      );
    final a = data.ra();
    return a[1].ra().map(e -> e.ri());
  }

  ///  Returns 'Date.fromTime(data.rf() * 1000)' if type is Ann_FIX or Ann_PERIODIC.
  public function date (): Date {
    if (type == Ann_MANUAL)
      throw new haxe.Exception("type must not be Ann_MANUAL");
    if (type == Ann_FIX)
      return Date.fromTime(data.rf() * 1000);
    final a = data.ra();
    return Date.fromTime(a[0].rf() * 1000);
  }

  public function toJs () : Js {
    return Js.wa([
      Js.wi(id),
      Js.wi(switch (type) {
        case Ann_PERIODIC: 0;
        case Ann_FIX: 1;
        case Ann_MANUAL: 2;
      }),
      data,
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
        default: Ann_MANUAL;
      },
      a[2],
      a[3].rs()
    );
  }
}
