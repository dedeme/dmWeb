// Copyright 20-Jan-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// JSON data.

package data;

import dm.Js;

class JsData {
  public final control: String;
  public final tooBig: Bool;
  /// One of Type constants: NULL, BOOLEAN, NUMBER, STRING, ARRAY or MAP
  public final type: Int;
  /// Has the following values:
  ///   NULL: Js.wn()
  ///   BOOLEAN: js.wb()
  ///   NUMBER: js.wi() or js.wf()
  ///   STRING: js.ws()
  ///   ARRAY: js.wa([length, [[index, value]]])
  ///          (values are js-strings possibly truncated)
  ///   MAP: js.wa([[allKeys], map[value])
  ///        (values are js-strings possibly truncated)
  public final data: Js;

  function new (
    control: String, tooBig: Bool, type: Int, data: Js
  ) {
    this.control = control;
    this.tooBig = tooBig;
    this.type = type;
    this.data = data;
  }

  public static function fromJs (js: Js): JsData {
    final a = js.ra();
    return new JsData(
      a[0].rs(),
      a[1].rb(),
      a[2].ri(),
      a[3]
    );
  }
}

