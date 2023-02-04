// Copyright 20-Jan-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Path of a field in a table.

package data;

import dm.Js;

/// Entry of field path. I can match an Array or a Map.
class FieldEntry {
  public final isMap: Bool;
  /// Array index if 'isMap==false'
  public final ix: Int;
  /// Map key if 'isMap== true'
  public final key: String;

  function new (isMap: Bool, ix: Int, key: String) {
    this.isMap = isMap;
    this.ix = ix;
    this.key = key;
  }

  /// Creates an Array entry.
  public static function mkArray (ix: Int) {
    return new FieldEntry(false, ix, "");
  }

  /// Creates a Map entry.
  public static function mkMap (key: String) {
    return new FieldEntry(true, -1, key);
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wb(isMap),
      Js.wi(ix),
      Js.ws(key)
    ]);
  }

  public static function fromJs (js: Js): FieldEntry {
    final a = js.ra();
    return new FieldEntry(
      a[0].rb(),
      a[1].ri(),
      a[2].rs()
    );
  }
}

/// Field path in a table.
class Field {
  public final fieldPath: Array<FieldEntry>;

  public function new (fieldPath: Array<FieldEntry>) {
    this.fieldPath = fieldPath;
  }

  public function toJs (): Js {
    return Js.wa(fieldPath.map(e -> e.toJs()));
  }

  public static function fromJs (js: Js): Field {
    return new Field(js.ra().map(FieldEntry.fromJs));
  }
}

