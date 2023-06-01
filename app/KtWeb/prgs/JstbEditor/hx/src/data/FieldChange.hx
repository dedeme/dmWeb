// Copyright 22-Jan-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Field change operation.

package data;

import dm.Js;

class FieldChange {
  public static final MAP_ADD = -1;
  public static final MAP_DEL = 0;
  public static final MAP_KEY = 1;
  public static final MAP_DUP = 2;

  /// One of Type constants.
  public final type: Int;
  /// String to control external modifications of table.
  public final hash: String;
  /// Takes the following values:
  ///   ARRAY -> Index of entry to modify or -1 for adding a 'null' entry at '0'.
  ///   MAP -> MAP_ADD: for adding a 'null' entry
  ///          MAP_DEL: Delete entry
  ///          MAP_KEY: Change key
  ///          MAP_DUP: Make a duplicate
  ///   others -> -1
  public final ix: Int;
  /// Takes the following values:
  ///   NULL => js.wi(intValue).  Type constant.
  ///   BOOLEAN => Js.wb(booleanValue)
  ///   NUMBER => Js.wi(intValue) or Js.wf(floatValue)
  ///   STRING => Js.ws(stringValue)
  ///   ARRAY => Js.wb(). 'true'->duplicate; 'false' delete.
  ///   MAP => ix == MAP_ADD: Js.ws(newKey)
  ///          ix == MAP_DEL: Js.ws(oldKey)
  ///          ix == MAP_KEY: Js.wa([oldKey, newKey])
  ///          ix == MAP_DUP: Js.wa([oldKey, newKey])
  ///   In any case can be 'js.wn()'.
  public final value: Js;

  function new (type: Int, hash: String, ix: Int, value: Js) {
    this.type = type;
    this.hash = hash;
    this.ix = ix;
    this.value = value;
  }

  /// Make any type to null.
  public static function mkToNull (hash: String, oldType: Int): FieldChange {
    return new FieldChange(oldType, hash, -1, Js.wn());
  }

  /// Value is Type constant.
  public static function mkNull (hash: String, value: Int): FieldChange {
    return new FieldChange(Type.NULL, hash, -1, Js.wi(value));
  }

  public static function mkBoolean (hash: String, value: Bool): FieldChange {
    return new FieldChange(Type.BOOLEAN, hash, -1, Js.wb(value));
  }

  public static function mkInt (hash: String, value: Int): FieldChange {
    return new FieldChange(Type.NUMBER, hash, -1, Js.wi(value));
  }

  public static function mkFloat (hash: String, value: Float): FieldChange {
    return new FieldChange(Type.NUMBER, hash, -1, Js.wf(value));
  }

  public static function mkString (hash: String, value: String): FieldChange {
    return new FieldChange(Type.STRING, hash, -1, Js.ws(value));
  }


  public static function mkArray (
    hash: String, ix: Int, isDuplicate: Bool
  ): FieldChange {
    return new FieldChange(Type.ARRAY, hash, ix, Js.wb(isDuplicate));
  }

  /// Creates a FieldChange for a map modification.
  ///   hash: Control table integrity.
  ///   type: One of MAP_XXX
  ///   oldKey: For type MAP_ADD its value is discarded.
  ///   newKey: For type MAP_DEL its value is discarded.
  public static function mkMap (
    hash: String, type: Int, oldKey: String, newKey: String
  ): FieldChange {
    return new FieldChange(Type.MAP, hash, type,
      type == MAP_ADD
        ? Js.ws(newKey)
        : type == MAP_DEL
          ? Js.ws(oldKey)
          : Js.wa([Js.ws(oldKey), Js.ws(newKey)])
    );
  }

  public static function mkMapModifyKey (
    hash: String, oldKey: String, newKey: String
  ): FieldChange {
    return new FieldChange(Type.MAP, hash, 1, Js.ws(newKey));
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wi(type),
      Js.ws(hash),
      Js.wi(ix),
      value
    ]);
  }
}

