// Copyright 15-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package dm;

import dm.Opt;
import dm.It;
import dm.Dt;

/// Static functions to manipulate the web local store.
class Store {

  /// Removes every key which starts with 'prefix'.
  public static function clear (prefix: String): Void {
    for(k in keys().to())
      if (StringTools.startsWith(k, prefix)) del(k);
  }

  /// Removes the key [key]
  public static function del (key: String): Void {
    untyped js.Syntax.code("localStorage.removeItem(key)");
  }

  /// Removes some [keys] past the time [time] since it was called itself.<p>
  /// If it has not called ever delete keys too.
  ///   name: Storage key for saving the time
  ///   keys: Array with the keys to remove
  ///   time: Time in hours
  public static function expires (
    name: String, keys: Array<String>, time: Float
  ): Void {
    final dt: Float = Date.now().getTime();
    switch (get(name)) {
      case Some(ks):
        if (dt > Std.parseFloat(ks)) It.from(keys).each(k -> del(k));
      case None:
        It.from(keys).each(k -> del(k));
    }
    put(name, Std.string (dt + time * 3600000.));
  }

  /// Returns the value of key [key] or <b>null</b> if it does not exists.
  public static function get (key: String): Option<String> {
    final r = untyped js.Syntax.code("localStorage.getItem(key)");
    return r == null ? None : Some(r);
  }

  /// Returns the key in position [ix].
  public static function key (ix: Int): String {
    return untyped js.Syntax.code("localStorage.key(ix)");
  }

  /// Returns a It with all keys
  public static function keys (): It<String> {
    final sz = length();
    var c = 0;
    return new It (
      function () { return c < sz; }
    , function () { return key(c++); }
    );
  }

  /// Puts a new value.
  public static function put (key: String, value: String): Void {
    untyped js.Syntax.code("localStorage.setItem(key, value)");
  }

  /// Returns the number of elements.
  public static function length (): Int {
    return untyped js.Syntax.code("localStorage.length");
  }

  /// Returns the memory used in bytes.
  public static function size (): Int {
    var sum = 0;
    keys().each(k ->
      sum += (k.length + Opt.get(Store.get(k)).length) * 2
    );
    return sum;
  }

  /// Returns a It with all values
  public static function values (): It<Option<String>> {
    return keys().map(e -> get(e));
  }

  /// Tries 'fn' with 'get(key)'. If 'get(key)' is 'None' or 'fn' throws an
  /// exception, returns 'value'.
  ///   key  : Key to read.
  ///   fn   : Function to run with the result of 'get(key)'.
  ///   value: Default value if key does not exist or 'fn' fail.
  public static function oget <T> (
    key: String, fn: String -> T, value: T
  ): T {
    try {
      switch (Store.get(key)) {
        case Some(l): return fn(l);
        case None: return value;
      }
    } catch (e) {
      return value;
    }
  }

}
