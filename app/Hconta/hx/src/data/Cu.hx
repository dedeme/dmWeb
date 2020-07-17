// Copyright 28-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Dec;
import dm.Js;

/// Currency.
class Cu {
  /// Value.
  public var value(default, null): Float;

  /// Constructor.
  /// 'v' is rounded with 2 decimals.
  public function new (v: Float) {
    value = Dec.round(v, 2);
  }

  /// Returns this + cu.
  public function add (cu: Cu): Cu {
    return new Cu(value + cu.value);
  }

  /// Returns this - cu.
  public function sub (cu: Cu): Cu {
    return new Cu(value - cu.value);
  }

  /// Returns abs(this).
  public function abs (): Cu {
    return value >= 0 ? this : new Cu(-value);
  }

  /// Returns -this
  public function negate (): Cu {
    return new Cu(-value);
  }

  /// Returns a ISO representation of 'this'.
  public function toString () {
    return Dec.toIso(value, 2);
  }

  /// Serialize to JSON.
  public function toJs (): Js {
    return Js.wf(value);
  }

  /// Restore a JSON serialization.
  public static function fromJs (js: Js): Cu {
    return new Cu(js.rf());
  }

}
