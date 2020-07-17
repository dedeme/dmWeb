// Copyright 28-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Account value.
class AccValue {
  /// Account description.
  public var description(default, null): String;
  /// Summary code (for balance and profits)
  public var summary(default, null): String;

  /// Constructor
  ///   description: Account description.
  ///   summary    : Summary code (for balance and profits)
  public function new (description: String, summary = "") {
    this.description = description;
    this.summary = summary;
  }

  /// Serialize to JSON.
  public function toJs (): Js {
    return Js.wa([Js.ws(description), Js.ws(summary)]);
  }

  /// Restore a JSON serialization.
  public static function fromJs (js: Js): AccValue {
    final a = js.ra();
    return new AccValue(a[0].rs(), a[1].rs());
  }

}
