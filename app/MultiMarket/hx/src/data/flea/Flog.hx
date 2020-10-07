// Copyright 10-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.flea;

import dm.Js;
import data.LogRow;

class Flog {
  // Log server identifier.
  public var id(default, null): String;
  // Log rows.
  public var entries(default, null): Array<LogRow>;

  /// Constructor.
  ///   id     : Log server identifier.
  ///   entries: Log rows.
  public function new (id: String, entries: Array<LogRow>) {
    this.id = id;
    this.entries = entries;
  }

  public static function fromJs (js: Js): Flog {
    final a = js.ra();
    return new Flog(
      a[0].rs(),
      a[1].ra().map(e -> LogRow.fromJs(e))
    );
  }
}
