// Copyright 28-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Dt;
import dm.Js;
import dm.Tp;
import dm.Opt;

/// Diary entry.
class DiaryEntry {
  /// Date of operation.
  public var date(default, null): Date;
  /// Description of operation.
  public var description(default, null): String;
  /// Debits counts-values.
  public var debits(default, null): Map<String, Cu>;
  /// Credits counts-values.
  public var credits(default, null): Map<String, Cu>;

  /// Constructor.
  public function new (
    date: Date, description: String,
    debits: Map<String, Cu>, credits: Map<String, Cu>
  ) {
    this.date = date;
    this.description = description;
    this.debits = debits;
    this.credits = credits;
  }

  /// Serialize to JSON.
  public function toJs (): Js {
    return Js.wa([
      Js.ws(Dt.to(date)),
      Js.ws(description),
      Js.wMap(debits, cu -> cu.toJs()),
      Js.wMap(credits, cu -> cu.toJs())
    ]);
  }

  /// Restore a JSON serialization.
  public static function fromJs (js: Js): DiaryEntry {
    final a = js.ra();
    return new DiaryEntry (
      Opt.get(Dt.from(a[0].rs())),
      a[1].rs(),
      a[2].rMap(Cu.fromJs),
      a[3].rMap(Cu.fromJs)
    );
  }

}
