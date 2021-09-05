// Copyright 27-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.It;
import dm.Opt;
import dm.Tp;
import I18n._;
import I18n._args;

/// Diary table
class Diary {
  public final entries: Array<DiaryEntry>;

  function new (entries: Array<DiaryEntry>) {
    this.entries = entries;
  }

  public function delete (entryIx: Int): Void {
    entries.splice(entryIx, 1);
  }

  /// Changes 'oldAcc' by 'newAcc'
  public function changeAcc (oldAcc: String, newAcc: String): Void {
    for (i in 0...entries.length) {
      final e = entries[i];
      for (j in 0...e.anns.length) {
        final a = e.anns[j];
        if (a.accId == oldAcc) e.anns[j] = new DiaryAnnotation(newAcc, a.am);
      }
    }
  }

  /// Returns the accounts list.
  public function accs (): Array<String> {
    final tmpMap: Map<String, Int> = [];
    for (e in entries) {
      for (a in e.anns) tmpMap.set(a.accId, 1);
    }
    final r: Array<String> = [];
    for (k in tmpMap.keys()) {
      r.push(k);
    }
    return r;
  }

  /// Returns 5 previous entries of ix
  public function previous (ix: Int): Array<DiaryEntry> {
    var start = ix - 5;
    if (start < 0) start = 0;
    return entries.slice(start, ix);
  }

  /// Returns 5 next entries of ix
  public function next (ix: Int): Array<DiaryEntry> {
    var end = ix + 6;
    if (end > entries.length) end = entries.length;
    return entries.slice(ix + 1, end);
  }

  public function toJs (): Js {
    return Js.wa(entries.map(e -> e.toJs()));
  }

  /// Returns a shallow copy of entries filtered and reversed.
  public function filterReverse (
    fromMonthIx: Int, toMonthIx: Int
  ): Array<Tp<Int, DiaryEntry>>  {
    final fromM = Month.format(fromMonthIx + 1);
    final toM = Month.format(toMonthIx + 1);
    final r: Array<Tp<Int, DiaryEntry>> = [];
    for (i in 0...entries.length) r.push(new Tp(i, entries[i]));
    return It.from(r)
      .filter(tp -> tp.e2.month >= fromM && tp.e2.month < toM)
      .reverse()
      .to()
    ;
  }

  /// The return is ever positive.
  public function accAmount (
    accId: String, fromMonthIx: Int, toMonthIx: Int
  ): Float {
    final fromM = Month.format(fromMonthIx + 1);
    final toM = Month.format(toMonthIx + 1);
    var sum = 0.0;
    for (e in entries) {
      if (e.month >= fromM && e.month < toM) {
        for (a in e.anns) {
          if (a.accId == accId) sum += a.am;
        }
      }
    }
    return sum;
  }

  public function totalAmount (
    plan: Plan, fromMonthIx: Int, toMonthIx: Int
  ): Float {
    final fromM = Month.format(fromMonthIx + 1);
    final toM = Month.format(toMonthIx + 1);
    var sum = 0.0;
    for (e in entries) {
      if (e.month >= fromM && e.month < toM) {
        for (a in e.anns) {
          if (plan.isIncome(a.accId)) sum += a.am;
          else sum -= a.am;
        }
      }
    }
    return sum;
  }

  public static function fromJs (js: Js): Diary {
    return new Diary(js.ra().map(e -> DiaryEntry.fromJs(e)));
  }
}

class DiaryEntry {
  public final month: String;
  public final desc: String;
  public final isIncome: Bool;
  public final am: Float;
  public final anns: Array<DiaryAnnotation>;

  public function new (
    month: String, desc: String, isIncome: Bool, anns: Array<DiaryAnnotation>
  ) {
    this.month = month;
    this.desc = desc;
    this.isIncome = isIncome;
    am = It.from(anns).reduce(0.0, (seed, e) -> seed + e.am);
    this.anns = anns;
  }

  /// Returns a new entry with the field 'anns' changed.
  public function setAnns (newAnns: Array<DiaryAnnotation>): DiaryEntry {
    return new DiaryEntry(month, desc, isIncome, newAnns);
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(month),
      Js.ws(desc),
      Js.wb(isIncome),
      Js.wa(anns.map(e -> e.toJs()))
    ]);
  }

  public static function fromJs (js: Js): DiaryEntry {
    final a = js.ra();
    return new DiaryEntry(
      a[0].rs(),
      a[1].rs(),
      a[2].rb(),
      a[3].ra().map(e -> DiaryAnnotation.fromJs(e))
    );
  }
}

class DiaryAnnotation {
  public final accId: String;
  public final am: Float;

  public function new (accId: String, am: Float) {
    this.accId = accId;
    this.am = am;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(accId),
      Js.wf(am)
    ]);
  }

  public static function fromJs (js: Js): DiaryAnnotation {
    final a = js.ra();
    return new DiaryAnnotation(
      a[0].rs(),
      a[1].rf()
    );
  }

  public static function setAccId (
    anns: Array<DiaryAnnotation>, ix: Int, acc: String
  ): Array<DiaryAnnotation> {
    final r: Array<DiaryAnnotation> = [];
    for (i in 0...anns.length) {
      final a = anns[i];
      if (i == ix) r.push(new DiaryAnnotation(acc, a.am));
      else r.push(new DiaryAnnotation(a.accId, a.am));
    }
    return r;
  }

  public static function setAm (
    anns: Array<DiaryAnnotation>, ix: Int, am: Float
  ): Option<Array<DiaryAnnotation>> {
    final oldSum = It.from(anns).reduce(0.0, (seed, e) -> seed + e.am);
    final r: Array<DiaryAnnotation> = [];
    final len1 = anns.length - 1;
    for (i in 0...len1) {
      final a = anns[i];
      if (i == ix) r.push(new DiaryAnnotation(a.accId, am));
      else r.push(new DiaryAnnotation(a.accId, a.am));
    }
    final newSum = It.from(r).reduce(0.0, (seed, e) -> seed + e.am);
    if (newSum > oldSum) return None;
    final a = anns[len1];
    r.push(new DiaryAnnotation(a.accId, oldSum - newSum));
    return Some(r);
  }

  public static function addAnnotation (
    anns: Array<DiaryAnnotation>
  ): Array<DiaryAnnotation> {
    final r = anns.copy();
    r.push(new DiaryAnnotation("", 0.0));
    return r;
  }

  public static function clear (
    anns: Array<DiaryAnnotation>
  ): Array<DiaryAnnotation> {
    return anns.filter(e -> e.am != 0.0);
  }

}

