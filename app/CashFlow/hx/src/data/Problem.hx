// Copyright 27-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Opt;
import data.Diary;
import data.Cash;

class Problem {
  /// Index of problem or -1
  public final ix: Int;

  /// Error entry or empty if there is any extra CashFlow entry.
  public final hcErr: Option<CashEntry>;
  /// 5 previous entries from before to after.
  public final hcPrevs: Array<CashEntry>;
  /// 5 next entries from before to after.
  public final hcNexts: Array<CashEntry>;

  /// Error entry or empty if there is any extra Hconta entry.
  public final cErr: Option<DiaryEntry>;
  /// 5 previous entries from before to after.
  public final cPrevs: Array<DiaryEntry>;
  /// 5 next entries from before to after.
  public final cNexts: Array<DiaryEntry>;

  function new (
    ix: Int,

    hcErr: Option<CashEntry>,
    hcPrevs: Array<CashEntry>,
    hcNexts: Array<CashEntry>,

    cErr: Option<DiaryEntry>,
    cPrevs: Array<DiaryEntry>,
    cNexts: Array<DiaryEntry>
  ) {
    this.ix = ix;
    this.hcErr = hcErr;
    this.hcPrevs = hcPrevs;
    this.hcNexts = hcNexts;
    this.cErr = cErr;
    this.cPrevs = cPrevs;
    this.cNexts = cNexts;
  }

  // Returns a nomal error.
  static function mk (
    hcDiary: Cash, cDiary: Diary, ix: Int
  ): Problem {
    return new Problem(
      ix,

      Some(hcDiary.entries[ix]),
      hcDiary.previous(ix),
      hcDiary.next(ix),

      Some(cDiary.entries[ix]),
      cDiary.previous(ix),
      cDiary.next(ix)
    );
  }

  // Returns an extra CashFlow annotation error.
  static function mkExtraC (
    hcDiary: Cash, cDiary: Diary, ix: Int
  ): Problem {
    return new Problem(
      ix,

      None,
      hcDiary.previous(ix),
      [],

      Some(cDiary.entries[ix]),
      cDiary.previous(ix),
      cDiary.next(ix)
    );
  }

  // Returns an extra Hconta annotation error.
  static function mkExtraHc (
    hcDiary: Cash, cDiary: Diary, ix: Int
  ): Problem {
    return new Problem(
      ix,

      Some(hcDiary.entries[ix]),
      hcDiary.previous(ix),
      hcDiary.next(ix),

      None,
      cDiary.previous(ix),
      []
    );
  }

  // Returns a 'No problem'
  static function mkOk () {
    return new Problem(-1, None, [], [], None, [], []);
  }

  /// Return the first problem of matching 'hcDiary' and 'cDiary'.
  ///    hcDiary: Hconta diary.
  ///    cDiary: CashFlow Diary.
  public static function firstProblem(hcDiary: Cash, cDiary: Diary): Problem {
    var lg = hcDiary.entries.length;
    if (cDiary.entries.length < lg) lg = cDiary.entries.length;

    for (i in 0...lg) {
      if (!hcDiary.entries[i].eqHcC(cDiary.entries[i])) {
        return mk(hcDiary, cDiary, i);
      }
    }

    if (lg == hcDiary.entries.length) {
      if (lg == cDiary.entries.length) return mkOk();
      return mkExtraC(hcDiary, cDiary, lg);
    }

    return mkExtraHc(hcDiary, cDiary, lg);
  }

}
