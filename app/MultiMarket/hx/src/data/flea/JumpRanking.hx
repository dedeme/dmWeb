// Copyright 13-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.flea;

import dm.Js;

/// Evaluation of a paremeter for a flea of one parameter.

class JumpRanking {
  public final date: String;
  public final ranking: Array<JumpResult>;

  function new (date: String, ranking: Array<JumpResult>) {
    this.date = date;
    this.ranking = ranking;
  }

  public static function fromJs (js: Js): JumpRanking {
    final a = js.ra();
    return new JumpRanking(
      a[0].rs(),
      a[1].ra().map(e -> JumpResult.fromJs(e))
    );
  }
}
