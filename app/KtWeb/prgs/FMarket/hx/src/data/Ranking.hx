// Copyright 08-Dic-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Rankings data base
class Ranking {
  public final date: String;
  public final fleas: Array<Flea>;

  function new (date: String, fleas: Array<Flea>) {
    this.date = date;
    this.fleas = fleas;
  }

  public static function fromJs(js: Js): Ranking {
    final a = js.ra();
    return new Ranking(
      a[0].rs(),
      a[1].ra().map(Flea.fromJs)
    );
  }
}
