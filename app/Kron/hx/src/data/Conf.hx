// Copyright 07-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Configuration data.
class Conf {
  /// Language.
  public final lang: String;

  function new (lang: String) {
    this.lang = lang;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(lang)
    ]);
  }

  public static function fromJs (js: Js): Conf {
    final a = js.ra();
    return new Conf(
      a[0].rs()
    );
  }
}
