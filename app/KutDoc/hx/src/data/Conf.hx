// Copyright 07-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Configuration data.
class Conf {
  /// Default source path.
  public final path: String;
  /// Language.
  public final lang: String;
  /// 'true' if all libraries should be shown.
  public final showAll: Bool;

  function new (path: String, lang: String, showAll: Bool) {
    this.path = path;
    this.lang = lang;
    this.showAll = showAll;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(path),
      Js.ws(lang),
      Js.wb(showAll)
    ]);
  }

  public static function fromJs (js: Js): Conf{
    final a = js.ra();
    return new Conf(
      a[0].rs(),
      a[1].rs(),
      a[2].rb()
    );
  }
}
