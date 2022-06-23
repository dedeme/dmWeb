// Copyright 17-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;

class Wrule {
  static function mk (width: Int, color, title: String): Domo {
    return Q("table")
      .klass("main")
      .style('color:${color}')
      .add(Q("tr")
        .add(Q("td")
          .style('width:${Std.string(width)}px;')
          .add(Q("hr")))
        .add(Q("td")
          .style("width:5px;white-space: nowrap;")
          .html(title))
        .add(Q("td")
          .add(Q("hr"))))
    ;
  }

  /// Returns a big rule.
  ///   title: Rule title.
  public static function mkBig (title: String) {
    return mk(50, "#101010", title);
  }

  /// Returns a small rule.
  ///   title: Rule title.
  public static function mkSmall (title: String) {
    return mk(20, "#808080", title);
  }
}
