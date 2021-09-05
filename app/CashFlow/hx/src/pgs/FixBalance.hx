// Copyright 20-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import I18n._;

/// Initial balance correction.
class FixBalance {
  public function new (wg: Domo, good: Float, wrong: Float) {
    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(_("Fix Initial Balance")))
    ;
  }

}
