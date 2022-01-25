// Copyright 03-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import update.RankingsUpdate;
import update.IbexUpdate;

class Update {

  public static function process () {
    RankingsUpdate.run();
    IbexUpdate.run();
  }
}
