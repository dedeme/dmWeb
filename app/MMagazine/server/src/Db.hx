// Copyright 31-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.File;

class Db {

  /// Data base initialization
  public static function init (): Void {
    ex.qmarket.Models.init();
    ex.qmarket.Profits.init();
    ex.qmarket.DiariesDb.init();
    ex.mmarket.MModelEvalsDb.init();

    if (!File.exists(Cts.dbHome)) {
      File.mkdir(Cts.dbHome);
    }
    db.Rankings.init(Cts.dbHome);
    db.IbexTb.init(Cts.dbHome);
    db.ModelEvalsTb.init(Cts.dbHome);
  }

}
