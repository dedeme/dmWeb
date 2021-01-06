// Copyright 26-Dec-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Opt;
import dm.Menu;

/// Extends Menu. Alernative menu with srinked selected box.
class Menu2 extends Menu {

  /// Constructor.
  ///   lopts        : Left options.
  ///   ropts        : Right options.
  ///   selected     : Identifier of the initial selected option. It it does
  ///                  not match any identifier, no one will be selected.
  ///   withSeparator: If its value is 'true' a vertical bar separates 'lopts'
  ///                  and 'ropts' (Default 'false').
  public function new (
    lopts: Array<MenuEntry>, ropts: Array<MenuEntry>,
    selected: String, withSeparator: Bool = false
  ) {
    super (lopts, ropts, selected, withSeparator);
    final setId = o -> {
      switch (o.id) {
        case Some(v): o.wg.klass(v == selected ? "frameMenu": "link");
        case None:
      }
    }
    for (o in lopts) setId(o);
    for (o in ropts) setId(o);
  }
}
