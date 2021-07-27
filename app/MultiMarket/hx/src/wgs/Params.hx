// Copyright 10-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;

/// Parameters widget
class Params {
  public final wg = Q("div");
  var param: Param;
  public var value(get, never): Float;
  function get_value() return param.value;

  /// Constructor:
  ///   names : Parameter name.
  ///   id    : TextFields root id. Final id will be made with id + index.
  ///   nextId: Id of next widget to pass focus.
  ///   values: Defaul values.
  public function new (
    name: String, id: String, nextId: String, ?value: Float
  ) {
    param = value == null
      ? new Param(name, id, nextId)
      : new Param(name, id, nextId, value)
    ;

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    wg
      .removeAll()
      .add(Q("table").klass("frame")
        .add(Q("tr")
          .add(Q("td").add(param.wg))))
    ;
  }
}

