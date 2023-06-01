// Copyright 25-Jan-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Change to null widget.

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Opt;
import data.Tpath;
import data.FieldChange;
import I18n._;
import I18n._args;

class ToNullWg {
  final wg: Domo;
  final hash: String;
  final type: Int;
  final fn: (FieldChange, Option<Tpath>) -> Void;

  public function new (
    wg: Domo, hash: String, type: Int, fn: (FieldChange, Option<Tpath>) -> Void
  ) {
    this.wg = wg;
    this.hash = hash;
    this.type = type;
    this.fn = fn;
  }

  // View ----------------------------------------------------------------------

  public function show () {
    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(type == -1
            ? Q("td")
                .text(_("Table is too big to be modified"))
            : Q("td")
                .klass("frame")
                .add(Ui.link(setTo)
                  .klass("link")
                  .text(_("Change field to 'null'"))))))
      .add(Q("hr"))
    ;
  }

  // Control -------------------------------------------------------------------

  function setTo (): Void {
    if (!Ui.confirm(_args(_("Set field to %0?"), ["'null'"]))) return;
    fn(FieldChange.mkToNull(hash, type), None);
  }
}
