// Copyright 09-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.fleas.overview;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Ui;
import data.flea.Fmodel;
import data.flea.Doc;

/// Fleas overview page.
class Overview {
  public function new (wg: Domo, model: Fmodel) {
    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .html("<big>" + model.id + "</big><br>" + model.name))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(Q("div")
              .klass("frame")
              .style("width:680px")
              .html(Doc.read(model.id))))))
    ;
  }
}

