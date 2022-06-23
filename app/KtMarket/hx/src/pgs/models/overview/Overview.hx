// Copyright 29-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.models.overview;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Ui;
import data.Model;
import I18n._;

/// Models overview page.
class Overview {
  public function new (wg: Domo, model: Model) {
    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .html("<big>" + model.name + "</big>"))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(Q("div")
              .klass("frame")
              .style("width:680px")
              .html(model.doc)))))
    ;
  }
}

