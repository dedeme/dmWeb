// Copyright 21-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import dm.Js;
import dm.Dt;
import dm.Opt;

class TeamTable<T> {
  final wg: Domo;
  final teams: Array<Array<String>>;
  final data: Array<Array<Option<T>>>;
  final toString: (T) -> String;

  /// Constructor.
  ///   wg: Container.
  ///   teams: Array with [id, name] values of teams.
  ///   data: Data to show.
  ///   toString: Converter for each datum to a String to show in table.
  public function new (
    wg: Domo,
    teams: Array<Array<String>>,
    data: Array<Array<Option<T>>>,
    toString: (T) -> String
  ) {
    this.wg = wg;
    this.teams = teams;
    this.data = data;
    this.toString = toString;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .style("border-collapse: collapse;")
        .add(Q("tr")
          .add(Q("td"))
          .adds(teams.map(t -> Q("td")
              .klass("pre")
              .add(Ui.img("clubs/" + t[0])
                .klass("badge")
                .att("title", t[1])))))
        .adds(It.range(teams.length).map(row -> Q("tr")
            .add(Q("td")
              .klass("border")
              .style("white-space:nowrap;")
              .add(Q("span")
                .add(Ui.img("clubs/" + teams[row][0])
                .klass("badge")
                .style("vertical-align:middle")))
              .add(Q("span")
                .text(teams[row][1])))
            .adds(It.range(teams.length).map(col -> Q("td")
                .klass("pre")
                .style("white-space: nowrap;")
                .html(col == row
                    ? "***"
                    : switch (data[row][col]) {
                        case Some(r): toString(r);
                        case None: "";
                      })
              ).to())
          ).to()))
    ;
  }

}
