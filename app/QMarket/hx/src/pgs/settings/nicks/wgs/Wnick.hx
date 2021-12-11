// Copyright 19-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings.nicks.wgs;

import dm.Domo;
import dm.Ui;
import dm.Dec;
import dm.Ui.Q;
import data.Nick;
import I18n._;

/// Nicks list entry widget.
class Wnick {
  // Constructor.
  //    wg     : Container.
  //    nicksPg: Main page.
  //    nick   : Nick
  //    isModel: If its the nick model.
  //    volume : Last quotes average volume.
  public static function mk (
    wg: Domo,
    nicksPg: Nicks,
    nick: Nick,
    isModel: Bool,
    volume: Float
  ) {
    final img = (id, title) -> Ui.img(id).att("title", title);
    final limg = (id, title) -> Ui.lightImg(id).att("title", title);
    final emptyBt = (title) -> Q("div")
      .style("padding:5px;" +
              "border: 1px solid #002040;border-radius: 6px;" +
              "background: #d0ddde;")
      .att("title", title)
    ;

    final model = isModel
      ? img("star", _("Model"))
      : nick.isSel
        ? Ui.link(e -> nicksPg.setModel(nick))
          .add(limg("star2", _("Model")))
        : Ui.link(e -> nicksPg.del(nick))
          .add(img("delete", _("Delete")))
    ;

    final isSel = nick.isSel
      ? isModel
        ? img(
            volume < 1000000
              ? "flag2"
              : "flag1",
            _("Selection")
          )
        : Ui.link(e -> nicksPg.setIsSel(nick))
            .add(img(
              volume < 1000000
                ? "flag2"
                : "flag1",
              _("Selection")
            ))
      : Ui.link(e -> nicksPg.setIsSel(nick))
        .add(emptyBt(_("Selection")))
    ;

    final nk = Ui.link(e -> {
      nicksPg.selectNick(nick);
    }).klass("link").text(nick.name);
    nk.att("title", Dec.toIso(volume, 0));

    wg
      .removeAll()
      .add(Q("table")
        .add(Q("tr")
          .add(Q("td").add(model))
          .add(Q("td").add(isSel))
          .add(Q("td").add(nk))))
    ;
  }
}
