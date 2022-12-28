// Copyright 23-Dic-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import data.Image;
import wgs.Tr;
import I18n._;
import I18n._args;

/// Home page.
class Home {
  final wg: Domo;
  final images: Array<Image>;
  var group = "0";

  function new (wg: Domo, images: Array<Image>) {
    this.wg = wg;
    this.images = images;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    if (images.length == 0) {
      wg
        .removeAll()
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
            .klass("frame")
            .text(_("There are no pictures to adjust")))))
      ;
      return;
    }

    final tdSave = Q("td").klass("frame");
    final trs = [];
    trs.push(Q("tr")
      .add(Q("td")
        .att("colspan", 5)
        .style(
            "pading-top:15px;" +
            "pading-bottom:10px"
          )
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(tdSave))))
    );
    trs.push(Q("tr")
      .add(Q("td")
        .att("colspan", 5)
        .add(Q("hr")))
    );

    mkSave(tdSave);
    for (ix in 0...images.length) {
      trs.push(new Tr(images, ix).mkTr());
    }

    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .adds(trs))
      .add(Cts.box.wg)
    ;
  }

  // Control -------------------------------------------------------------------

  function mkSave (td: Domo): Void {
    td
      .removeAll()
      .add(Ui.img("wait.gif"));
    Global.client.send([
      "prg" => Js.ws(Cts.appName),
      "source" => Js.ws("Home"),
      "rq" => Js.ws("idata2")
    ], rp -> {
      final duplicates = rp["duplicates"].ra().map(d -> d.rs());
      group = rp["group"].rs();

      if (duplicates.length == 0) {
        td
          .removeAll()
          .add(Ui.link(save)
            .klass("link")
            .text(_args(_("Save in Group %0"), [group])))
        ;
      } else {
        td
          .removeAll()
          .html("<p>" + _("There are duplicated pictures") + ":</p><p>" +
              duplicates.join("<br>") + "</p>")
        ;
      }
    });
  }

  function save (): Void {
    if (Ui.confirm(_args(_("Save pictures in group %0?"), [group]))) {
      Cts.boxContent
        .removeAll()
        .add(Ui.img("wait.gif"))
      ;
      Cts.box.show(true);
      Global.client.send([
        "prg" => Js.ws(Cts.appName),
        "source" => Js.ws("Home"),
        "rq" => Js.ws("save"),
        "group" => Js.ws(group)
      ], rp -> {
        js.Browser.location.assign("");
      });
    }
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Global.client.send([
      "prg" => Js.ws(Cts.appName),
      "source" => Js.ws("Home"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final images = rp["images"].ra().map(Image.fromJs);
      new Home(wg, images).show();
    });
  }
}
