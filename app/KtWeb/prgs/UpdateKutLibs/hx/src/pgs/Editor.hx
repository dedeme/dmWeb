// Copyright 09-Feb-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Opt;
import data.Lpath;
import I18n._;
import I18n._args;

/// Editor of links to library
class Editor {
  final wg: Domo;
  final lpath: Lpath;
  final llist: Array<Lpath>;
  final list: Array<String>;

  final selector: Domo;
  final checks: Array<Domo>;

  function new (
    wg: Domo, lpath: Lpath, llist: Array<Lpath>, list: Array<String>
  ) {
    list.sort((e1, e2) -> dm.Str.compare(e1.toUpperCase(), e2.toUpperCase()));

    this.wg = wg;
    this.lpath = lpath;
    this.llist = llist;
    this.list = list;

    this.selector = Ui.select("Llibs", ["+ "].concat(llist.map(e -> e.id)));
    this.checks = list.map(e -> Q("input").att("type", "checkbox"));
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("width: 5px;white-space:nowrap;")
            .add(Q("div")
              .klass("head")
              .text(_args(_("Links to '%0'"), [lpath.id]))))
          .add(Q("td")
            .style("text-align: right")
            .add(Ui.link(toList)
              .klass("link")
              .text(_("List"))))))
      .add(Q("hr"))
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:left")
            .add(Ui.link(select)
              .klass("link")
              .text(_("Select All")))
            .add(Q("span").html("&nbsp;|&nbsp;"))
            .add(Ui.link(deselect)
              .klass("link")
              .text(_("Deselect All"))))
          .add(Q("td")
            .style("text-align: right")
            .add(Ui.link(link)
              .klass("link")
              .text(_("New link to")))
            .add(Q("span").html("&nbsp;&nbsp;"))
              .add(selector))))
      .add(Q("hr"))
      .add(Q("table")
        .att("align", "center")
        .adds(checks.length > 0
          ? It.range(checks.length).map(i -> Q("tr")
              .add(Q("td")
                .add(checks[i]))
              .add(Q("td")
                .text(list[i]))
            )
          : [
              Q("tr")
                .add(Q("td")
                  .att("colspan", 2)
                  .klass("frame")
                  .style("text-align:center")
                  .text(_("Without Links")))
            ]
        ))
    ;
  }

  // Control -------------------------------------------------------------------

  function toList (): Void {
    js.Browser.location.assign("?");
  }

  function select (): Void {
    for (ch in checks) ch.checked(true);
  }

  function deselect (): Void {
    for (ch in checks) ch.checked(false);
  }

  function link (): Void {
    final id = selector.getValue();
    if (id == "") {
      Ui.alert(_("Library target is missing"));
      return;
    }
    final slist: Array<String> = [];
    It.from(checks).eachIx((ch, i) ->
      if (ch.getChecked()) slist.push(list[i])
    );
    if (slist.length == 0) {
      Ui.alert(_("No link has been selected"));
      return;
    }
    var target: Option<String> = None;
    for (lp in llist) if (lp.id == id) target = Some(lp.fpath);

    Global.client.ssend([
      "prg" => Js.ws("UpdateKutLibs"),
      "source" => Js.ws("Editor"),
      "rq" => Js.ws("update"),
      "links" => Js.wa(slist.map(Js.ws)),
      "target" => Js.ws(Opt.get(target))
    ], rp -> {
      final linksFailed = rp["linksFailed"].ra().map(e -> e.rs());
      if (linksFailed.length != 0) {
        Ui.alert(_args(
          _("There was errors linking:\n%0"),
          [linksFailed.join("\n")]
        ));
      }
      mk(wg, lpath);
    });

  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo, lpath: Lpath): Void {
    Global.client.send([
      "prg" => Js.ws("UpdateKutLibs"),
      "source" => Js.ws("Editor"),
      "rq" => Js.ws("idata"),
      "lpath" => lpath.toJs()
    ], rp -> {
      final llist = rp["llist"].ra().map(Lpath.fromJs);
      final list = rp["list"].ra().map(e -> e.rs());
      new Editor(wg, lpath, llist, list).show();
    });
  }
}
