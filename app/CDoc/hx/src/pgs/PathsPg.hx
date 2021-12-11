// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

using StringTools;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import dm.Js;
import I18n._;
import I18n._args;
import data.Conf;
import data.Dpath;
import pgs.ChangePass;

/// Paths page.
class PathsPg {
  final wg: Domo;
  final cf: Conf;
  final paths: Array<Dpath>;

  public function new (wg: Domo, cf: Conf, paths: Array<Dpath>) {
    this.wg = wg;
    this.cf = cf;
    this.paths = paths;
  }

  // View ----------------------------------------------------------------------

  public function show () {
    wg
      .removeAll()
      .add(Q("h2")
        .att("align", "center")
        .text(_("Libraries")))
      .add(
        Q("table")
          .att("class", "border")
          .att("border", "0")
          .att("align", "center")
          .att("style", "background-color: rgb(255, 250, 250)")
          .add(Q("tr")
            .add(Q("td")
              .add(Ui.img("new")
                .att("style", "vertical-align:-15%")))
            .add(Q("td")
              .att("id", "newEnter")
              .att("colspan", "2")
              .att("align", "center")
              .add(Q("button")
                .style("width:80px")
                .att("id", "newEnterBt")
                .add(Ui.img("enter")
                  .att("style", "vertical-align:-10%"))
                .on(CLICK, () ->
                    newPath(Q("#autofocus").getValue(), Q("#pathIn").getValue())
                  )))
            .add(Q("td")
              .att("id", "newName")
              .add(Ui.field("pathIn")
                .att("id", "autofocus").att("size", "20")))
            .add(Q("td")
              .att("id", "newPath")
              .add(Ui.field("newEnterBt")
                .att("id", "pathIn")
                .att("size", "60")))
            .add(Q("td")))
          .add(Q("tr")
            .add(Q("td")
              .att("id", "titleInOut")
              .att("width", "18px")
              .add(Ui.link(changeShowAll)
                .add(Ui.img(cf.showAll ? "out" : "in"))))
            .add(Q("td")
              .add(Ui.img("blank"))
              .att("width", "18px"))
            .add(Q("td")
              .add(Ui.img("blank"))
                .att("width", "18px"))
            .add(Q("td")
              .html("&nbsp;&nbsp;<b>" + _("Name") + "</b>"))
            .add(Q("td")
              .html("&nbsp;&nbsp;<b>" + _("Path") + "</b>"))
            .add(Q("td")
              .add(Ui.img("blank"))))
          .adds(
            paths.length > 0
              ? It.from(paths).filter(p ->
                p.isShown || cf.showAll
              ).sort((p1, p2) ->
                p1.id.toUpperCase() > p2.id.toUpperCase() ? 1 : -1
              ).map(entry -> {
                  final id = entry.id;
                  final path = entry.path;
                  final sel = entry.isShown;
                  final error = !entry.isValid;

                  return Q("tr")
                    .add(Q("td")
                      .att("id", id + ":InOut")
                      .add(Ui.link(() -> changeShown(id, error))
                        .add(Ui.img(sel ? "out" : "in"))))
                    .add(Q("td")
                      .att("id", id + ":Modify")
                      .style("text-align:center;")
                      .add(Ui.link(() -> modifyBegin(id))
                        .add(Ui.img("edit"))))
                    .add(Q("td")
                      .att("id", id + ":Delete")
                      .style("text-align:center;")
                      .add(Ui.link(() -> this.deletePath(id))
                          .add(Ui.img("delete"))))
                    .add(Q("td")
                      .att("class", "border")
                      .att("id", id + ":Name")
                      .text(id.length > 20 ? id.substring(0, 17) + "..." : id))
                    .add(Q("td")
                      .att("class", "border")
                      .att("id", id + ":Path")
                      .text(path.length > 60
                          ? path.substring(0, 57) + "..."
                          : path
                        ))
                    .add(Q("td")
                      .att("id", id + ":Error")
                      .add(error ? Ui.img("error") : Ui.img("well")))
                  ;
                }).to()
              : [Q("tr")
                .add(Q("td")
                  .att("colspan", "6")
                  .att("align", "center")
                  .att("class", "border")
                  .text(_("There are no libraries")))
              ]))
      .add(Q("p")
        .att("style", "text-align:center")
        .add(Ui.link(changeLang)
          .att("class", "link")
          .html(_args(_("Change Language to %0"),
            [cf.lang == "es" ? "EN" : "ES"])))
        .add(Q("span")
          .html("&nbsp;|&nbsp;"))
        .add(Ui.link(changePassPage)
          .att("class", "link")
          .html(_("Change Password"))))
    ;
  }

  function modifyBegin (id: String): Void {
    Q("#newEnter")
      .removeAll()
      .add(Q("div")
        .style("width:80px")
        .add(Ui.lightImg("enter")
          .att("style", ";vertical-align:-12%")))
    ;
    Q("#autofocus").value("").disabled(true);
    Q("#pathIn").value("").disabled(true);
    Q("#titleInOut")
      .removeAll()
      .add(Ui.lightImg(cf.showAll ? "out" : "in"))
    ;

    for (p in paths) {
      final pId = p.id;
      final path = p.path;
      final isShown = p.isShown;

      if (pId == id) {
        Q("#" + pId + ":InOut")
          .removeAll()
          .add(Ui.img("blank"))
        ;
        Q("#" + pId + ":Modify")
          .removeAll()
          .add(Ui.link(Main.main)
            .add(Ui.img("cancel")))
        ;
        Q("#" + pId + ":Delete")
          .removeAll()
          .add(Ui.link(() ->
            modifyPath(
              pId,
              Q("#nameModify").getValue(),
              Q("#pathModify").getValue()
            )
          ).add(Ui.img("enter")))
        ;
        Q("#" + pId + ":Name")
          .removeAll()
          .add(Ui.field("pathModify")
            .att("id", "nameModify")
            .att("size", "20")
            .value(pId))
        ;
        Q("#" + pId + ":Path")
          .removeAll()
          .add(Ui.field("nameModify")
            .att("id", "pathModify")
            .att("size", "60")
            .value(path))
        ;
        Q("#nameModify").e.focus();
      } else {
        Q("#" + pId + ":InOut")
          .removeAll()
          .add(Ui.lightImg(isShown ? "out" : "in"))
        ;
        Q("#" + pId + ":Modify").removeAll().add(Ui.lightImg("edit"));
        Q("#" + pId + ":Delete").removeAll().add(Ui.lightImg("delete"));
      }
    }
  }

  // Control -------------------------------------------------------------------

  // Returns "" is ok, otherwise an error.
  function validateChar (chars: String, id: String): String {
    for (i in 0...chars.length) {
      final ch = chars.charAt(i);
      if (id.indexOf(ch) != -1)
        return _args(_("Name '%0' contains '%1'"), [id, ch]);
    }
    return "";
  }

  // Returns "" is ok, otherwise an error.
  function validateId (id: String): String {
    final r = id == ""
      ? _("Name is missing")
      : id.indexOf(" ") != -1
        ? _args(_("Name '%0' contains blanks"), [id])
        : validateChar("=@/?", id)
    ;
    return r == ""
      ? It.from(paths).some(p -> p.id == id)
        ? _args(_("Name '%0' is repeated"), [id])
        : ""
      : r
    ;
  }

  // Returns "" is ok, otherwise an error.
  function validatePath (path: String): String {
    if (!path.startsWith("/"))
      return _args(_("Path '%0' does not start with '/'"), [path]);
    while (path.length > 1 && path.endsWith("/"))
      path = path.substring(0, path.length - 1);
    return path == "/"
      ? _("Path is '/'")
      : path == ""
        ? _("Path is missing")
        : ""
    ;
  }

  // Returns "" is ok, otherwise an error.
  function validateIdPath (id: String, path: String): String {
    final err = validateId(id);
    return err != "" ? err : validatePath(path);
  }

  function newPath (id: String, path: String): Void {
    final err = validateIdPath(id, path);
    if (err != "") {
      Ui.alert(err);
      return;
    }
    Cts.client.ssend([
      "source" => Js.ws("PathsPg"),
      "rq" => Js.ws("new"),
      "id" => Js.ws(id),
      "path" => Js.ws(path)
    ], rp -> {
      Main.main();
    });
  }

  function changeShowAll (): Void {
    Cts.client.ssend([
      "source" => Js.ws("PathsPg"),
      "rq" => Js.ws("changeShowAll")
    ], rp -> {
      Main.main();
    });
  }

  function changeShown (id: String, error: Bool): Void {
    if (error) {
      Ui.alert(_("This source can not be selected, because it does not exist"));
      return;
    }
    Cts.client.ssend([
      "source" => Js.ws("PathsPg"),
      "rq" => Js.ws("changeShown"),
      "id" => Js.ws(id)
    ], rp -> {
      Main.main();
    });
  }

  function deletePath (id: String): Void {
    if (!Ui.confirm(_args(_("Delete %0?"), [id]))) return;
    Cts.client.ssend([
      "source" => Js.ws("PathsPg"),
      "rq" => Js.ws("delete"),
      "id" => Js.ws(id)
    ], rp -> {
      Main.main();
    });
  }

  function changeLang (): Void {
    Cts.client.ssend([
      "source" => Js.ws("PathsPg"),
      "rq" => Js.ws("changeLang")
    ], rp -> {
      Main.main();
    });
  }

  function changePassPage (): Void {
    new ChangePass(wg, show).show();
  }

  function modifyPath (id: String, newId: String, path: String): Void {
    final err = id == newId
      ? validatePath(path)
      : validateIdPath(newId, path)
    ;
    if (err != "") {
      Ui.alert(err);
      return;
    }
    Cts.client.ssend([
      "source" => Js.ws("PathsPg"),
      "rq" => Js.ws("modify"),
      "id" => Js.ws(id),
      "newId" => Js.ws(newId),
      "path" => Js.ws(path),
    ], rp -> {
      Main.main();
    });
  }
}
