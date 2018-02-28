// Copyright 03-Jan-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("paths_View");

paths_View = class {
  /** @param {!paths_Control} control */
  constructor (control) {
    /** @const {!paths_Control} */
    this._control = control;
  }

  /** @return {void} */
  show () {
    const control = this._control;
    const paths = control.paths();
    const upControl = control.control();
    const conf = upControl.conf();

    upControl.dom().show(paths, "", $("div")
      .add($("h2")
        .att("align", "center")
        .text(_("Libraries")))
      .add(
        $("table")
          .att("class", "border")
          .att("border", "0")
          .att("align", "center")
          .att("style", "background-color: rgb(255, 250, 250)")
          .add($("tr")
            .add($("td")
              .add(Dom.img("new").att("style", "vertical-align:-15%")))
            .add($("td")
              .att("id", "newEnter")
              .att("colspan", "2")
              .att("align", "center")
              .add($("button")
                .att("id", "newEnterBt")
                .add(Dom.img("enter").att("style", "vertical-align:-10%"))
                .on("click", ev => {
                    control.newPath(
                      $("#nameIn").value(), $("#pathIn").value()
                    );
                  })))
            .add($("td").att("id", "newName")
              .add(Ui.field("pathIn").att("id", "nameIn").att("size", "20")))
            .add($("td").att("id", "newPath")
              .add(Ui.field("newEnterBt")
                .att("id", "pathIn").att("size", "60")))
            .add($("td")))
          .add($("tr")
            .add($("td").att("id", "titleInOut").att("width", "18px")
              .add(
                Ui.link(ev => { control.changeShowAll(); })
                  .add(Dom.img(conf.showAll() ? "out" : "in"))
              ))
            .add($("td").add(Dom.img("blank")).att("width", "18px"))
            .add($("td").add(Dom.img("blank")).att("width", "18px"))
            .add($("td").html("&nbsp;&nbsp;<b>" + _("Name") + "</b>"))
            .add($("td").html("&nbsp;&nbsp;<b>" + _("Path") + "</b>"))
            .add($("td").add(Dom.img("blank")))
            )
          .addIt(
            (paths.length > 0
            ? It.from(paths).filter(p =>
                p.show() || conf.showAll()
              ).sortf((p1, p2) =>
                p1.id().toUpperCase() > p2.id().toUpperCase() ? 1 : -1
              ).map(entry => {
                let id = entry.id();
                let path = entry.path();
                let sel = entry.show();
                let error = !entry.valid();

                return $("tr")
                  .add($("td").att("id", id + ":InOut")
                    .add(Ui.link(ev => {
                      control.selPath(id, sel ? false : true, error);
                    }).add(Dom.img(sel ? "out" : "in"))))
                  .add($("td").att("id", id + ":Modify")
                    .style("text-align:center;")
                    .add(
                      Ui.link(ev => {
                        control.modifyBegin(id);
                      })
                        .add(Dom.img("edit"))
                    ))
                  .add($("td").att("id", id + ":Delete")
                    .style("text-align:center;")
                    .add(
                      Ui.link(ev => { control.deletePath(id); })
                        .add(Dom.img("delete"))
                    ))
                  .add(
                    $("td").att("class", "border").att("id", id + ":Name")
                      .text(
                        id.length > 20 ? id.substring(0, 17) + "..." : id
                      )
                  )
                  .add(
                    $("td").att("class", "border").att("id", id + ":Path")
                      .text(
                        path.length > 60 ? path.substring(0, 57) + "..." : path
                      )
                  )
                  .add(
                    $("td").att("id", id + ":Error")
                      .add(error ? Dom.img("error") : Dom.img("well"))
                  );
              })
            : It.from([
                $("tr")
                  .add($("td")
                    .att("colspan", "6")
                    .att("align", "center")
                    .att("class", "border")
                    .text(_("There are no libraries")))
              ])
            )
          )
        )
      .add($("p").att("style", "text-align:center")
        .add(Ui.link(ev => { control.changeLang(); })
          .att("class", "link")
          .html(_args(_("Change Language to %0"),
            conf.lang() === "es" ? "EN" : "ES")))
        .add($("span").html("&nbsp;|&nbsp;"))
        .add(Ui.link(ev => { control.changePassPage(); })
          .att("class", "link")
          .html(_("Change Password")))
      )
    );
    $("#nameIn").e().focus();
  }

  /**
   * @param {string} id
   * @return {void}
   */
  modify (id) {
    const control = this._control;
    const conf = this._control.control().conf();
    const paths = this._control.paths();
    const pathsIt =  It.from(paths).filter(p => p.show() || conf.showAll());

    $("#newEnter").removeAll().add(
      Dom.lightImg("enter").att("style", "vertical-align:-12%")
    );
    $("#nameIn").value("").disabled(true);
    $("#pathIn").value("").disabled(true);

    $("#titleInOut").removeAll().add(
      Dom.lightImg(conf.showAll() ? "out" : "in")
    );

    pathsIt.each(function (p) {
      const pId = p.id();
      const path = p.path();
      const show = p.show();

      if (pId === id) {
        $("#" + pId + ":InOut").removeAll().add(Dom.img("blank"));
        $("#" + pId + ":Modify").removeAll()
          .add(Ui.link(function (ev) {
              location.assign("");
            }).add(Dom.img("cancel")));
        $("#" + pId + ":Delete").removeAll()
          .add(Ui.link(function (ev) {
            control.modifyPath(
              pId,
              $("#nameModify").value(),
              path,
              $("#pathModify").value()
            );
          })
            .add(Dom.img("enter")));
        $("#" + pId + ":Name").removeAll()
          .add(Ui.field("pathModify")
            .att("id", "nameModify")
            .att("size", "20")
            .value(pId));
        $("#" + pId + ":Path").removeAll()
          .add(Ui.field("nameModify")
            .att("id", "pathModify")
            .att("size", "60")
            .value(path));
        $("#nameModify").e().focus();
      } else {
        $("#" + pId + ":InOut").removeAll().add(Dom.lightImg(
          show ? "out" : "in"
        ));
        $("#" + pId + ":Modify").removeAll().add(Dom.lightImg("edit"));
        $("#" + pId + ":Delete").removeAll().add(Dom.lightImg("delete"));
      }
    });
  }
}
