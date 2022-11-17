// Copyright 17-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package dm;

import dm.Opt;
import dm.Domo;
import dm.Ui.Q;
import dm.Ui;
import dm.It;

/// Menu entry
class MenuEntry {
  /// Menu entry identifier.
  public var id(default, null): Option<String>;
  /// Menu entry widget.
  public var wg(default, null): Domo;

  /// Constructor.
  ///   id: Entry identifier. If the entry is not selectable its value must
  ///       be 'None'.
  ///   wg: Widget which will be showed.
  public function new (id: Option<String>, wg: Domo) {
    this.id = id;
    this.wg = wg;
  }
}

/// Menu widget.
class Menu {
  /// Menu entry widget.
  public var wg(default, null): Domo = Q("div");

  /// Constructor.
  ///   lopts        : Left options.
  ///   ropts        : Right options.
  ///   selected     : Identifier of the initial selected option. It it does
  ///                  not match any identifier, no one will be selected.
  ///   withSeparator: If its value is 'true' a vertical bar separates 'lopts'
  ///                  and 'ropts' (Default 'false').
  public function new (
    lopts: Array<MenuEntry>, ropts: Array<MenuEntry>,
    selected: String, withSeparator: Bool = false
  ) {
    final setId = o -> {
      switch (o.id) {
        case Some(v): o.wg.style(v == selected
          ?
            "background-color: rgb(250, 250, 250);" +
            "border: 1px solid rgb(110,130,150);" +
            "padding: 4px;border-radius: 4px;"
          : "text-decoration: none;color: #000080;" +
            "font-weight: normal;cursor:pointer;"
          );
        case None:
      }
    }
    for (o in lopts) setId(o);
    for (o in ropts) setId(o);

    // View --------------------------------------------------------------------
    wg
      .add(Q("table")
        .style("border-collapse:collapse;width:100%;")
        .add(Q("tr")
          .add(Q("td")
            .style(
              "text-align:left;padding-right:4px;" +
              "${withSeparator ? 'border-right: 1px solid #000000;' : ''}"
            )
            .adds(It.from(lopts).map(e -> e.wg).to()))
          .add(Q("td")
            .style(
              "padding-left:4px;vertical-align:top;" +
              "text-align:right;white-space:nowrap"
            )
            .adds(It.from(ropts).map(e -> e.wg).to()))))
      .add(Q("hr"))
    ;
  }

  // Static --------------------------------------------------------------------

  /// Separator ·
  public static function separator (): MenuEntry {
    return new MenuEntry(None, Q("span").text(" · "));
  }

  /// Separator |
  public static function separator2 (): MenuEntry {
    return new MenuEntry(None, Q("span").text(" | "));
  }

  /// Option type text.
  ///   id: Identifier.
  ///   tx: Html text to show.
  ///   f : Function on click.
  public static function toption (
    id: String, tx: String, f: Void -> Void
  ): MenuEntry {
    return new MenuEntry(Some(id), Ui.link(_ -> f()).html(tx));
  }

  /// Option type text.
  ///   id : Identifier.
  ///   img: Image to show. It must be placed in a directory named 'img'.
  ///        If it has not extension, '.png' will be used.
  ///   f  : Function on click.
  public static function ioption (
    id: String, img: String, f: Void -> Void
  ): MenuEntry {
    return new MenuEntry(
      Some(id),
      Ui.link(_ -> f())
        .add(Ui.img(img)
          .style("vertical-align:middle"))
    );
  }

  /// Option type link.
  /// Link is formed
  ///   '"?" + module + "&" + id'
  /// or
  ///   '"?" + id'
  /// if module is null.
  ///   id: Identifier.
  ///   tx: Html text to show.
  ///   module : Module or null (Default 'null').
  public static function tlink (
    id: String, tx: String, module: Null<String> = null
  ): MenuEntry {
    return new MenuEntry(
      Some(id),
      Q("a")
        .att("href", "?" + (module == null ? "" : module + "&") + id)
        .html(tx)
    );
  }

  /// Option type link.
  /// Link is formed
  ///   '"?" + module + "&" + id'
  /// or
  ///   '"?" + id'
  /// if module is null.
  ///   id: Identifier.
  ///   img: Image to show. It must be placed in a directory named 'img'.
  ///        If it has not extension, '.png' will be used.
  ///   module : Module or null (Default 'null').
  public static function ilink (
    id: String, img: String, ?module: String
  ): MenuEntry {
    return new MenuEntry(
      Some(id),
      Q("a")
        .att("href", "?" + (module == null ? "" : module + "&") + id)
        .add(Ui.img(img)
          .style("vertical-align:middle"))
    );
  }

  /// Option close (x)
  ///   fbye: Function on click.
  public static function close (fbye: Void -> Void): MenuEntry {
    return new MenuEntry(
      None,
      Ui.link(_ -> fbye())
        .add(Ui.img("cross")
          .style("vertical-align:middle"))
    );
  }

}
