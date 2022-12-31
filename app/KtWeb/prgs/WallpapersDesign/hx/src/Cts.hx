// Copyright 15-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Client;
import dm.Ui.Q;
import I18n._;
import data.Dim;

/// Constants.
class Cts {
  /// Application name.
  public static final appName = "WallpapersDesign";
  /// Application version.
  public static var version(default, null) = "202212";
  /// Page foot.
  public static final foot = Q("table")
    .klass("main")
    .add(Q("tr")
      .add(Q("td")
        .add(Q("hr"))))
    .add(Q("tr")
      .add(Q("td")
        .style("text-align: right;color:#808080;font-size:x-small;")
        .html('- © ºDeme. KtWeb:${appName} (${version}) -')))
  ;

  /// Content of box to show adjustment widgets
  public static final boxContent = Q("div");

  /// Box to show adjustment widgets.
  public static final box = new dm.ModalBox(boxContent, false);

  /// Pixels for default cut operation.
  public static final pixelsCut = 0;

  /// Ratio for default blur operation.
  public static final ratioBlur = 85;

  /// Available dimensions for target images.
  public static final dims = [
    "1280 x 1024" => new Dim(1280, 1024),
    "1920 x 1080" => new Dim(1920, 1080)
  ];
}
