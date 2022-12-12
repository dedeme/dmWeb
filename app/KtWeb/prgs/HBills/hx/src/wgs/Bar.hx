// Copyright 17-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Immutable graphic bar

package wgs;

import dm.Domo;
import dm.Ui.Q;

class Bar {
  /// Bar width.
  public final width: Int;
  /// Bar height.
  public final height: Int;
  /// Part with foreground.
  public final ratio: Float;
  /// Foreground color (type #000000)
  public final foreground: String;
  /// Background color (type #ffffff)
  public final background: String;

  public function new (
    width = 100,
    height = 2,
    ratio = 0.5,
    foreground = "#0080c0",
    background = "#ffffff"
  ) {
    this.width = width;
    this.height = height;
    this.ratio = ratio;
    this.foreground = foreground;
    this.background = background;
  }

  public function mkWg (): Domo {
    final foreWidth = Std.int(ratio * width);
    return Q("table")
      .att("align", "left")
      .style(
        "border : 1px solid rgb(110,130,150);" +
        "border-collapse : collapse;" +
        "background-color : " + background + ";" +
        "width: " + width + "px;"
      )
      .add(Q("tr")
        .add(Q("td")
          .style(
            "border : 1px solid rgb(110,130,150);" +
            "background-color : " + foreground + ";" +
            "width: " + foreWidth + "px;" +
            "height: " + height + "px;"
          ))
        .add(Q("td")))
    ;
  }

  /// Returns a new Bar copy of 'this' with a new 'width'
  public function withWidth (value: Int): Bar {
    return new Bar(value, height, ratio, background, foreground);
  }

  /// Returns a new Bar copy of 'this' with a new 'height'
  public function withHeight (value: Int): Bar {
    return new Bar(width, value, ratio, background, foreground);
  }

  /// Returns a new Bar copy of 'this' with a new 'ratio'
  public function withRatio (value: Float): Bar {
    return new Bar(width, height, value, background, foreground);
  }

  /// Returns a new Bar copy of 'this' with a new 'background'
  public function withBackground (value: String): Bar {
    return new Bar(width, height, ratio, value, foreground);
  }

  /// Returns a new Bar copy of 'this' with a new 'foreground'
  public function withForeground (value: String): Bar {
    return new Bar(width, height, ratio, background, value);
  }
}
