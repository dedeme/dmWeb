// Copyright 15-Jul-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package dm;

import js.html.CanvasRenderingContext2D;
import js.html.CanvasElement;
import js.html.CanvasGradient;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Dec;

/// Speedometer widget
class Speedometer {
  public final wg: Domo;
  final ratio: Float;
  final back: String;
  final w: Int;
  final h: Int;
  final ctx: CanvasRenderingContext2D;


  /// Returns a speedometer whose property 'wg' is the DOM object.
  ///   value: A value between 0 and 1, both inclusive.
  ///   ratio: Size ratio of 'wg'. Default: 1.0.
  ///   ?border: A style color like "#6e8296" or "black". If it is null 'wg'
  ///            has not border.
  ///   ?back  : A style color like "#6e8296" or "white". If it is null 'bg'
  ///            has no background.
  public function new (
    value: Float, ratio = 1.0, ?border: String, ?back: String
  ) {
    if (ratio < 0) ratio = 0;
    if (ratio > 1) ratio = 1;
    this.ratio = ratio;
    this.back = back;
    w = Dec.toInt(300 * ratio);
    h = Dec.toInt(170 * ratio);

    var style = "";
    if (border != null) {
      style += "border:1px solid " + border + ";";
    }
    if (back != null) {
      style += "background:" + back + ";";
    }
    wg = Q("canvas")
      .att("width", w)
      .att("height", h)
      .style(style)
    ;

    ctx = cast(wg.e, CanvasElement).getContext2d();

    mkDial();
    mkNeedle(value);
    mkArc();
  }

  function mkDial () {
    function grad (p1: Pt, p2: Pt, c1: String, c2: String): CanvasGradient {
      final grd = ctx.createLinearGradient(
        Dec.toInt(p1.x), Dec.toInt(p1.y),
        Dec.toInt(p2.x), Dec.toInt(p2.y)
      );
      grd.addColorStop(0, c1);
      grd.addColorStop(1, c2);
      return grd;
    }

    ctx.lineWidth = Dec.toInt(50 * ratio);
    final radius = Dec.toInt(120 * ratio);

    function arc (start: Float, end: Float) {
      ctx.beginPath();
      ctx.arc(Dec.toInt(150 * ratio), Dec.toInt(150 * ratio),
      radius,
      start, end,
      false);
      ctx.stroke();
    }

    ctx.strokeStyle = grad(
      new Pt(Dec.toInt(28 * ratio), Dec.toInt(146 * ratio)),
      new Pt(Dec.toInt(65 * ratio), Dec.toInt(64 * ratio)),
      "#40a040", "#4040a0"
    );
    arc(-Math.PI * 1, -Math.PI * 0.65);

    ctx.strokeStyle = grad(
      new Pt(Dec.toInt(65 * ratio), Dec.toInt(64 * ratio)),
      new Pt(Dec.toInt(149 * ratio), Dec.toInt(29 * ratio)),
      "#4040a0", "#a040f0"
    );
    arc(-Math.PI * 0.75, -Math.PI * 0.5);


    ctx.strokeStyle = grad(
      new Pt(Dec.toInt(149 * ratio), Dec.toInt(29 * ratio)),
      new Pt(Dec.toInt(237 * ratio), Dec.toInt(64 * ratio)),
      "#a040f0", "#a040a0"
    );
    arc(-Math.PI * 0.50, -Math.PI * 0.25);

    ctx.strokeStyle = grad(
      new Pt(Dec.toInt(237 * ratio), Dec.toInt(64 * ratio)),
      new Pt(Dec.toInt(273 * ratio), Dec.toInt(146 * ratio)),
      "#a040a0", "#a04040"
    );
    arc(-Math.PI * 0.255, -Math.PI * 0);
  }

  function mkNeedle (value: Float) {
    final angle = Math.PI *  value;

    final dx = Dec.toInt(150 * ratio);
    final dy = Dec.toInt(150 * ratio);
    function getX (p: Pt): Int {
      return p.xCtx(dx);
    }
    function getY (p: Pt): Int {
      return p.yCtx(dy);
    }

    final p1 = new Pt(130 * ratio, 0 * ratio).rotate(angle);
    final p2 = new Pt(0, -16 * ratio).rotate(angle);
    final p3 = new Pt(0, 16 * ratio).rotate(angle);

    ctx.lineJoin = "round";
    ctx.lineWidth = Dec.toInt(4 * ratio);
    ctx.strokeStyle = back;
    ctx.fillStyle   = "black";

    ctx.beginPath();
    ctx.moveTo(getX(p1), getY(p1));
    ctx.lineTo(getX(p2), getY(p2));
    ctx.lineTo(getX(p3), getY(p3));
    ctx.closePath();
    ctx.stroke();
    ctx.fill();

    ctx.lineJoin = "miter";
  }

  function mkArc () {
    ctx.lineWidth = Dec.toInt(6 * ratio);
    ctx.strokeStyle = "#406080";
    ctx.fillStyle   = "black";

    ctx.beginPath();
    ctx.arc(Dec.toInt(150 * ratio), Dec.toInt(150 * ratio),
    Dec.toInt(40 * ratio),
    0, Math.PI,
    true);
    ctx.lineTo(Dec.toInt(110 * ratio), Dec.toInt(165 * ratio));
    ctx.lineTo(Dec.toInt(190 * ratio), Dec.toInt(165 * ratio));
    ctx.closePath();
    ctx.fill();
    ctx.stroke();

    ctx.beginPath();
    ctx.moveTo(Dec.toInt(2 * ratio), Dec.toInt(165 * ratio));
    ctx.lineTo(Dec.toInt(298 * ratio), Dec.toInt(165 * ratio));
    ctx.stroke();

  }

}

class Pt {
  public final x: Float;
  public final y: Float;

  public function new (x: Float, y: Float) {
    this.x = x;
    this.y = y;
  }

  public function xCtx (dx = 0): Int {
    return Dec.toInt(x) + dx;
  }

  public function yCtx (dy = 0): Int {
    return -Dec.toInt(y) + dy;
  }

  /// Clockwise rotation
  public function rotate (angle: Float): Pt {
    final cos = Math.cos(angle);
    final sin = Math.sin(angle);
    return new Pt(
      -x * cos + y * sin,
      y * cos + x * sin
    );
  }
}
