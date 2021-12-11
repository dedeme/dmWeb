// Copyright 29-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>


package data.model;

// Models documentation.
class Doc {
//******************
static final doc = "
<pre>
PARÁMETROS
  salto (%): Incremento de stops de compras y ventas.

INICIO
  * Se calcula
    - d = 1 + salto * {qlevel} / 3
    - jmp = 1 + salto
    - lgJmp = Log(jmp)
  * Se define:
    - upGap(q) = jmp ^ (round(Log(q/d) / lgJmp) + 1) * d
    - downGap2(q) = repeat
                      ref2 = ref * jmp
                      if ref2 * jmp ^ (1/2) >= q -> return ref
                      else ref = ref2
    -downGap(q) = jmp ^ (round(Log(q/d) / lgJmp) - 1) * d
    -upGap2(q) = repeat
                  ref2 = ref / jmp
                  if ref2 / jmp ^ (1/2) <= q -> return ref
                  else ref = ref2
  * Se fija una referencia de venta (rf = downGap(q))

COMPRA
  * Si q > rf se compra y se fija rf = downGap(q) / jmp
  * En otro caso se calcula rf' = downGap2(q) y si rf' < rf se fija rf = rf'.

VENTA
  * Si q < rf se vende y se fija rf = upGap(q) * jmp
  * En otro caso se calcula rf' = upGap2(q) y si rf' > rf se fija rf = rf'.

</pre>
<p><small>q = close<br>rf = referencia de compra o venta</small></p>
";

// -------------------------------------------------------------------------- //

  /// Returns summary of a model.
  ///   qlevel: Model quantum level.
  public static function read (qlevel: Int): String {
    return StringTools.replace(doc, "{qlevel}", "" + qlevel);
  }
}
