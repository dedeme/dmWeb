// Copyright 09-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>


package data.flea;

// Flea models documentation.
class Doc {
static final footer = '
<p><small>q = close<br>rf = referencia de compra o venta</small></p>
';

//*******************
static final apprDoc = "
<pre>
PARÁMETROS
  ic (%): Inicio de compra [rf = q * (1 + pc)]
  pc (%): Paso de compra [rf\' = rf - ((rf - q) * pc)]
  iv (%): Inicio de venta [rf = q * (1 - pv)]
  pv (%): Paso de venta [rf' = rf + ((q - rf) * pv)]

INICIO
  * Se fija un inicio de venta igual a la primera cotización.

COMPRA
  * rf va disminuyendo porcentualmente y cuando rf' < q se compra.

VENTA
  * rf va aumentando porcentualmente y cuando rf' > q se vende.
</pre>
" + footer;

//******************
static final incrDoc = "
<pre>
PARÁMETROS
  d     : Días.</li>
  fc (%): Franja de compra
            Referencia base -> [rf = q(-d)]
            Referencia operativa -> [rf' = rf * (1 + fc)]
  fv (%): Franja de venta
            Referencia base -> [rf = q(-d)]
            Referencia operativa -> [rf' = rf * (1 - fv)]

INICIO
  * Se espera 'd' dias y entonces se fija un inicio de venta (rf).

COMPRA
  * Se va disminuyendo rf cuando q(-d) disminuye. En otro caso rf se
    mantiene.
  * Cuando rf' < q se compra.

VENTA
  * Se va aumentando rf cuando q(-d) aumenta. En otro caso rf se mantiene.
    Cuando rf' > q se vende.
</pre>
" + footer;

//********************
static final maxMinDoc = "
<pre>
PARÁMETROS
  bc (%): Banda de compra [rf = maximo del último período * (1 + bc)]
  pc (%): Paso de compra [rf' = rf - ((rf - q) * pc)]
  bv (%): Banda de venta [rf = mínimo del último período * (1 - bv)]
  pv (%): Paso de venta [rf' = rf + ((q - rf) * pv)]

INICIO
  * Se fija una referncia de venta igual a la banda de venta de la primera
    cotización.
  * Se fija un máximo de cotización igual a la primera cotización

COMPRA
  * rf va disminuyendo porcentualmente y cuando rf' < q se compra.
  * Si no se compra y q es menor que el mínimo de cotización, se utiliza q
    como mínimo.
  * Si se compra se fija rf igual a la banda de venta del mínimo de cotización
    y se fija q como máximo de cotización.

VENTA
  * rf va aumentando porcentualmente y cuando rf' > q se vende.
  * Si no se vende y q es mayor que el máximo de cotización, se utiliza q
    como máximo.
  * Si se vende se fija rf igual a la banda de compra del máximo de cotización
    y se fija q como mínimo de cotización.
</pre>
" + footer;

//****************
static final gaDoc = "
<pre>
PARÁMETROS
  di (%): Días de cálculo (admite decimales)
          [rf = rf * di / (di + 1) + q / (di + 1)]
  bc (%): Banda de compra [rf' = rf * (1 + bc)]
  bv (%): Banda de venta [rf' = rf * (1 - bv)]

INICIO
  * Se fija una rf igual a la primera cotización.

COMPRA
  * Se determina rf' y si q > rf' se compra.

VENTA
  * Se determina rf' y si q < rf' se vende.
</pre>
" + footer;

//*****************
static final difDoc = "
<pre>
PARÁMETROS
  dv (%): Diferencia de compra.
  dc (%): Diferencia de venta.

INICIO
  * Se fija una referencia de venta (rf = q * (1 - dv)).

COMPRA
  * Se determina rf' = q * (1 + dc).
  * Si rf' > rf, se mantiene rf' = rf.
  * Si q > rf' se compra y se fija rf = q * (1 - dc); en otro caso se usa rf'
    como la nueva rf.

VENTA
  * Se determina rf' = q * (1 - dv).
  * Si rf' < rf, se mantiene rf' = rf.
  * Si q < rf' se vende y se fija rf = q * (1 + dc); en otro caso se usa rf'
    como la nueva rf.
</pre>
" + footer;

//******************
static final jumpDoc = "
<pre>
PARÁMETROS
  salto (%): Incremento de stops de compras y ventas.

INICIO
  * Se calcula
    - jmp = 1 + salto
    - lgJmp = Log(jmp)
  * Se define:
    - upGap(q) = jmp ^ (round(Log(q) / lgJmp) + 1)
    - downGap2(q) = repeat
                      ref2 = ref * jmp
                      if ref2 * jmp ^ (1/2) >= q -> return ref
                      else ref = ref2
    -downGap(q) = jmp ^ (round(Log(q) / lgJmp) - 1)
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
" + footer;

//******************
static final jmp2Doc = "
<pre>
PARÁMETROS
  salto (%): Incremento de stops de compras y ventas.

INICIO
  * Se calcula
    - jmp = 1 + salto
    - lgJmp = Log(jmp)
  * Se define:
    - downGap(q) = repeat
                      ref2 = ref * jmp
                      if ref2 * jmp ^ (1/2) >= q -> return ref
                      else ref = ref2
    -upGap(q) = repeat
                  ref2 = ref / jmp
                  if ref2 / jmp ^ (1/2) <= q -> return ref
                  else ref = ref2
  * Se fija una referencia de venta (rf = q / jmp / jmp)

COMPRA
  * Si q > rf se compra y se fija rf = q / jmp / jmp
  * En otro caso se calcula rf' = downGap(q) y si rf' < rf se fija rf = rf'.

VENTA
  * Si q < rf se vende y se fija rf = q * jmp * jmp
  * En otro caso se calcula rf' = upGap(q) y si rf' > rf se fija rf = rf'.

</pre>
" + footer;

//******************
static final gapDoc = "
<pre>
PARÁMETROS
  hueco (%): Diferencia máxima entre q y rf

INICIO
  * Se fija una referencia de venta [rf = q * (1 + hueco)]

COMPRA
  * Si q > rf se compra y se fija rf = q * (1 - hueco)
  * En otro caso se calcula rf' = q * (1 + hueco) y si rf' < rf se fija
    rf = rf'.

VENTA
  * Si q < rf se vende y se fija rf = q * (1 + hueco)
  * En otro caso se calcula rf' = q * (1 - hueco) y si rf' > rf se fija
    rf = rf'.

</pre>
" + footer;

// -------------------------------------------------------------------------- //

  /// Returns summary of a model.
  ///   modelId: Model identifier.
  public static function read (modelId) {
    switch (modelId) {
      case "APPR": return apprDoc;
      case "INCR": return incrDoc;
      case "MM": return maxMinDoc;
      case "GA": return gaDoc;
      case "DIF": return difDoc;
      case "JUMP": return jumpDoc;
      case "JMP2": return jmp2Doc;
      case "GAP": return gapDoc;
    }
    return "<p>Sin documentación</p>";
  }
}
