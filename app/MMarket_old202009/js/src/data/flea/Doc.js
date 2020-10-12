// Copyright 12-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Flea models documentation.
**/

const footer = `
  <p><small>q = close<br>rf = referencia de compra o venta</small></p>
  `;


const apprDoc = `
  <pre>
  PARÁMETROS
    ic (%): Inicio de compra [rf = q * (1 + pc)]
    pc (%): Paso de compra [rf' = rf - ((rf - q) * pc)]
    iv (%): Inicio de venta [rf = q * (1 - pv)]
    pv (%): Paso de venta [rf' = rf + ((q - rf) * pv)]

  INICIO
    * Se fija un inicio de venta igual a la primera cotización.

  COMPRA
    * rf va disminuyendo porcentualmente y cuando rf' < q se compra.

  VENTA
    * rf va aumentando porcentualmente y cuando rf' > q se vende.
  </pre>
  ` + footer;

const incrDoc = `
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
  ` + footer;

const maxMinDoc = `
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
  ` + footer;

const gaDoc = `
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
  ` + footer;

/**
    Flea models documentation.
**/
export default class Doc {
  /**
      @param {string} modelId
      @return {string}
  **/
  static read (modelId) {
    switch (modelId) {
    case "APPR": return apprDoc;
    case "INCR": return incrDoc;
    case "MM": return maxMinDoc;
    case "GA": return gaDoc;
    }
    return "<p>Sin documentación</p>";
  }
}
