// Copyright 12-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Flea models documentation.
**/

const footer = `
  <p><small>q = close<br>rf = referencia de compra o venta</small></p>
  `;


const apprDoc = `
  <dl>
  <dt>PARÁMETROS</dt>
  <dd><ul>
      <li>ic (%): Inicio de compra [rf = q * (1 + pc)]</li>
      <li>pc (%): Paso de compra [rf' = rf - ((rf - q) * pc)]</li>
      <li>iv (%): Inicio de venta [rf = q * (1 - pv)]</li>
      <li>pv (%): Paso de venta [rf' = rf + ((q - rf) * pv)]</li></ul></dd>
  <dt>INICIO</dt>
  <dd>Se fija un inicio de venta.</dd>
  <dt>COMPRA</dt>
  <dd>rf va disminuyendo porcentualmente y cuando rf < q se compra.</dd>
  <dt>VENTA</dt>
  <dd>rf va aumentando porcentualmente y cuando rf > q se vende.</dd>
  </dl>
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
    }
    return "<p>Sin documentación</p>";
  }
}
