// Copyright 14-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Mobil Average documentation.
package mmDoc

func Get() string {
	return "<h3>Símbolos</h3>" +
		"<pre>" +
		"<i>ds</i>: Dias de la media<br>" +
		"<i> &Delta;</i>: Banda de incremento" +
		"<i> q</i>: Última cotización<br>" +
		"<i>q'</i>: Cotización de <i>hoy - ds</i><br>" +
		"<i> r</i>: Referencia<br>" +
		"<i>r'</i>: Nueva referencia<br>" +
		"</pre>" +
		"<h3>Resumen</h3>" +
		"<dl>" +
		"<dt>Compra</dt>" +
		"  <dd>Cuando <i>q</i> pasa de menor a mayor que <i>q' * (1 + &Delta;)</i>" +
		"<dt>Venta</dt>" +
		"  <dd>Cuando <i>q</i> pasa de mayor a menor que <i>q' * (1 - &Delta;)</i>" +
		"</dl>" +
		"<h3>Proceso</h3>" +
		"<h4>Inicialización</h4>" +
		"<p>Durante los días del parámetro 'dia' se calcula las media artimética" +
		"   de los cierres. Se devuelve <i>q * (1 - &Delta;)</i> como referencia.</p>" +
		"<dl>" +
		"<dt>Si <i>q < q'</i></dt>" +
		"   <dd><ul>" +
		"     <li>Se establece la posición 'vendido'</li>" +
		"     <li>Se calcula r' = q' * (1 + &Delta;)</li>" +
		"   </ul></dd>" +
		"<dt>En otro caso</dt>" +
		"   <dd><ul>" +
		"     <li>Se establece la posición 'comprado'</li>" +
		"     <li>Se calcula r' = q' * (1 - &Delta;)</li>" +
		"   </ul></dd>" +
		"</dl>" +
		"<h4>Cada día</h4>" +
		"<dl>" +
		"<dt>Si la posición es 'vendido':</dt>" +
		"   <dd><dl>" +
		"   <dt>Si <i>q > r</i>:</dt>" +
		"     <dd><ul>" +
		"       <li>Se cambia a posición 'comprado'</li>" +
		"       <li>Se establece <i>r' = q' * (1 - &Delta;)</i>" +
		"     </ul></dd>" +
		"   <dt>En otro caso:</dt>" +
		"     <dd>Se establece <i>r' = q' * (1 + &Delta;)</i> y r' = r' < r ? r' : r.</dd>" +
		"   </dl></dd>" +
		"<dt>Si la posición es 'comprado':</dt>" +
		"   <dd><dl>" +
		"   <dt>Si <i>q < r</i>:</dt>" +
		"     <dd><ul>" +
		"       <li>Se cambia a posición 'vendido'</li>" +
		"       <li>Se establece <i>r' = q' * (1 + &Delta;)</i>" +
		"     </ul></dd>" +
		"   <dt>En otro caso:</dt>" +
		"     <dd>Se establece <i>r' = q' * (1 - &Delta;)</i> y r' = r' > r ? r' : r.</dd>" +
		"   </dl></dd>" +
		"</dl>"
}
