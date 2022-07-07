// Copyright 14-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Model Mobil Quantum documentation.
package qmobDoc

func Get() string {
	return "<h3>Símbolos</h3>" +
		"<pre>" +
		"<i> s</i>: Ratio de separación<br>" +
		"<i> q</i>: Última cotización<br>" +
		"<i> r</i>: Referencia<br>" +
		"<i>r'</i>: Nueva referencia<br>" +
		"</pre>" +
		"<h3>Resumen</h3>" +
		"<dl>" +
		"<dt>Compra</dt>" +
		"  <dd>Cuando <i>q</i> pasa de menor a mayor que <i>r</i>" +
		"<dt>Venta</dt>" +
		"  <dd>Cuando <i>q</i> pasa de mayor a menor que <i>r</i>" +
		"</dl>" +
		"<h3>Proceso</h3>" +
		"<h4>Inicialización</h4>" +
		"<p>Se establece la posición de 'comprado' y se calcula r' = q * (1 - s)</p>" +
		"<h4>Cada día</h4>" +
		"<dl>" +
		"<dt>Si la posición es 'vendido':</dt>" +
		"   <dd><dl>" +
		"   <dt>Si <i>q > r</i>:</dt>" +
		"     <dd><ul>" +
		"       <li>Se cambia a posición 'comprado'</li>" +
		"       <li>Se establece <i>r' = q * (1 - s)</i>" +
		"     </ul></dd>" +
		"   <dt>En otro caso:</dt>" +
		"     <dd>Se establece <i>r' = q * (1 + s)</i> y <i>r' = r' < r ? r' : r</i></dd>" +
		"   </dl></dd>" +
		"<dt>Si la posición es 'comprado':</dt>" +
		"   <dd><dl>" +
		"   <dt>Si <i>q < r</i>:</dt>" +
		"     <dd><ul>" +
		"       <li>Se cambia a posición 'vendido'</li>" +
		"       <li>Se establece <i>r' = q * (1 + s)</i>" +
		"     </ul></dd>" +
		"   <dt>En otro caso:</dt>" +
		"     <dd>Se establece <i>r' = q * (1 - s)</i> y <i>r' = r' > r ? r' : r</i></dd>" +
		"   </dl></dd>" +
		"</dl>"
}
