// Copyright 14-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Model Fix Quantum documentation.
package qfixDoc

func Get() string {
	return "<h3>Símbolos</h3>" +
		"<pre>" +
		"<i> j</i>: Salto de las referencias<br>" +
		"<i>lj</i>: ln(j)<br>" +
		"<i> q</i>: Última cotización<br>" +
		"<i>q0</i>: Previa cotización<br>" +
		"<i> r</i>: Referencia<br>" +
		"<i> d</i>: (Función) Baja el intervalo cuando la posición cambia a Comprado.<br>" +
		"           <i>d = pow(j, round(log(q) / lj) - 1)</i><br>" +
		"<i> u</i>: (Función) Sube el intervalo cuando la posición cambia a Vendido.<br>" +
		"           <i>d = pow(j, round(log(q) / lj) + 1)</i><br>" +
		"<i>d2</i>: (Función) Sube el intervalo cuando la posición continua Comprada.<br>" +
		"           for (;;) <br>" +
		"             r2 = r * j<br>" +
		"             if (r2 * sqrt(j) >= q) return r<br>" +
		"             r = r2<br>" +
		"<i>u2</i>: (Función) Baja el intervalo cuando la posición continua Vendida.<br>" +
		"           for (;;) <br>" +
		"             r2 = r / j<br>" +
		"             if (r2 / sqrt(j) <= q) return r<br>" +
		"             r = r2<br>" +
		"</pre>" +
		"<h3>Resumen</h3>" +
		"<p>Se dividen las posibles cotizaciones en intervalos.</p>" +
		"<dl>" +
		"<dt>Compra</dt>" +
		"  <dd>Cuando <i>q</i> pasa a un intervalo superior.</dd>" +
		"<dt>Venta</dt>" +
		"  <dd>Cuando <i>q</i> pasa a intervalo inferior.</dd>" +
		"</dl>" +
		"<h3>Proceso</h3>" +
		"<h4>Inicialización</h4>" +
		"<p>Se inicializa <i>r = d(q) / j</i>.</p>" +
		"<h4>Cada día</h4>" +
		"<ul>" +
		"<li>Si <i>q0 >= r</i> &rArr;<br>" +
		"       <ul>" +
		"       <li>Si <i>q > q0</i> &rArr; se recalcula <i>r = u2(q, r)</i>.</li>" +
		"       <li>O si <i>q < r</i> &rArr; se recalcula <i>r = d(q) * j</i> (Se vende).</li>" +
		"       </ul></li>" +
		"<li>Si <i>q0 < r</i> &rArr;</br>" +
		"       <ul>" +
		"       <li>Si <i>q < q0</i> &rArr; se recalcula <i>r = d2(q, r)</i>.</li>" +
		"       <li>O si <i>q > r</i> &rArr; se recalcula <i>r = u(q) / j</i> (Se compra).</li>" +
		"       </ul></li>" +
		"</ul>"
}