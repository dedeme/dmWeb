// Copyright 22-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Store;
import I18n._;
import Cts;

/// Settings page.
class Help {
  static final txEs = "
    <h2>Cambio de ejercicio</h2>
    <p>El cambio de ejercicio se realiza automáticamente al apuntar una
    anotación en un año posterior al de la última.</p>
    <p>Como los informes se realizan teniendo en cuenta únicamente las
    anotaciones del año en curso (página principal) o del seleccionado (Informe
    de Hacienda), es preciso anotar manualmente <i>con fecha 1 de enero</i>
    las existencias finales del año anterior como compras, para que sean
    utilizadas en su elaboración.</p>
    <p>La anotación en el 1 de enero debe ser <i>exclusivamente</i> la
    correspondiente a las referidas existencias finales. Ello es debido a que
    las fichas de almacén se realizan con las anotaciones de todos los años,
    saltándose las correspondientes a los 1 de Enero, pare evitar así
    duplicados.</p>
  ";

  static final txEn = "
    <h2>Change of Year</h2>
    <p>The change of year is made automatically when an annotation of a
    new year is done.</p>
    <p>Because reports are made only with annotations of the current year
    (main page) or annotations of the selected one (Treasury report),
    is necessary to annotate manually <i>with date of the first of Junary</i>
    the final existences of previous year. Oterwise they are not used by
    reports.</p>
    <p>The first of January annotation <i>must include only</i> the final
    existences, because stock forms use annotatations from all the years and
    skip that of the first of January, avoiding in this way to duplicate
    entries.</p>
  ";

  /// Constructor.
  public static function mk (wg: Domo): Void {
    final lang = switch (Store.get(Cts.langKey)) {
      case Some(l): l;
      case None: "es";
    }

    wg
      .removeAll()
      .add(Q("div")
        .style("text-align:center")
        .add(Q("div")
          .klass("head")
          .html(_("Help")))
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .klass("frame")
              .style("Text-align: left")
              .html(lang == "es" ? txEs : txEn)))))
    ;
  }
}
