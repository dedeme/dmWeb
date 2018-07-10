// Copyright 10-Mar-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("module_ModuleHelp");
goog.provide("module_HelpFinal");

module_HelpFinal = class {
  /**
   * @param {string} name
   * @param {string} help
   * @param {string} code
   * @param {string} link
   */
  constructor (name, help, code, link) {
    /** @private */
    this._name = name;
    /** @private */
    this._help = help;
    /** @private */
    this._code = code;
    /** @private */
    this._link = link;
  }

  /** @return {string} */
  name () {
    return this._name;
  }

  /** @return {string} */
  help () {
    return this._help;
  }

  /** @return {string} */
  code () {
    return this._code;
  }

  /** @return {string} */
  link () {
    return this._link;
  }
}

module_ModuleHelp = class {
  /**
   * @param {string} name
   * @param {string} help
   */
  constructor (name, help) {
    /** @private */
    this._name = name;
    /** @private */
    this._help = help;
    /**
     * @private
     * @type {string}
     */
    this._link = "";
    /**
     * @private
     * @type {string}
     */
    this._code = "";
    /**
     * @private
     * @type {!Array<!module_HelpFinal>}
     */
    this._defines = [];
    /**
     * @private
     * @type {!Array<!module_HelpFinal>}
     */
    this._enums = [];
    /**
     * @private
     * @type {!Array<!module_HelpFinal>}
     */
    this._structs = [];
    /**
     * @private
     * @type {!Array<!module_HelpFinal>}
     */
    this._typedefs = [];
    /**
     * @private
     * @type {!Array<!module_HelpFinal>}
     */
    this._unions = [];
    /**
     * @private
     * @type {!Array<!module_HelpFinal>}
     */
    this._functions = [];
    /**
     * @private
     * @type {!Array<!module_HelpFinal>}
     */
    this._vars = [];

  }

  /** @return {string} */
  name () {
    return this._name;
  }

  /** @return {string} */
  help () {
    return this._help;
  }

  /** @param {string} value */
  setHelp (value) {
    this._help = value;
  }

  /** @return {string} */
  link () {
    return this._link;
  }

  /** @return {string} */
  code () {
    return this._code;
  }

  /** @return {!Array<!module_HelpFinal>} */
  defines () {
    return this._defines;
  }

  /** @return {!Array<!module_HelpFinal>} */
  enums () {
    return this._enums;
  }

  /** @return {!Array<!module_HelpFinal>} */
  structs () {
    return this._structs;
  }

  /** @return {!Array<!module_HelpFinal>} */
  typedefs () {
    return this._typedefs;
  }

  /** @return {!Array<!module_HelpFinal>} */
  unions () {
    return this._unions;
  }

  /** @return {!Array<!module_HelpFinal>} */
  functions () {
    return this._functions;
  }

  /** @return {!Array<!module_HelpFinal>} */
  vars () {
    return this._vars;
  }

}
