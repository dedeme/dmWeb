// Copyright 06-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// APP must be equals in the server side
const APP = "MyMarket";
const VERSION = "201810";
const LANG_STORE = "${app}__lang";

const SETTINGS_PAGE_ID = "settings";
const BACKUPS_PAGE_ID = "backups";
const HOME_PAGE_ID = "home";
const ANNOTATIONS_PAGE_ID = "annotations";

/** Constans of application. */
export default class Cts {
  /** @return {string} Application name */
  static get APP () {
    return APP;
  }

  /** @return {string} Application version */
  static get VERSION () {
    return VERSION;
  }

  /** @return {string} Key for language data store */
  static get LANG_STORE () {
    return LANG_STORE;
  }

  /** @return {string} Id of settings page */
  static get SETTINGS_PAGE_ID () {
    return SETTINGS_PAGE_ID;
  }

  /** @return {string} Id of backups page */
  static get BACKUPS_PAGE_ID () {
    return BACKUPS_PAGE_ID;
  }

  /** @return {string} Id of home page */
  static get HOME_PAGE_ID () {
    return HOME_PAGE_ID;
  }

  /** @return {string} Id of annotations page */
  static get ANNOTATIONS_PAGE_ID () {
    return ANNOTATIONS_PAGE_ID;
  }
}
