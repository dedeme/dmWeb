// Copyright 21-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Maybe from "../dmjs/Maybe.js";

/**
    Pages path management.
**/
export default class Path {
  /**
      @private
      @param {string} path
  **/
  constructor (path) {
    this._path = path;
  }

  /**
      Return the current page. If path is rootPath throw a exception.
      @return {string}
  **/
  get page () {
    const path = this._path;
    if (path === "") throw Error("Path is root");
    const es = path.split("&");
    return es[es.length - 1];
  }

  /**
      The correponding location, including "?".
      @return {string}
  **/
  get location () {
    return (this._path === "") ? "" : "?" + this._path;
  }

  /**
      Returns the next page in the actual url.
      If the actual url does not start with 'this' or is equal to it, returns
      Maybe.nothing.
      @param {string} def Default page.
      @return {!Maybe<string>}
  **/
  nextPage (def) {
    const search = location.search.substring(1);
    const p = this._path;
    if (!search.startsWith(p) && !p.startsWith(search)) return Maybe.nothing;
    const r = p === ""
      ? search.split("&")[0]
      : search.split("&")[p.split("&").length]
    ;
    if (r === undefined) return Maybe.just(def);
    if (r === "") return Maybe.nothing;
    return Maybe.just(r);
  }

  /**
      Changes the last page of path. If Path is root throws a exception.
      @param {string} page It can not be "".
      @return {!Path} A new Path.
  **/
  change (page) {
    if (page === "") throw Error("'page' is empty");
    if (this._path === "") throw Error("Path is root");
    const es = this._path.split("&");
    es[es.length - 1] = page;
    return new Path(es.join("&"));
  }

  /**
      Adds a new page.
      @param {string} page It can not be "".
      @return {!Path} A new Path.
  **/
  add (page) {
    if (page === "") throw Error("'page' is empty");
    return new Path(this._path === "" ? page : this._path + "&" + page);
  }

  /**
      Creates a root path.
  **/
  static mkRoot () {
    return new Path("");
  }
}
