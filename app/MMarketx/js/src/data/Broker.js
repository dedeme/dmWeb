// Copyright 08-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Broker operations.
**/
export default class Broker {
  /**
      Calculate fees of Broker + Market.
      @param {number} amount
  **/
  static fees (amount) {
    let broker = 9.75;
    if (amount > 25000) {
      broker = amount * 0.001;
    }

    let bolsa = 4.65 + amount * 0.00012;
    if (amount > 140000) {
      bolsa = 13.4;
    } else if (amount > 70000) {
      bolsa = 9.2 + amount * 0.00003;
    } else if (amount > 35000) {
      bolsa = 6.4 + amount * 0.00007;
    }

    return broker + bolsa;
  }

  /**
      Returns net cost of operation.
      @param {number} stocks
      @param {number} price
  **/
  static buy (stocks, price) {
    const amount = stocks * price;
    return amount + Broker.fees(amount);
  }

  /**
      Returns net incomes of operation.
      @param {number} stocks
      @param {number} price
  **/
  static sell (stocks, price) {
    const amount = stocks * price;
    return amount - Broker.fees(amount);
  }

}
