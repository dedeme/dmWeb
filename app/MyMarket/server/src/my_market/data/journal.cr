# Copyright 10-Oct-2017 ºDeme
# GNU General Public License - V3 <http://www.gnu.org/licenses/>

require "market_lib"
require "json"
require "./ledger"
require "./inventory"
require "./ann"

include MarketLib

module MyMarket
  private def mk_error (err : String) : NamedTuple(
    error: String,
    ledger: Ledger,
    inventory: Inventory
  )
    {error: err, ledger: Ledger.new, inventory: Inventory.new}
  end

  class Journal
    getter anns = [] of Ann

    def initialize
    end

    # Adds an annotation.
    #
    # If operation fails returns:
    # * 0date (e.g. "020181009") if nick is not in inventory when selling.
    # * 1date (e.g. "120181009") if trying to sell more stocks than there
    #   are in inventory.
    def add(ann : Ann) : String
      date = ann.date
      new_anns = [] of Ann
      not_added = true
      @anns.each do |a|
        if not_added && a.date > date
          new_anns << ann
          not_add = false
        end
        new_anns << a
      end
      new_anns << ann if not_added

      old_anns = @anns
      @anns = new_anns
      error, _, _ = make_books

      if error != ""
        @anns = old_anns
      end

      error
    end

    # Remove an annotation.
    #
    # If operation fails returns:
    # * 0date (e.g. "020181009") if nick is not in inventory when selling.
    # * 1date (e.g. "120181009") if trying to sell more stocks than there
    #   are in inventory.
    def remove(ix : Int32) : String
      new_anns = [] of Ann
      @anns.each_with_index do |a, i|
        new_anns << a if i != ix
      end

      old_anns = @anns
      @anns = new_anns
      error, _, _ = make_books

      if error != ""
        @anns = old_anns
      end

      error
    end

    def modify(ix : Int32, ann : Ann) : String
      new_anns = [] of Ann
      @anns.each_with_index do |a, i|
        if i == ix
          new_anns << ann
        else
          new_anns << a
        end
      end

      old_anns = @anns
      @anns = new_anns
      error, _, _ = make_books

      if error != ""
        @anns = old_anns
      end

      error
    end

    # Process all the annotations and generates accounting books.
    #
    # It returns:
    # * error: String. If there is no error returns an empty string. Otherwise
    #   returns the error.
    # * ledger: Ledger. If there is error returns blank ledger (all values 0.0)
    # * inventory: Inventory. If there is error returns an empty inventory.
    #
    # Error codes are:
    # * 0date (e.g. "020181009") if nick is not in inventory when selling.
    # * 1date (e.g. "120181009") if trying to sell more stocks than there
    #   are in inventory.
    def make_books : NamedTuple(
      error: String,
      ledger: Ledger,
      inventory: Inventory
    )
      err = ""
      ledger = Ledger.new
      inventory = Inventory.new

      anns.each do |ann|
        case ann.type
        when Ann::Type::Sell
          ie = inventory[ann.nick]?
          if !ie
            err = "0" + ann.date
            break
          end
          if ie.stocks < ann.stocks
            err = "1" + ann.date
            break
          end
          iout = ann.stocks * ie.price
          inventory.remove(ann.nick, ann.stocks)

          income0 = ann.stocks * ann.money
          fees = Fees.app(income0)
          income = income0 - fees

          dif = income0 - iout

          ledger.cash += income
          ledger.stocks -= iout
          ledger.sells += dif
          ledger.fees -= fees
        when Ann::Type::Buy
          pay0 = ann.stocks * ann.money
          fees = Fees.app(pay0)
          pay = pay0 + fees

          inventory.add(ann.nick, ann.stocks, ann.money)

          ledger.cash -= pay
          ledger.stocks += pay0
          ledger.fees -= fees
        when Ann::Type::Withdrawal
          ledger.cash -= ann.money
          ledger.capital -= ann.money
        when Ann::Type::Income
          ledger.cash += ann.money
          ledger.capital += ann.money
        when Ann::Type::Profits
          ledger.cash += ann.money
          ledger.incomes += ann.money
        when Ann::Type::Fees
          ledger.cash -= ann.money
          ledger.fees -= ann.money
        when Ann::Type::Pdif
          ledger.cash += ann.money
          ledger.differences += ann.money
        when Ann::Type::Ndif
          ledger.cash -= ann.money
          ledger.differences -= ann.money
        when Ann::Type::InitialCapital
          ledger.capital += ann.money
        when Ann::Type::InitialCash
          ledger.cash += ann.money
        when Ann::Type::InitialStock
          ledger.stocks += ann.stocks * ann.money
          inventory.add(ann.nick, ann.stocks, ann.money)
        else
          raise ArgumentError.new("Unkown type annotation")
        end
      end

      return mk_error(err) if err != ""
      {error: err, ledger: ledger, inventory: inventory}
    end

    def to_json(jsb : JSON::Builder)
      jsb.array do
        @anns.each do |a|
          a.to_json jsb
        end
      end
    end

    def to_s : String
      JSON.build { |jsb| to_json jsb }
    end

    def self.from_json(js : JSON::Any) : Journal
      r = Journal.new
      js.as_a.each do |j|
        err = r.add(Ann.from_json j)
        if err != ""
          raise IllegalArgument.new("Bad annotation")
        end
      end
      r
    end

    def self.from_s(s : String) : Journal
      self.from_json(JSON.parse s)
    end

    def self.close(
      ledger : Ledger,
      inventory : Inventory,
      after : Journal,
      after_year : String
    ) : NamedTuple(
      error: String, journal: Journal
    )
      r = Journal.new
      date = after_year + "0101"
      capital = ledger.capital + ledger.sells + ledger.fees +
        ledger.incomes + ledger.differences
      r.add(Ann.new(date, Ann::InitialCapital, "", 0, capital, ""))
      r.add(Ann.new(date, Ann::InitialCash, "", 0, ledger.cash, ""))
      inventory.entries.each do |nick, entry|
        r.add(Ann.new(
          date, Ann::IntialStock, nick, entry.stocks, entry.price, ""
        ))
      end

      (after.anns.skip { |a| a.date[4..-1] == "0101" }).each do |a|
        error = r.add a
        if error != ""
          break
        end
      end

      if error == ""
        {error: "", journal: r}
      else
        {error: error, journal: Journal.new}
      end

      error
    end

  end
end
