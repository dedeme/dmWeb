# Copyright 10-Oct-2017 ºDeme
# GNU General Public License - V3 <http://www.gnu.org/licenses/>

require "json"

module MyMarket
  class InventoryEntry
    getter stocks : Int32
    getter price : Float64
    getter close : Float64

    def initialize(@stocks, @price, @close)
    end
  end

  class Inventory
    getter entries = {} of String => InventoryEntry

    def initialize
    end

    def []?(nick : String) : InventoryEntry | Nil
      @entries[nick]?
    end

    def nicks() : Array(String)
      entries.keys
    end

    def add(nick : String, stocks : Int32, price : Float64)
      e = entries[nick]?
      if (e)
        nstocks = stocks + e.stocks
        entries[nick] = InventoryEntry.new(
          nstocks,
          (stocks * price + e.stocks * e.price) / nstocks,
          0.0
        )
      elsif
        entries[nick] = InventoryEntry.new(stocks, price, 0.0)
      end
    end

    def remove(nick : String, stocks : Int32)
      e = entries[nick]
      nstocks = e.stocks - stocks
      if nstocks == 0
        entries.delete nicks
      elsif
        entries[nick] = InventoryEntry.new(nstocks, e.price,  0.0)
      end
    end

    def setClose(nick : String, close : Float64)
      e = entries[nick]
      emtries[nick] = InventoryEntry.new(e.stocks, e.price, close)
    end

    def accValue : Float64
      entries.values.reduce(0) { |acc, e| acc + e.stocks * e.price }
    end

    def marketValue : Float64
      entries.values.reduce(0) { |acc, e| acc + e.stocks * e.close }
    end

  end
end
