# Copyright 11-Oct-2017 ºDeme
# GNU General Public License - V3 <http://www.gnu.org/licenses/>

module MyMarket
  # Users management
  class Years
    getter user

    def initialize(@user : String)
    end

    private def read_journal(year : String) : Journal
      Journal.from_s(Io.read("#{Cts.annotations_dir}/#{@user}/#{year}"))
    end

    private def write_journal(year : String, journal : Journal)
      Io.write(
        "#{Cts.annotations_dir}/#{@user}/#{year}",
        journal.to_s
      )
    end

    # Returns a sorted list from before to after
    def list : Array(String)
      (Io.children "#{Cts.annotations_dir}/#{@user}").sort
    end

    def addFirst
      first_year = list[0]
      new_year = (first_year.to_i - 1).to_s

      j = read_journal(first_year)
      anns = j.anns.take_while { |a| a.date[4..-1] == "0101" }
      new_j = journal_new
      anns.each { |a| new_j.add a }
      write_jounal(new_year, new_j)
    end

    # 'addLast' closes last year and create the next one.
    #
    # If closing fails return an error with codes:
    # * 0date (e.g. "020181009") if nick is not in inventory when selling.
    # * 1date (e.g. "120181009") if trying to sell more stocks than there
    #   are in inventory.
    def addLast : String
      last_year = list[-1]
      new_year = (first_year.to_i + 1).to_s

      j = read_journal(last_yar)
      err, ledger, inventory = j.make_books
      if err != ""
        return err
      end

      new_j = j.close(ledger, inventory, Journal.new,  new_year)
      write_jounal(new_year, new_j)
    end

    def removeLast
      if list.size > 1
        year = list[-1]
        Io.del "#{Cts.annotations_dir}/#{@user}/#{year}"
      end
    end

    def removeFirst
      if list.size > 1
        year = list[0]
        Io.del "#{Cts.annotations_dir}/#{@user}/#{year}"
      end
    end

  end
end
