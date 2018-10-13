# Copyright 11-Oct-2017 ºDeme
# GNU General Public License - V3 <http://www.gnu.org/licenses/>

module MyMarket
  # Users management
  module Users
    extend self

    # Returns an unsorted list
    def list : Array(String)
      Io.children Cts.annotations_dir
    end

    def add(user : String)
      if !list.any? { |u| u == user }
        Io.mkdir "#{Cts.annotations_dir}/#{user}"
        year = Time.new.year.to_s
        Io.write("#{Cts.annotations_dir}/#{user}/#{year}", "[]")
      end
    end

    def remove(user : String)
      ls = list
      if ls.any? { |u| u == user }
        if ls.size == 1
          raise ArgumentError.new("It is not posible to remove an only user")
        else
          Io.del "#{Cts.annotations_dir}/#{user}"
        end
      end
    end

    def modify(old_user : String, new_user : String)
      ls = list
      if ls.any? { |u| u == old_user }
        Io.rename(
          "#{Cts.annotations_dir}/#{old_user}",
          "#{Cts.annotations_dir}/#{new_user}"
        )
      end
    end

  end
end
