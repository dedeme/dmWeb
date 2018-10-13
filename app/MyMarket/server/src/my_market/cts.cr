# Copyright 09-Oct-2017 ºDeme
# GNU General Public License - V3 <http://www.gnu.org/licenses/>

# Constants
module MyMarket
  module Cts
    extend self

    # Its value must be the same in the client side
    APP_NAME = "MyMarket"

    # Data version
    DATA_VERSION = "MyMarket: Data version 201810"

    # PORT
    PORT = 18100

    # Absolute path of web root directory
    WEB_DIRECTORY ="/dm/www2/#{APP_NAME}"

    # Web temporary directory
    WEB_TMP = "#{WEB_DIRECTORY}/tmp"

    # Browser point of entry
    BROWSER_PATH = "http://localhost:#{PORT}/index.html"

    # Quotes directory
    QUOTES_DIR = "/dm/wwwcgi/dmcgi/Quotes/data"

    # Data which will be saved in backups
    def data_dir
      "#{Sys.home}/data"
    end

    # Automatica backups
    def backups_dir
      "#{Sys.home}/backups"
    end

    # Decarded backups
    def trash_dir
      "#{Sys.home}/trash"
    end

    # Temporary directory connected with www
    def tmp_dir
      "#{Sys.home}/tmp"
    end

    # Temporary directory connected with www
    def quotes_dir
      "#{Sys.home}/Quotes"
    end

    # Temporary directory connected with www
    def annotations_dir
      "#{data_dir}/annotations"
    end

  end
end
