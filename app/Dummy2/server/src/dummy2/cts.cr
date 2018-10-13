# Copyright 06-Oct-2017 ºDeme
# GNU General Public License - V3 <http://www.gnu.org/licenses/>

# Constants
module Dummy2
  module Cts
    extend self

    # Its value must be the same in the client side
    APP_NAME = "Dummy2"

    # Data version
    DATA_VERSION = "Dummy2: Data version 201810"

    # PORT
    PORT = 18106

    # Absolute path of web root directory
    WEB_DIRECTORY ="/dm/www2/#{APP_NAME}"

    # Web temporary directory
    WEB_TMP = "#{WEB_DIRECTORY}/tmp"

    # Browser point of entry
    BROWSER_PATH = "http://localhost:#{PORT}/index.html"

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

  end
end
