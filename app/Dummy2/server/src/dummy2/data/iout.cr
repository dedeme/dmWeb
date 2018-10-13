# Copyright 06-Oct-2017 ºDeme
# GNU General Public License - V3 <http://www.gnu.org/licenses/>

module Dummy2
  module Iout
    extend self

    # Initialize data base in 'home' and in 'web'
    def init
      if !Io.exists? Cts::WEB_DIRECTORY
        raise IO::Error.new("'#{Cts::WEB_DIRECTORY}' not found")
      end
      Io.mkdir Cts::WEB_TMP

      if !Io.exists? Cts.data_dir
        Io.mkdir Cts.data_dir
        Io.mkdir Cts.backups_dir
        Io.mkdir Cts.trash_dir
        Io.symlink(Cts::WEB_TMP, Cts.tmp_dir)

        Io.write("#{Cts.data_dir}/version.txt", Cts::DATA_VERSION)
        write_conf Conf.new("es", "settings")
      end
    end

    private def write_conf(conf : Conf)
      Io.write("#{Cts.data_dir}/conf.db", conf.to_s)
    end

    def read_conf : String
      Io.read "#{Cts.data_dir}/conf.db"
    end

    private def read_conf2 : Conf
      Conf.from_s read_conf
    end

    def set_lang(lang : String)
      cf = read_conf2
      cf.lang = lang
      write_conf cf
    end

    def set_menu(option : String)
      cf = read_conf2
      cf.menu = option
      write_conf cf
    end

    def backup : String
      name = "#{Cts::APP_NAME}Backup#{Time.now.to_s("%Y%m%d")}.zip"
      Io.del Cts::WEB_TMP
      Io.mkdir Cts::WEB_TMP
      Ext.zip(Cts.data_dir, "#{Cts.tmp_dir}/#{name}")
      name
    end

    def restore_start
      Io.del Cts::WEB_TMP
      Io.mkdir Cts::WEB_TMP
    end

    def restore_append(bs : Bytes)
      f = Io.aopen "#{Cts.tmp_dir}/backup.zip"
      Io.write(f, bs)
      f.close
    end

    def restore_end : String
      fpath = "#{Cts.tmp_dir}/backup.zip"
      if !Io.exists?(fpath)
        return "'#{fpath}' not found"
      end

      begin
        Ext.unzip(fpath, Cts.tmp_dir)
      rescue ex
        puts ex.message
        return "restore:unzip"
      end

      fversion = "#{Cts.tmp_dir}/data/version.txt"
      if !Io.exists?(fversion)
        return "restore:valid"
      end
      if Cts::DATA_VERSION != Io.read(fversion)
        return "restore:version"
      end

      t = Time.now.to_s("%Y%m%d-%H%M%S%L")
      Ext.zip(Cts.data_dir, "#{Cts.trash_dir}/#{t}.zip")

      Io.del Cts.data_dir
      Io.rename("#{Cts.tmp_dir}/data", Cts.data_dir)
      ""
    end

    def autorestore(file : String)
      Ext.unzip("#{Cts.backups_dir}/#{file}", Cts.tmp_dir)

      t = Time.now.to_s("%Y%m%d-%H%M%S%L")
      Ext.zip(Cts.data_dir, "#{Cts.trash_dir}/#{t}.zip")

      Io.del Cts.data_dir
      Io.rename("#{Cts.tmp_dir}/data", Cts.data_dir)
    end

    def automatic_backup
      t = Time.now.to_s("%Y%m%d")
      Ext.zip(Cts.data_dir, "#{Cts.backups_dir}/#{t}.zip")

      d1 = (Time.now - 7.days).to_s("%Y%m%d")
      d2 = (Time.now - 1.year).to_s("%Y%m%d")
      previous = "      "
      Io.children(Cts.backups_dir).sort.reverse.each do | fname |
        name = fname[0..-5]
        if name < d2
          if name[0..3] == previous[0..3]
            Io.del "#{Cts.backups_dir}/#{fname}"
          else
            previous = name
          end
        elsif name < d1
          if name[0..5] == previous[0..5]
            Io.del "#{Cts.backups_dir}/#{fname}"
          else
            previous = name
          end
        end
      end
    end

    def restoreTrash(file : String)
      Ext.unzip("#{Cts.trash_dir}/#{file}", Cts.tmp_dir)

      t = Time.now.to_s("%Y%m%d-%H%M%S%L")
      Ext.zip(Cts.data_dir, "#{Cts.trash_dir}/#{t}.zip")

      Io.del Cts.data_dir
      Io.rename("#{Cts.tmp_dir}/data", Cts.data_dir)
    end

    def clearTrash
      Io.del Cts.trash_dir
      Io.mkdir Cts.trash_dir
    end

    def backups_trash_lists : NamedTuple(
      backups: Array(String), trash: Array(String)
    )
      {
        backups: Io.children(Cts.backups_dir),
        trash: Io.children(Cts.trash_dir)
      }
    end

  end
end
