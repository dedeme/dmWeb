// Copyright 18-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Functions using external programs

#ifndef DMC_EXT_H
  #define DMC_EXT_H

/// ext_wget_new calls "wget -q -O - <i>url</i>" and returns the text read.<br>
/// If the reading fails, it returns an empty string.
char *ext_wget_new(const char *url);

/// ext_zenity_entry_new reads a text using GUI. It calls:
///   zenity --entry --title=<i>title</i> --text=<i>prompt</i>
/// The return removes starting and trailing spaces.<br>
/// If user clicks on cancel, it returns an empty string.<br>
/// It is posible set a default text adding in promp:
///   \" --entry-text \"<i>text_to_add</i>
char *ext_zenity_entry_new(const char *title, const char *prompt);

/// ext_zenity_msg shows a message box. It calls:
///   zenity --notification --window-icon=<i>type</i> --text=<i>text</i>
/// 'icon' is one of gnome icon stock. For example: info, dialog-warning,
/// dialog-error, dialog-information, face-wink, etc
void ext_zenity_msg(const char *icon, const char *text);

/// ext_pdf generates a pdf file from a html text. It calls:
///   pdfPrinter -s %s -t %s options 2>&1
/// Parameters:
///   tx_source  : Text html
///   file_target: Path of the new pdf file
///   options    : Options for pdfPrinter
void ext_pdf(
  const char *tx_source,
  const char *file_target,
  const char *options
);

/// ext_zip compress source in target. It calls:
///   zip -q <i>target</i> <i>source</i> 2>&1
/// if 'target' already exists, source will be added to it. If you require a
/// fresh target file, you have to delete it previously.<br>
/// Parameters:
///   source: can be a file or directory,
///   target: Zip file. If it is a relative path, it hangs on source parent.
void ext_zip(const char *source, const char *target);

/// ext_unzip uncompress source in target, It calls:
///   unzip -q <i>source</i> -d <i>target</i> 2>&1
/// Parameters:
///   source: Zip file.
///   target: A directory. It it does not exist, it is created.
void ext_unzip(const char *source, const char *target);

#endif
