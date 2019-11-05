// Copyright 18-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Utilities for managing dates

#ifndef DMC_DATE_H
  #define DMC_DATE_H

#include <time.h>
#include "dmc/std.h"

/// date_new makes a new time_t.
///   this  : New time_t
///   year  : Year with all digits
///   month : Month in base 1 (1 to 12)
///   day   : Day in base 1 (1 to 31)
time_t date_new (int day, int month, int year);

/// date_now returns the current date
time_t date_now (void);

/// date_from_str makes a date from a string is in format yyyymmdd
/// (month and day in base 1)
time_t date_from_str (const char *date);

/// date_from_iso makes a date from a string in format [x]x/[x]x/[xxx]x
/// (month and day in base 1)
time_t date_from_iso (const char *date);

/// date_from_us makes a date from a string in format  [x]x/[x]x/[xxx]x
/// (month and day in base 1)
time_t date_from_us (const char *date);

/// date_from_iso_sep makes a date from a string is in format
/// [x]xSP[x]xSP[xxx]x. If 'data' is not valid, returns '0'
time_t date_from_iso_sep (const char *date, char sep);

/// date_from_us_sep makes a date from a string in format
/// [x]xSP[x]xSP[xxx]x. If 'data' is not valid, returns '0'
time_t date_from_us_sep (const char *date, char sep);

///
int date_eq (time_t this, time_t another);

///
int date_cmp (time_t this, time_t another);

/// date_df returns the difference in days this - another.
int date_df (time_t this, time_t another);

///
time_t date_add (time_t this, int days);

///
int date_day (time_t this);

///
int date_month (time_t this);

///
int date_year (time_t this);

/// date_format formats a time_t. Format can be:
///   %a     The abbreviated name of the day of the week according to the
///          current locale.
///   %A     The full name of the day of the week according to the current
///          locale.
///   %b     The abbreviated month name according to the current locale.
///   %B     The full month name according to the current locale.
///   %c     The preferred date and time representation for the current
///          locale.
///   %C     The century number (year/100) as a 2-digit integer. (SU)
///   %d     The day of the month as a decimal number (range 01 to 31).
///   %D     Equivalent to %m/%d/%y.  (Yecch—for Americans only.  Americans
///          should note that in other countries %d/%m/%y is rather common.
///          This means that in international context this format is
///          ambiguous and should not be used.) (SU)
///   %e     Like %d, the day of the month as a decimal number, but a
///          leading zero is replaced by a space. (SU)
///   %F     Equivalent to %Y-%m-%d (the ISO 8601 date format). (C99)
///   %G     The ISO 8601 week-based year (see NOTES) with century as a
///          decimal number.  The 4-digit year corresponding to the ISO
///          week number (see %V).  This has the same format and value as
///          %Y, except that if the ISO week number belongs to the previous
///          or next year, that year is used instead. (TZ)
///   %g     Like %G, but without century, that is, with a 2-digit year
///          (00-99). (TZ)
///   %h     Equivalent to %b.  (SU)
///   %H     The hour as a decimal number using a 24-hour clock (range 00
///          to 23).
///   %I     The hour as a decimal number using a 12-hour clock (range 01
///          to 12).
///   %j     The day of the year as a decimal number (range 001 to 366).
///   %k     The hour (24-hour clock) as a decimal number (range 0 to 23);
///          single digits are preceded by a blank.  (See also %H.)  (TZ)
///   %l     The hour (12-hour clock) as a decimal number (range 1 to 12);
///          single digits are preceded by a blank.  (See also %I.)  (TZ)
///   %m     The month as a decimal number (range 01 to 12).
///   %M     The minute as a decimal number (range 00 to 59).
///   %n     A newline character. (SU)
///   %O     Modifier: use alternative format, see below. (SU)
///   %p     Either "AM" or "PM" according to the given time value, or the
///          corresponding strings for the current locale.  Noon is treated
///          as "PM" and midnight as "AM".
///   %P     Like %p but in lowercase: "am" or "pm" or a corresponding
///          string for the current locale. (GNU)
///   %r     The time in a.m. or p.m. notation.  In the POSIX locale this
///          is equivalent to %I:%M:%S %p.  (SU)
///   %R     The time in 24-hour notation (%H:%M).  (SU) For a version
///          including the seconds, see %T below.
///   %s     The number of seconds since the Epoch, 1970-01-01 00:00:00
///          +0000 (UTC). (TZ)
///   %S     The second as a decimal number (range 00 to 60).  (The range
///          is up to 60 to allow for occasional leap seconds.)
///   %t     A tab character. (SU)
///   %T     The time in 24-hour notation (%H:%M:%S).  (SU)
///   %u     The day of the week as a decimal, range 1 to 7, Monday being
///          1.  See also %w.  (SU)
///   %U     The week number of the current year as a decimal number, range
///          00 to 53, starting with the first Sunday as the first day of
///          week 01.  See also %V and %W.
///   %V     The ISO 8601 week number (see NOTES) of the current year as a
///          decimal number, range 01 to 53, where week 1 is the first week
///          that has at least 4 days in the new year.  See also %U and %W.
///          (SU)
///   %w     The day of the week as a decimal, range 0 to 6, Sunday being
///          0.  See also %u.
///   %W     The week number of the current year as a decimal number, range
///          00 to 53, starting with the first Monday as the first day of
///          week 01.
///   %x     The preferred date representation for the current locale
///          without the time.
///   %X     The preferred time representation for the current locale
///          without the date.
///   %y     The year as a decimal number without a century (range 00 to
///          99).
///   %Y     The year as a decimal number including the century.
///   %z     The +hhmm or -hhmm numeric timezone (that is, the hour and
///          minute offset from UTC). (SU)
///   %Z     The timezone name or abbreviation.
///   %%     A literal '%' character.
char *date_f_new(time_t this, const char *template);

/// date_to_str returns a string in format yyyymmdd
char *date_to_str_new(time_t this);

/// date_to_iso returns a string in format dd/mm/yyyy
char *date_to_iso_new(time_t this);

/// date_to_us returns a string in format mm/dd/yyyy
char *date_to_us_new(time_t this);

///
Js *date_to_js_new(time_t this);

///
time_t date_from_js(Js *js);


#endif
