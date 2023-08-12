! bof
! **********************************************************************
! Fortran module char_utilities

! $Id: coco.f90,v 2.11 2013/04/21 16:23:20 dan Exp $
! ----------------------------------------------------------------------
!  Copyright 2013 Dan Nagle

!   This module is free software; you can redistribute it and/or
!   modify it under the terms of the GNU General Public
!   License as published by the Free Software Foundation; either
!   version 3 of the License, or (at your option) any later version.

!   This module is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!   General Public License for more details.

!   You should have received a copy of the GNU General Public
!   License along with this library; if not, write to the Free
!   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

! To report bugs, suggest enhancements, etc. to the Authors,
! Contact:
!    Dan Nagle
!                               send email to dannagle@verizon.net
!                                  or mail to 731 Spruce St
!                                             Boulder CO 80302 USA

! ----------------------------------------------------------------------

!  char_utilities

! **********************************************************************

module char_utilities

! ----------------------------------------------------------------------

!  char_utilities uses modules

use :: constants, only: open_paren, close_paren, open_braket, close_braket, single_quote, double_quote, blank

! ----------------------------------------------------------------------

!  no implicit typing: require explicit declarations

implicit none

! ----------------------------------------------------------------------

!  all names are private: require explicit exports

private

! ----------------------------------------------------------------------

!  char_utilities RCS strings

! ----------------------------------------------------------------------

!  source file identifier supplied by RCS

character( len= *), public, parameter :: char_utilities_rcs_id = &
   '$Id: coco.f90,v 2.11 2013/04/21 16:23:20 dan Exp $'

! ----------------------------------------------------------------------

!  char_utilities library

! ----------------------------------------------------------------------

public :: replace_substring

public :: seek_close_paren
public :: seek_close_braket
public :: unquote_string

public :: to_lower

public :: format_date
public :: format_time

! **********************************************************************

!  module procedures

! **********************************************************************

contains

! **********************************************************************

!  %%% string utilities- editing, parenthesis and quotes

! **********************************************************************
! **********************************************************************

!  replace_substring() edit source lines

subroutine replace_substring( mixed_case_str, lower_case_str, search_str, replace_str, first_idx)

! **********************************************************************

!  replace_substring() interface

! ----------------------------------------------------------------------

!  mixed case string to be printed

character( len= *), intent( in out), optional :: mixed_case_str

!  lower case string to be searched

character( len= *), intent( in out) :: lower_case_str

!  substring to be replaced

character( len= *), intent( in) :: search_str

!  string to replace target

character( len= *), intent( in) :: replace_str

!  location of first occurance

integer, intent( in) :: first_idx

! **********************************************************************

!  entry: line is a line of Fortran source with (possibly) ?target?

!  exit: line has any ?target? strings replaced

! **********************************************************************

!  replace_substring() local

! ----------------------------------------------------------------------

!  beginning and end of target within lines

   integer :: end_idx

   integer :: search_idx

   integer :: search_len

! **********************************************************************

!  replace_substring() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  initialize

   search_idx = first_idx

   search_len = len( search_str)

! ----------------------------------------------------------------------

!  if mixed case is present

   mixed_present: if( present( mixed_case_str) )then

!  replace in both strings

      edit_mixed: do

         if( search_idx == 0 ) exit edit_mixed

         end_idx = search_idx + search_len

         end_mixed: if( search_idx == 1 )then

            mixed_case_str = replace_str // mixed_case_str( end_idx: )

            lower_case_str = replace_str // lower_case_str( end_idx: )

         else if( end_idx > len( lower_case_str) )then end_mixed

            mixed_case_str = mixed_case_str( 1: search_idx - 1) &
                           // replace_str

            lower_case_str = lower_case_str( 1: search_idx - 1) &
                           // replace_str

         else end_mixed

            mixed_case_str = mixed_case_str( 1: search_idx - 1) &
                           // replace_str &
                           // mixed_case_str( end_idx: )

            lower_case_str = lower_case_str( 1: search_idx - 1) &
                           // replace_str &
                           // lower_case_str( end_idx: )

         end if end_mixed

         search_idx = index( lower_case_str, search_str)

      end do edit_mixed

! ----------------------------------------------------------------------

!  mixed case is not present

   else mixed_present

!  replace in lower case only

      edit_string: do

         if( search_idx == 0 ) exit edit_string

         end_idx = search_idx + search_len

         end_lower: if( search_idx == 1 )then

            lower_case_str = replace_str // lower_case_str( end_idx: )

         else if( end_idx > len( lower_case_str) )then end_lower

            lower_case_str = lower_case_str( 1: search_idx - 1) // replace_str

         else end_lower

            lower_case_str = lower_case_str( 1: search_idx - 1) &
                           // replace_str &
                           // lower_case_str( end_idx: )

         end if end_lower

         search_idx = index( lower_case_str, search_str)

      end do edit_string

   end if mixed_present

! ----------------------------------------------------------------------

!  replace_substring() exit

return

! **********************************************************************

!  replace_substring()

end subroutine replace_substring

! **********************************************************************
! **********************************************************************

!  seek_close_paren() true if successfully found matching ()

subroutine seek_close_paren( string, start, match)

! **********************************************************************

!  seek_close_paren() interface

! ----------------------------------------------------------------------

!  the string starting with open parenthesis

character( len= *), intent( in) :: string

!  the index of the open parenthesis

integer, intent( in) :: start

!  the index of the matching close parenthesis

integer, intent( out) :: match

! **********************************************************************

!  seek_close_paren() local

! ----------------------------------------------------------------------

!  counters and pointers

   integer :: level

   integer :: string_len

! **********************************************************************

!  seek_close_paren() text

continue

! ----------------------------------------------------------------------

!  initialize

   string_len = len_trim( string)

   level = 0

! ----------------------------------------------------------------------

   search: do match = start + 1, string_len

      levels: select case( string( match: match) )

      case( open_paren) levels

         level = level + 1

      case( close_paren) levels

         eureka: if( level == 0 )then

            exit search

         end if eureka

         level = level - 1

      end select levels

   end do search

! ----------------------------------------------------------------------

!  seek_close_paren() exit

return

! **********************************************************************

!  seek_close_paren()

end subroutine seek_close_paren

! **********************************************************************
! **********************************************************************

!  seek_close_braket() true if successfully found matching ()

subroutine seek_close_braket( string, start, match)

! **********************************************************************

!  seek_close_braket() interface

! ----------------------------------------------------------------------

!  the string starting with open braket

character( len= *), intent( in) :: string

!  the index of the open braket

integer, intent( in) :: start

!  the index of the matching close braket

integer, intent( out) :: match

! **********************************************************************

!  seek_close_braket() local

! ----------------------------------------------------------------------

!  counters and pointers

   integer :: level

   integer :: string_len

! **********************************************************************

!  seek_close_braket() text

continue

! ----------------------------------------------------------------------

!  initialize

   string_len = len_trim( string)

   level = 0

! ----------------------------------------------------------------------

   search: do match = start + 1, string_len

      levels: select case( string( match: match) )

      case( open_braket) levels

         level = level + 1

      case( close_braket) levels

         eureka: if( level == 0 )then

            exit search

         end if eureka

         level = level - 1

      end select levels

   end do search

! ----------------------------------------------------------------------

!  seek_close_braket() exit

return

! **********************************************************************

!  seek_close_braket()

end subroutine seek_close_braket

! **********************************************************************
! **********************************************************************

!  unquote_string() true if extracts string from between quotes

subroutine unquote_string( quoted_str, unquoted_str, in_len, out_len)

! **********************************************************************

!  unquote_string() interface

! ----------------------------------------------------------------------

!  the quoted string to be unquoted

character( len= *), intent( in) :: quoted_str

!  the unquoted string

character( len= *), intent( out) :: unquoted_str

!  the length of the quoted string

integer, intent( out) :: in_len

!  the length of the unquoted string

integer, intent( out) :: out_len

! **********************************************************************

!  unquote_string() local

! ----------------------------------------------------------------------

!  which quote is to be used

   character( len= 1) :: quote

! **********************************************************************

!  unquote_string() text

continue

! ----------------------------------------------------------------------

!  which quote is the first quote (if either)

   which_quote: select case( quoted_str( 1: 1) )

! ----------------------------------------------------------------------

!  string delimited by single quote

   case( single_quote) which_quote

      quote = single_quote

! ----------------------------------------------------------------------

!  string delimited by double quote

   case( double_quote) which_quote

      quote = double_quote

! ----------------------------------------------------------------------

!  string delimited by neither quote- nothing to do

   case default which_quote

      in_len = 0

      out_len = len_trim( quoted_str)

      unquoted_str = quoted_str

      return

   end select which_quote

! ----------------------------------------------------------------------

!  initialize scan loop

   in_len = 2
   out_len = 1

   unquoted_str = blank

!  scan thru the quoted string

   scan_string: do

      if( in_len > len_trim( quoted_str) ) exit scan_string

! ----------------------------------------------------------------------

!  if find one matching quote

      next_char: if( quoted_str( in_len: in_len) == quote )then

!  check for a pair of quotes

         next_quote: if( quoted_str( in_len + 1: in_len + 1) == quote )then

            unquoted_str( out_len: out_len) = quoted_str( in_len: in_len)

            in_len = in_len + 1

            out_len = out_len + 1

         else next_quote

            exit scan_string

         end if next_quote

!  check next character

         in_len = in_len + 1

! ----------------------------------------------------------------------

!  character is not a matching quote

      else next_char

         unquoted_str( out_len: out_len) = quoted_str( in_len: in_len)

         in_len = in_len + 1

         out_len = out_len + 1

      end if next_char

   end do scan_string

! ----------------------------------------------------------------------

!  unquote_string() exit

return

! **********************************************************************

!  unquote_string()

end subroutine unquote_string

! **********************************************************************
! **********************************************************************

!  to_lower() string is returned all lower case

pure function to_lower( string) result( lc_str)

! **********************************************************************

!  to_lower() interface

! ----------------------------------------------------------------------

!  the string to be lowercased

character( len= *), intent( in) :: string

!  the lower case string

character( len= len( string)) :: lc_str

! **********************************************************************

!  to_lower() local

! ----------------------------------------------------------------------

!  ascii characters change case

integer, parameter :: change_case = 32

! ----------------------------------------------------------------------

!  index characters in string

   integer :: i

! **********************************************************************

!  to_lower() text

continue

! ----------------------------------------------------------------------

!  check every character in string

   scan_string: do i = 1, len( string)

      check_char: select case( string( i: i))

      case( 'A': 'Z') check_char

         lc_str( i: i) = char( ichar( string( i: i)) + change_case)

      case default check_char

         lc_str( i: i) = string( i: i)

      end select check_char

   end do scan_string

! ----------------------------------------------------------------------

!  to_lower() exit

return

! **********************************************************************

!  to_lower()

end function to_lower

! **********************************************************************
! **********************************************************************

!  format_date() date string is with slashes

pure function format_date( string) result( fmt_str)

! **********************************************************************

!  format_date() interface

! ----------------------------------------------------------------------

!  the date string to be formatted

character( len= 8), intent( in) :: string

!  the lower case string

character( len= 10) :: fmt_str

! **********************************************************************

!  format_date() constants

character( len= *), parameter :: slash = '/'

! **********************************************************************

!  format_date() text

continue

! ----------------------------------------------------------------------

!  build the new string

   fmt_str = string( 1: 4) // slash // string( 5: 6) // slash // string( 7: 8)

! ----------------------------------------------------------------------

!  format_date() exit

return

! **********************************************************************

!  format_date()

end function format_date

! **********************************************************************
! **********************************************************************

!  format_time() time string is with colons

pure function format_time( string) result( fmt_str)

! **********************************************************************

!  format_time() interface

! ----------------------------------------------------------------------

!  the time string to be formatted

character( len= 10), intent( in) :: string

!  the lower case string

character( len= 12) :: fmt_str

! **********************************************************************

!  format_time() constants

! ----------------------------------------------------------------------

!  character used to separate hh and mm, and mm and ss

character( len= *), parameter :: colon = ':'

! **********************************************************************

!  format_time() text

continue

! ----------------------------------------------------------------------

!  build the new string

   fmt_str = string( 1: 2) // colon // string( 3: 4) // colon // string( 5: 10)

! ----------------------------------------------------------------------

!  format_time() exit

return

! **********************************************************************

!  format_time()

end function format_time

! **********************************************************************
! ----------------------------------------------------------------------

!  char_utilities

! $Id: coco.f90,v 2.11 2013/04/21 16:23:20 dan Exp $
! **********************************************************************
! eof
end module char_utilities
