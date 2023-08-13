! bof
! **********************************************************************
! Fortran 2008 program env2inc

! $Id$
! ----------------------------------------------------------------------
!  Copyright 2013 Dan Nagle

!   This program is free software; you can redistribute it and/or
!   modify it under the terms of the GNU General Public
!   License as published by the Free Software Foundation; either
!   version 3 of the License, or (at your option) any later version.

!   This program is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!   General Public License for more details.

!   You should have received a copy of the GNU General Public
!   License along with this program; if not, write to the Free
!   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

! To report bugs, suggest enhancements, or contact the Authors,
! Contact:
!    Dan Nagle
!                               send email to dannagle@verizon.net
!                                  or mail to 731 Spruce St.
!                                             Boulder, CO 80302 USA

! ----------------------------------------------------------------------

!  env2inc

! **********************************************************************

program env2inc

! ----------------------------------------------------------------------

!  env2inc imports modules

! ----------------------------------------------------------------------

!  processor description

use, intrinsic :: iso_fortran_env, only: compiler_version, compiler_options, &
                                         character_storage_size, file_storage_size, numeric_storage_size, &
                                         character_kinds, integer_kinds, logical_kinds, real_kinds, &
                                         int8, int16, int32, int64, &
                                         real32, real64, real128, &
                                         error_unit

! ----------------------------------------------------------------------

!  all names are declared: require explicit declarations

implicit none

! ----------------------------------------------------------------------

!  source file identifier supplied by RCS

character( len= *), parameter :: env2inc_rcs_id = &
   '$Id$'

! ----------------------------------------------------------------------

!  env2inc constants

! ----------------------------------------------------------------------

!  include file unit

integer, parameter :: inc_unit = 20

!  include file name

character( len= *), parameter :: inc_name = 'fortran_env.inc'

!  formats

character( len= *), parameter :: string_fmt = '( a)'

character( len= *), parameter :: string_int_fmt = '( a, i0)'

! ----------------------------------------------------------------------

!  env2inc data

! ----------------------------------------------------------------------

   integer :: io_status

   character( len= 100) :: io_msg

   integer :: major, minor, patch

! **********************************************************************

!  env2inc executable

! **********************************************************************

continue

! ----------------------------------------------------------------------

!  open the include file to receive the definitions

   open( unit= inc_unit, file= inc_name, status= 'replace', iostat= io_status, iomsg= io_msg)

   open_error: if( io_status > 0 )then

      write( unit= error_unit, fmt= string_fmt) trim( io_msg)

      stop 'open error in env2inc'

   end if open_error

! ----------------------------------------------------------------------

!  identify the compiler version and options and env2inc verison

   write( unit= inc_unit, fmt= string_fmt) '?? macro :: compiler_version = ' // compiler_version()

   write( unit= inc_unit, fmt= string_fmt) '?? macro :: compiler_options = ' // compiler_options()

   write( unit= inc_unit, fmt= string_fmt) '?? macro :: env2inc_version = ' // env2inc_rcs_id

! ----------------------------------------------------------------------

!  try to get major release, minor release, and patch level from the version string

   call get_revision_as_integers( major, minor, patch)

   write( unit= inc_unit, fmt= string_int_fmt) '?? integer, parameter :: compiler_major_revision = ', major

   write( unit= inc_unit, fmt= string_int_fmt) '?? integer, parameter :: compiler_minor_revision = ', minor

   write( unit= inc_unit, fmt= string_int_fmt) '?? integer, parameter :: compiler_patch_level = ', patch

! ----------------------------------------------------------------------

!  identify the storage unit sizes

   write( unit= inc_unit, fmt= string_int_fmt) '?? integer, parameter :: character_storage_size = ', character_storage_size

   write( unit= inc_unit, fmt= string_int_fmt) '?? integer, parameter :: file_storage_size = ', file_storage_size

   write( unit= inc_unit, fmt= string_int_fmt) '?? integer, parameter :: numeric_storage_size = ', numeric_storage_size

! ----------------------------------------------------------------------

!  identify the number of kinds for each intrinsic type

   write( unit= inc_unit, fmt= string_int_fmt) '?? integer, parameter :: count_character_kinds = ', size( character_kinds)

   write( unit= inc_unit, fmt= string_int_fmt) '?? integer, parameter :: count_integer_kinds = ', size( integer_kinds)

   write( unit= inc_unit, fmt= string_int_fmt) '?? integer, parameter :: count_logical_kinds = ', size( logical_kinds)

   write( unit= inc_unit, fmt= string_int_fmt) '?? integer, parameter :: count_real_kinds = ', size( real_kinds)

! ----------------------------------------------------------------------

!  identify the value of the processor's integer kinds

   write( unit= inc_unit, fmt= string_int_fmt) '?? integer, parameter :: int8 = ', int8

   write( unit= inc_unit, fmt= string_int_fmt) '?? integer, parameter :: int16 = ', int16

   write( unit= inc_unit, fmt= string_int_fmt) '?? integer, parameter :: int32 = ', int32

   write( unit= inc_unit, fmt= string_int_fmt) '?? integer, parameter :: int64 = ', int64

! ----------------------------------------------------------------------

!  identify the value of the processor's real kinds

   write( unit= inc_unit, fmt= string_int_fmt) '?? integer, parameter :: real32 = ', real32

   write( unit= inc_unit, fmt= string_int_fmt) '?? integer, parameter :: real64 = ', real64

   write( unit= inc_unit, fmt= string_int_fmt) '?? integer, parameter :: real128 = ', real128

! ----------------------------------------------------------------------

!  identify the value of the processor's character kinds

   write( unit= inc_unit, fmt= string_int_fmt) '?? integer, parameter :: ascii = ', selected_char_kind( 'ascii')

   write( unit= inc_unit, fmt= string_int_fmt) '?? integer, parameter :: iso_10646 = ', selected_char_kind( 'iso_10646')

   write( unit= inc_unit, fmt= string_int_fmt) '?? integer, parameter :: default = ', selected_char_kind( 'default')

! ----------------------------------------------------------------------

!  close and keep the include file

   close( unit= inc_unit, status= 'keep', iostat= io_status, iomsg= io_msg)

   close_error: if( io_status > 0 )then

      write( unit= error_unit, fmt= string_fmt) trim( io_msg)

      stop 'close error in env2inc'

   end if close_error

! ----------------------------------------------------------------------

!  env2inc

stop 'normal exit in env2inc'

! **********************************************************************

!  env2inc library

contains

! ----------------------------------------------------------------------

!  attempt to extract major revision, minor revision, and patch level

subroutine get_revision_as_integers( major, minor, patch)

integer, intent( out) :: major, minor, patch

character( len= *), parameter :: blank = ' '

character( len= *), parameter :: dot = '.'
character( len= *), parameter :: digits = '0123456789'
character( len= *), parameter :: digits_dot = digits // dot

character( len= *), parameter :: f90_str = '90'
character( len= *), parameter :: f95_str = '95'
character( len= *), parameter :: f2003_str = '2003'
character( len= *), parameter :: f03_str = '03'
character( len= *), parameter :: f2008_str = '2008'
character( len= *), parameter :: f08_str = '08'

character( len= *), parameter :: conversion_fmt = '( bz, i10)'

   character( len= 10) :: conversion_str

   character( len= 80) :: version

   integer :: digit_idx, dot_idx, dot2_idx, end_idx

! ----------------------------------------------------------------------

!  text

continue

!  get the processor-reported verison string

   version = compiler_version()

!  remove possible strings that would confound the search

   digit_idx = index( string= version, substring= f90_str)

   rm_f90: if( digit_idx > 0 )then

      version( digit_idx: digit_idx + len( f90_str) - 1) = blank

   end if rm_f90

   digit_idx = index( string= version, substring= f95_str)

   rm_f95: if( digit_idx > 0 )then

      version( digit_idx: digit_idx + len( f95_str) - 1) = blank

   end if rm_f95

   digit_idx = index( string= version, substring= f2003_str)

   rm_f2003: if( digit_idx > 0 )then

      version( digit_idx: digit_idx + len( f2003_str) - 1) = blank

   end if rm_f2003

   digit_idx = index( string= version, substring= f03_str)

   rm_f03: if( digit_idx > 0 )then

      version( digit_idx: digit_idx + len( f03_str) - 1) = blank

   end if rm_f03

   digit_idx = index( string= version, substring= f2008_str)

   rm_f2008: if( digit_idx > 0 )then

      version( digit_idx: digit_idx + len( f2008_str) - 1) = blank

   end if rm_f2008

   digit_idx = index( string= version, substring= f08_str)

   rm_f08: if( digit_idx > 0 )then

      version( digit_idx: digit_idx + len( f08_str) - 1) = blank

   end if rm_f08

!  find the first digit in the string

   digit_idx = scan( string= version, set= digits)

!  when no digit is present set all to zero and quit

   no_digits_major: if( digit_idx == 0 )then

      major = 0
      minor = 0
      patch = 0
      return

   end if no_digits_major

!  find a dot following the digit

   dot_idx = scan( string= version, set= dot)

!  check whether dot is found past the digit

   no_dot_major: if( dot_idx < digit_idx )then

!  when no dot is present find the end of the digits

      end_idx = verify( string= version( digit_idx: ), set= digits) + digit_idx

      conversion_str = version( digit_idx: end_idx - 1)

   else no_dot_major

!  found a dot

      conversion_str = version( digit_idx: dot_idx - 1)

   end if no_dot_major

   conversion_str = adjustr( conversion_str)

   read( unit= conversion_str, fmt= conversion_fmt) major

   dot2_idx = scan( string= version( dot_idx + 1: ), set= dot)

   count_dots: if( dot2_idx > 0 )then

      dot2_idx = dot2_idx + dot_idx

      conversion_str = version( dot_idx + 1: dot2_idx - 1)
      conversion_str = adjustr( conversion_str)

      read( unit= conversion_str, fmt= conversion_fmt) minor

      end_idx = verify( version( dot2_idx + 1: ), digits_dot) + dot2_idx

      conversion_str = version( dot2_idx + 1: end_idx - 1)
      conversion_str = adjustr( conversion_str)

      read( unit= conversion_str, fmt= conversion_fmt) patch

   else count_dots

      end_idx = verify( version( dot_idx + 1: ), digits_dot) + dot_idx

      conversion_str = version( dot_idx + 1: end_idx - 1)
      conversion_str = adjustr( conversion_str)

      read( unit= conversion_str, fmt= conversion_fmt) minor

      patch = 0

   end if count_dots

return

end subroutine get_revision_as_integers

! **********************************************************************

!  env2inc

! $Id$
! **********************************************************************
! eof
end program env2inc

