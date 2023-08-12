! bof
! **********************************************************************
! Fortran module constants

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
!   License along with this module; if not, write to the Free
!   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

! To report bugs, suggest enhancements, etc. to the Authors,
! Contact:
!    Dan Nagle
!                               send email to dannagle@verizon.net
!                                  or mail to 731 Spruce St
!                                             Boulder CO 80302 USA

! ----------------------------------------------------------------------

!  constants

! **********************************************************************

module constants

! ----------------------------------------------------------------------

!  no implicit typing: require explicit declarations

implicit none

! ----------------------------------------------------------------------

!  all names are private: require explicit exports

private

! ----------------------------------------------------------------------

!  constants RCS strings

! ----------------------------------------------------------------------

!  source file identifier supplied by RCS

character( len= *), parameter, public :: constants_rcs_id = &
   '$Id: coco.f90,v 2.11 2013/04/21 16:23:20 dan Exp $'

! ----------------------------------------------------------------------

!  constants constants

! ----------------------------------------------------------------------

!  length to hold a processor error message

integer, parameter :: status_msg_len = 1024

! ----------------------------------------------------------------------

!  length of character storing a constant or variable name

integer, parameter, public :: symbol_name_len = 31

! ----------------------------------------------------------------------

!  length of character storing file names

integer, parameter, public :: file_name_len = 4096

! ----------------------------------------------------------------------

!  length of a Fortran source line

integer, parameter, public :: free_form_len = 132

integer, parameter, public :: card_image_len = 80

integer, parameter, public :: fixed_form_len = 72

!  length of character line buffers (allows for max_continuations number of continuations)

integer, parameter, public :: max_continuations = 39

!  buffer a whole coco statement and always have a blank at the end

integer, parameter, public :: buffer_len = ( max_continuations + 1) * free_form_len + 1

! ----------------------------------------------------------------------

!  null string

character( len= *), parameter, public :: null_string = ''

!  blank character

character( len= *), parameter, public :: blank = ' '

!  . separates file names from extensions, delimits logical operators & literals

character( len= *), parameter, public :: dot = '.'

! ----------------------------------------------------------------------

!  strings used to declare symbol names and values

! ----------------------------------------------------------------------

!  equal sign

character( len= *), parameter, public :: equals = '='

!  open parenthesis

character( len= *), parameter, public :: open_paren = '('

!  close parenthesis

character( len= *), parameter, public :: close_paren = ')'

!  open braket

character( len= *), parameter, public :: open_braket = '['

!  close braket

character( len= *), parameter, public :: close_braket = ']'

!  quotes

character( len= *), parameter, public :: single_quote = "'"

character( len= *), parameter, public :: double_quote = '"'

! ----------------------------------------------------------------------

!  names must be made of alphanumeric characters only

character( len= *), parameter, public :: alpha_chars = 'abcdefghijklmnopqrstuvwxyz'

character( len= *), parameter, public :: digit_chars = '0123456789'

character( len= *), parameter, public :: underscore = '_'

character( len= *), parameter, public :: alphanum_chars =  alpha_chars // digit_chars // underscore

! ----------------------------------------------------------------------

!  character is special to getopt()

character( len= *), parameter, public :: colon = ':'

!  the default substitution key

character( len= *), parameter, public :: arg_key = '?'

!  length of ?name?

integer, parameter, public :: target_len = len( arg_key) + symbol_name_len + len( arg_key)

! ----------------------------------------------------------------------

!  conversion of integers to strings- 10 digits supports 32 bit values

! ----------------------------------------------------------------------

!  length of strings used to convert between integers and characters

integer, parameter, public :: conversion_len = 10

!  format used to convert between integers and characters

character( len= *), parameter, public :: conversion_fmt = '( i10)'

! ----------------------------------------------------------------------

!  constants types

! ----------------------------------------------------------------------

!  a status value and message

type, public :: status_t

   integer :: status = 0
   character( len= status_msg_len) :: message = null_string

end type status_t

! ----------------------------------------------------------------------

!  constants data

! ----------------------------------------------------------------------

!  coco status value and message

type( status_t), save, public :: state

! ----------------------------------------------------------------------

!  constants library

! ----------------------------------------------------------------------

public :: is_status_ok
public :: is_status_info
public :: is_status_error

! ----------------------------------------------------------------------

contains

! ----------------------------------------------------------------------

function is_status_ok( s) result( l)

type( status_t), intent( in) :: s
logical :: l

continue

   l = s% status == 0

return

end function is_status_ok

! ----------------------------------------------------------------------

function is_status_info( s) result( l)

type( status_t), intent( in) :: s
logical :: l

continue

   l = s% status < 0

return

end function is_status_info

! ----------------------------------------------------------------------

function is_status_error( s) result( l)

type( status_t), intent( in) :: s
logical :: l

continue

   l = s% status > 0

return

end function is_status_error

! ----------------------------------------------------------------------

!  constants

! $Id: coco.f90,v 2.11 2013/04/21 16:23:20 dan Exp $
! **********************************************************************
! eof
end module constants
