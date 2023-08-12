! bof
! **********************************************************************
! Fortran module values

! $Id: coco.f90,v 2.11 2013/04/21 16:23:20 dan Exp $
! ----------------------------------------------------------------------
!  Copyright 2013 Dan Nagle

!   This library is free software; you can redistribute it and/or
!   modify it under the terms of the GNU General Public
!   License as published by the Free Software Foundation; either
!   version 3 of the License, or (at your option) any later version.

!   This library is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!   Library General Public License for more details.

!   You should have received a copy of the GNU Library General Public
!   License along with this library; if not, write to the Free
!   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

! To report bugs, suggest enhancements, etc. to the Authors,
! Contact:
!    Dan Nagle
!                               send email to dannagle@verizon.net
!                                  or mail to 731 Spruce St
!                                             Boulder CO 80302 USA

! ----------------------------------------------------------------------

!  values

! **********************************************************************

module values

! ----------------------------------------------------------------------

!  values uses modules

use :: constants, only: buffer_len, null_string

! ----------------------------------------------------------------------

!  no implicit typing: require explicit declarations

implicit none

! ----------------------------------------------------------------------

!  all names are private: require explicit exports

private

! ----------------------------------------------------------------------

!  values RCS strings

! ----------------------------------------------------------------------

!  source file identifier supplied by RCS

character( len= *), public, parameter :: values_rcs_id = &
   '$Id: coco.f90,v 2.11 2013/04/21 16:23:20 dan Exp $'

! ----------------------------------------------------------------------

!  values constants

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------

!  values types

! ----------------------------------------------------------------------

type, public :: value_t

   private

   character( len= buffer_len) :: value_str = null_string
   integer :: value_len = 0

end type value_t

! ----------------------------------------------------------------------

!  values data

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------

!  values library

! ----------------------------------------------------------------------

!  overload assignment

public :: assignment( =)

interface assignment( =)

   module procedure char_to_value
   module procedure value_to_char

end interface

!  overload concatenation

public :: operator( //)

interface operator( //)

   module procedure char_cat_value

end interface

!  extract value string

public :: trim_value
public :: set_value

! **********************************************************************

!  module procedures

! **********************************************************************

contains

! ----------------------------------------------------------------------

subroutine char_to_value( v, c)

character( len= *), intent( in) :: c
type( value_t), intent( out) :: v

continue

   v% value_str = c
   v% value_len = len_trim( c)

return

end subroutine char_to_value

! ----------------------------------------------------------------------

subroutine value_to_char( c, v)

type( value_t), intent( in) :: v
character( len= v% value_len), intent( out) :: c

continue

   c = v% value_str( 1: v% value_len)

return

end subroutine value_to_char

! ----------------------------------------------------------------------

function char_cat_value( c, v) result( e)

character( len= *), intent( in) :: c
type( value_t), intent( in) :: v

character( len= len( c) + v% value_len) :: e

continue

   e = c // v% value_str( 1: v% value_len)

return

end function char_cat_value

! ----------------------------------------------------------------------

function trim_value( v) result( c)

type( value_t), intent( in) :: v

character( len= v% value_len) :: c

continue

   c = v% value_str( 1: v% value_len)

return

end function trim_value

! ----------------------------------------------------------------------

function set_value( c) result( v)

character( len= *), intent( in) :: c

type( value_t) :: v

continue

   v% value_str = c
   v% value_len = len( c)

return

end function set_value

! ----------------------------------------------------------------------

!  values

! $Id: coco.f90,v 2.11 2013/04/21 16:23:20 dan Exp $
! **********************************************************************
! eof
end module values
