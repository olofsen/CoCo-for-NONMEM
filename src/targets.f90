! bof
! **********************************************************************
! Fortran module targets

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

!  targets

! **********************************************************************

module targets

! ----------------------------------------------------------------------

!  targets uses modules

use :: constants, only: symbol_name_len, target_len, null_string, arg_key

! ----------------------------------------------------------------------

!  no implicit typing: require explicit declarations

implicit none

! ----------------------------------------------------------------------

!  all names are private: require explicit exports

private

! ----------------------------------------------------------------------

!  targets RCS strings

! ----------------------------------------------------------------------

!  source file identifier supplied by RCS

character( len= *), parameter, public :: targets_rcs_id = &
   '$Id: coco.f90,v 2.11 2013/04/21 16:23:20 dan Exp $'

! ----------------------------------------------------------------------

!  targets constants

! ----------------------------------------------------------------------

!  targets types

! ----------------------------------------------------------------------

type, public :: target_t

   private

   character( len= symbol_name_len) :: name_str = null_string
   integer :: name_len = 0

   character( len= target_len) :: target_str = null_string
   integer :: target_len = 0

end type target_t

! ----------------------------------------------------------------------

!  targets data

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------

!  targets library

! ----------------------------------------------------------------------

!  overload assignment

public :: assignment( =)

interface assignment( =)

   module procedure char_to_target

end interface

!  overload equality

public :: operator( ==)

interface operator( ==)

   module procedure compare_targets

end interface

!  overload concatenation

public :: operator( //)

interface operator( //)

   module procedure char_cat_target

end interface

!  overload index

public :: index

intrinsic :: index

interface index

   module procedure target_index

end interface index

!  trim

public :: trim_name
public :: trim_target

!  accessors

public :: get_target_len
public :: get_name_str

! **********************************************************************

!  module procedures

! **********************************************************************

contains

! ----------------------------------------------------------------------

!  target_t = character( len= *)

subroutine char_to_target( t, c)

type( target_t), intent( out) :: t
character( len= *), intent( in) :: c

continue

   t% name_str = c
   t% name_len= len_trim( c)

   t% target_str = arg_key // trim( c) // arg_key
   t% target_len = len( arg_key) + t% name_len + len( arg_key)

return

end subroutine char_to_target

! ----------------------------------------------------------------------

!  target_t == target_t

function compare_targets( tl, tr) result( e)

type( target_t), intent( in) :: tl, tr

logical :: e

continue

   e = tl% name_str( 1: tl% name_len) == tr% name_str( 1: tr% name_len)

return

end function compare_targets

! ----------------------------------------------------------------------

!  character( len= *) // target_t

function char_cat_target( c, t) result( r)

character( len= *), intent( in) :: c
type( target_t), intent( in) :: t

character( len= len( c) + t% name_len) :: r

continue

   r = c // t% name_str( 1: t% name_len)

return

end function char_cat_target

! ----------------------------------------------------------------------

!  index( character( len= *), target_t)

function target_index( c, t) result( i)

integer :: i

character( len= *), intent( in) :: c
type( target_t), intent( in) :: t

continue

   i = index( c, t% target_str( 1: t% target_len))

return

end function target_index

! ----------------------------------------------------------------------

!  character( len= len_trim( name))

function trim_name( t) result( c)

type( target_t), intent( in) :: t

character( len= t% name_len) :: c

continue

   c = t% name_str( 1: t% name_len)

return

end function trim_name

! ----------------------------------------------------------------------

!  character( len= len_trim( target))

function trim_target( t) result( c)

type( target_t), intent( in) :: t

character( len= t% target_len) :: c

continue

   c = t% target_str( 1: t% target_len)

return

end function trim_target

! ----------------------------------------------------------------------

!  integer = len_trim( target)

function get_target_len( t) result( l)

type( target_t), intent( in) :: t

integer :: l

continue

   l = t% target_len

return

end function get_target_len

! ----------------------------------------------------------------------

!  character( len= len_trim( name))

function get_name_str( t) result( c)

type( target_t), intent( in) :: t

character( len= symbol_name_len) :: c

continue

   c = t% name_str

return

end function get_name_str

! ----------------------------------------------------------------------

!  targets

! $Id: coco.f90,v 2.11 2013/04/21 16:23:20 dan Exp $
! **********************************************************************
! eof
end module targets
