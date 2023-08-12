! bof
! **********************************************************************
! Fortran module switches

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

!  switches

! **********************************************************************

module switches

! ----------------------------------------------------------------------

!  use modules

! ----------------------------------------------------------------------

!  get file name length

use :: constants, only: file_name_len, null_string, arg_key, blank

! ----------------------------------------------------------------------

!  no implicit typing: require explicit declarations

implicit none

! ----------------------------------------------------------------------

!  all names are private: require explicit exports

private

! ----------------------------------------------------------------------

!  switches RCS strings

! ----------------------------------------------------------------------

!  source file identifier supplied by RCS

character( len= *), parameter, public :: switches_rcs_id = &
   '$Id: coco.f90,v 2.11 2013/04/21 16:23:20 dan Exp $'

! ----------------------------------------------------------------------

!  switches types

! ----------------------------------------------------------------------

!  state_t stores a set of coco options

! ----------------------------------------------------------------------

!  state_t

type, public :: state_t

   integer :: count_input_files = 0

   integer :: alter_state = 0

   logical :: free_form = .true.

   logical :: freeze_declarations = .false.

   character( len= file_name_len) :: freeze_file_name = null_string

   logical :: mark_input = .false.

   logical :: number_source = .false.

   logical :: print_report = .false.

   logical :: postpend_set_file = .true.

   logical :: seek_set_file = .true.

   logical :: verbose_mode = .false.

   logical :: wrapping_lines = .true.

   logical :: got_sep_char = .false.

   character( len= 1) :: sep_char = blank

   logical :: warning = .false.

   integer :: wrap_len = 0

end type state_t

! ----------------------------------------------------------------------

!  switches data

! ----------------------------------------------------------------------

!  option switches

! ----------------------------------------------------------------------

!  options actually used and those set from set file

type( state_t), save, public :: options

!  options from the command line override the set file options

type( state_t), save, public :: cl_options

! **********************************************************************

!  switches

! $Id: coco.f90,v 2.11 2013/04/21 16:23:20 dan Exp $
! **********************************************************************
! eof
end module switches
