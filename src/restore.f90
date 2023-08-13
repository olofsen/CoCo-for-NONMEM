! bof
! **********************************************************************
! Fortran 95 program restore

! **********************************************************************
! Source Control Strings

! $Id: restore.f90 1.1 2001/01/17 00:10:20Z Dan Exp $

! **********************************************************************
!  Copyright 2000 Purple Sage Computing Solutions, Inc.
!  All Rights Reserved

!   This program is free software; you can redistribute it and/or
!   modify it under the terms of the GNU General Public
!   License as published by the Free Software Foundation; either
!   version 2 of the License, or (at your option) any later version.

!   This program is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!   General Public License for more details.

!   You should have received a copy of the GNU General Public
!   License along with this program; if not, write to the Free
!   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

! To report bugs, suggest enhancements, etc. to the Authors,
! Contact:
!    Purple Sage Computing Solutions, Inc.
!                               send email to dnagle@erols.com
!                                   or fax to 703 471 0684 (USA)
!                                  or mail to 12142 Purple Sage Ct.
!                                             Reston, VA 20194-5621 USA

! **********************************************************************
! restore describe the program

!  This is basically just the restore program from the coco standard.

! **********************************************************************

!  restore reads

!     stdin- source to have its alter: shift3 undone

!  restore writes

!     stdout- source with its coco directives in restored

!  restore uses

!     standard_types

!  restore constants

!  restore types

!  restore data

!  restore library

! **********************************************************************

!  restore

! **********************************************************************

program restore

! **********************************************************************

!  restore uses programs

! **********************************************************************

!  processor description

! use standard_types

  use iso_fortran_env  
  
! **********************************************************************

!  turn off implicit typing

implicit none                                                        ! explicit declarations

! **********************************************************************

!  restore RCS strings

! **********************************************************************

!  program source filename supplied by RCS

character( len= *), parameter :: restore_rcs_id = &
   '$Id: restore.f90 1.1 2001/01/17 00:10:20Z Dan Exp $'

! **********************************************************************

!  restore constants

! **********************************************************************

character( len= *), parameter :: line_fmt = '(a)'

! **********************************************************************

!  restore types

! **********************************************************************

! **********************************************************************

!  restore data

character( len= 135) :: line

integer :: ieof

! **********************************************************************

!  restore text

! **********************************************************************

continue                                                             ! restore

!  print version

   write( unit= error_unit, fmt= line_fmt) restore_rcs_id

!  do until exit

   lines: do                                                         ! do all lines

!  read a line from stdin

      read( unit= input_unit, fmt= line_fmt, iostat= ieof) line      ! read a line

!  if end of file detected, ecit

      detect_eof: if( is_iostat_end( ieof) )then

         exit lines                                                  ! exit at end of file

      endif detect_eof

!  if coco alter: shift3 string found, remove it

      coco_line: if( line( 1: 3) == '!?>' )then

         line( 1: 132) = line( 4: 135)                               ! unshift line

      endif coco_line

!  if coco end of source marker found, exit

      end_of_input: if( line == '?? This was produced using the following SET file' )then

         exit lines                                                  ! exit before setfile

      endif end_of_input

!  write the line

      write( unit= output_unit, fmt= line_fmt) trim( line)           ! write a line

!  do the next line

   enddo lines                                                       ! do all lines

stop 'restore'                                                       ! restore

! **********************************************************************

!  restore

! $Id: restore.f90 1.1 2001/01/17 00:10:20Z Dan Exp $
! **********************************************************************

end program restore                                                  ! eof
