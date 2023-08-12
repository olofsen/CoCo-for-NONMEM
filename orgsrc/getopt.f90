! bof
! **********************************************************************
! Fortran module getopt_m

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

!  getopt

! **********************************************************************

module getopt_m

! ----------------------------------------------------------------------

!  getopt_m uses

use :: constants, only: null_string, file_name_len, colon, blank

use :: files, only: msg_quit

! ----------------------------------------------------------------------

!  no implicit typing: require explicit declarations

implicit none

! ----------------------------------------------------------------------

!  all names are private: require explicit exports

private

! ----------------------------------------------------------------------

!  getopt_m RCS strings

! ----------------------------------------------------------------------

!  source file identifier supplied by RCS

character( len= *), public, parameter :: getopt_m_rcs_id = &
   '$Id: coco.f90,v 2.11 2013/04/21 16:23:20 dan Exp $'

! ----------------------------------------------------------------------

!  getopt_m constants

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------

!  getopt() 'no more arguments'

integer, parameter, public :: end_of_args = -1

!  getopt() 'not in optltrs'

character( len= *), parameter, public :: unknown_option = '?'

! ----------------------------------------------------------------------

!  getopt_m data

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------

!  getopt() string returning non-option letter words

character( len= file_name_len), save, public :: optarg = null_string

!  count command line words

integer, save, public :: optind = 0

!  number of command line args

integer, save, public :: nargs

! ----------------------------------------------------------------------

!  getopt_m library

! ----------------------------------------------------------------------

public :: getopt

public :: get_cl_arg_check_len

! **********************************************************************

!  module procedures

! **********************************************************************

contains

! **********************************************************************

!  getopt() return next known option from command line or unknown

integer function getopt( optstring)

! **********************************************************************

!  getopt() interface

! ----------------------------------------------------------------------

!  the string of valid option letters

character( len= *), intent( in) :: optstring

! **********************************************************************

!  getopt() constants

! ----------------------------------------------------------------------

!  special characters

character( len= *), parameter :: dash = '-'

! **********************************************************************

!  getopt() local

! ----------------------------------------------------------------------

!  argument buffer

   character( len= file_name_len) :: optword

!  index in optstring

   integer :: index_optstring

! **********************************************************************

!  getopt() text

continue

! ----------------------------------------------------------------------

!  initialize for next option

   check_inc: if( optind >= nargs )then

      optarg = unknown_option
      getopt = end_of_args

      return

   end if check_inc

! ----------------------------------------------------------------------

!  get next option

   optind = optind + 1

   call get_cl_arg_check_len( optind, optword)

!  if word is not -?

   not_an_option: if( optword( 1: 1) /= dash )then

      optarg = optword
      getopt = end_of_args

      return

!  if word is --

   else if( optword( 2: 2) == dash )then not_an_option

      optarg = unknown_option
      getopt = end_of_args

      return

   end if not_an_option

! ----------------------------------------------------------------------

!  optword is -x (not --)

   index_optstring = index( optstring, optword( 2: 2))

   is_opt: if( index_optstring > 0 )then

!  if this optltr must have another word

      opt_string: if( optstring( index_optstring + 1: index_optstring + 1) == colon )then

!  it can be separated by a blank

         next_word: if( optword( 3: 3) == blank )then

            optind = optind + 1
            call get_cl_arg_check_len( optind, optarg)

!  or not be separated by a blank

         else next_word

            optarg = optword( 3: )

         end if next_word

      end if opt_string

      getopt = ichar( optword( 2: 2))

!  if this optltr must not have another word

   else is_opt

      optarg = optword
      getopt = ichar( unknown_option)

   end if is_opt

! ----------------------------------------------------------------------

!  getopt() exit

return

! **********************************************************************

!  getopt()

end function getopt

! **********************************************************************
! **********************************************************************

!  get_cl_arg_check_len() overwrites the logical value with one from the command line

subroutine get_cl_arg_check_len( iarg, buffer)

! **********************************************************************

!  get_cl_arg_check_len() interface

! ----------------------------------------------------------------------

!  which word

integer, intent( in) :: iarg

!  buffer to be filled

character( len= *), intent( out) :: buffer

! ----------------------------------------------------------------------

!  communicate with get_command_argument

   integer :: cl_stat

   integer :: cl_arg_len

! ----------------------------------------------------------------------

!  get_cl_arg_check_len() text

continue

! ----------------------------------------------------------------------

!  get the length then the value

   call get_command_argument( number= iarg, length= cl_arg_len, status= cl_stat)

!  problem accessing the command line

   len_stat: if( cl_stat > 0 )then

      call msg_quit( "can't get command argument length")

   end if len_stat

!  argument is too long for the buffer

   len_error: if( cl_arg_len > len( buffer) )then

      call msg_quit( "command line argument too long")

   end if len_error

!  return it

   call get_command_argument( number= iarg, value= buffer)

! ----------------------------------------------------------------------

!  get_cl_arg_check_len() exit

return

! **********************************************************************

!  get_cl_arg_check_len()

end subroutine get_cl_arg_check_len

! **********************************************************************
! ----------------------------------------------------------------------

!  getopt_m

! $Id: coco.f90,v 2.11 2013/04/21 16:23:20 dan Exp $
! **********************************************************************
! eof
end module getopt_m
