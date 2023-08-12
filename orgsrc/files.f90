! bof
! **********************************************************************
! Fortran module files

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

!  files

! **********************************************************************

module files

! ----------------------------------------------------------------------

!  files uses modules

use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, error_unit

!  get file name length

use :: constants, only: file_name_len, blank, conversion_fmt, conversion_len, buffer_len, null_string, state, is_status_error

!  get options to get verbose setting

use :: switches, only: options

! ----------------------------------------------------------------------

!  no implicit typing: require explicit declarations

implicit none

! ----------------------------------------------------------------------

!  all names are private: require explicit exports

private

! ----------------------------------------------------------------------

!  files RCS strings

! ----------------------------------------------------------------------

!  source file identifier supplied by RCS

character( len= *), parameter, public :: files_rcs_id = &
   '$Id: coco.f90,v 2.11 2013/04/21 16:23:20 dan Exp $'

! ----------------------------------------------------------------------

!  files constants

! ----------------------------------------------------------------------

!  input/output units

! ----------------------------------------------------------------------

!  log file unit else use error_unit, + 4 tries to avoid plot_unit, punch_unit, sundries

integer, parameter :: starting_unit = max( input_unit, output_unit, error_unit, 6) + 4

!  not any logical unit

integer, parameter :: not_a_unit = -1

! ----------------------------------------------------------------------

!  formats

! ----------------------------------------------------------------------

!  used to read/write lines

character( len= *), parameter, public :: string_fmt = '( a: i0: a)'

! ----------------------------------------------------------------------

!  files types

! ----------------------------------------------------------------------

!  coco files and search paths

! ----------------------------------------------------------------------

!  file class

type, public, abstract :: file_t

   integer :: io_unit = not_a_unit

   character( len= buffer_len), pointer :: line => null()

   integer :: lines_transfered = 0

contains

   procedure( file_operation), pass( file), deferred :: open_file

   generic :: open => open_file

   procedure( file_operation), pass( file), deferred :: close_file

   generic :: close => close_file

end type file_t

!  file operations operate on files

abstract interface

   subroutine file_operation( file)
      import :: file_t
      class( file_t), target, intent( in out) :: file
   end subroutine file_operation

end interface

!  a file that will be read as input

type, public, extends( file_t) :: input_file_t

   character( len= file_name_len) :: name_str = null_string

contains

   procedure, pass( file) :: open_file => open_input_file

   procedure, pass( file) :: close_file => close_input_file

   procedure, pass( file) :: read_file => read_input_file

   generic :: read => read_file

end type input_file_t

type( input_file_t), parameter, public :: input_freeze_file = input_file_t( name_str= null_string)

!  a file that will be written as output

type, public, extends( file_t) :: output_file_t

   character( len= file_name_len) :: name_str = null_string

contains

   procedure, pass( file) :: open_file => open_output_file

   procedure, pass( file) :: close_file => close_output_file

end type output_file_t

type( output_file_t), parameter, public :: output_freeze_file = output_file_t( name_str= null_string)

!  a file that will be written as a log file

type, public, extends( file_t) :: log_file_t

   character( len= file_name_len) :: name_str = null_string

contains

   procedure, pass( file) :: open_file => open_log_file

   procedure, pass( file) :: close_file => close_log_file

end type log_file_t

!  a file that will be first written and then read as temporary storage

type, public, extends( file_t) :: scratch_file_t

contains

   procedure, pass( file) :: open_file => open_scratch_file

   procedure, pass( file) :: close_file => close_scratch_file

   procedure, pass( file) :: read_file => read_scratch_file

   generic :: read => read_file

   procedure, pass( file) :: rewind_file => rewind_scratch_file

   generic :: rewind => rewind_file

end type scratch_file_t

! **********************************************************************

!  set_file_name_t stores set file names

! ----------------------------------------------------------------------

type :: set_file_name_t

   logical :: named = .false.

   character( len= file_name_len) :: name_str = null_string

end type set_file_name_t

! ----------------------------------------------------------------------

!  files data

! ----------------------------------------------------------------------

!  input file, output file, or set file

! ----------------------------------------------------------------------

!  the (first) input file

type( input_file_t), target, save, public :: input_file

! ----------------------------------------------------------------------

!  the output file

type( output_file_t), target, save, public :: output_file

! ----------------------------------------------------------------------

!  the set file

type( input_file_t), target, save, public :: set_file

! ----------------------------------------------------------------------

!  the freeze file might be input or output so it might be the current file

class( file_t), allocatable, target, save, public :: freeze_file

! ----------------------------------------------------------------------

!  the log file is never the current file

type( log_file_t), save, public :: log_file

! ----------------------------------------------------------------------

!  point to current input file for error messages

class( file_t), pointer, public :: current_file => null()

! ----------------------------------------------------------------------

!  the input/output line buffer

character( len= buffer_len), target, public :: line

!  the log file line buffer

character( len= buffer_len), target, public :: log_line

!  set file names from various sources

type( set_file_name_t), save, public :: dash_s_name
type( set_file_name_t), save, public :: base_name

! ----------------------------------------------------------------------

!  the next logical unit

integer, save :: next_unit = starting_unit

! **********************************************************************

!  files library

! **********************************************************************

public :: get_class_file_name

public :: is_named
public :: is_connected
public :: is_preconnected

public :: msg_quit
public :: msg_continue

public :: assignment( =)

interface assignment( =)

   module procedure char_to_set_file
   module procedure char_to_file_name

end interface

! **********************************************************************

!  module procedures

! **********************************************************************

contains

! **********************************************************************

!  get_next_unit() return a unit to open the file

function get_next_unit() result( u)

! **********************************************************************

!  get_next_unit() interface

! ----------------------------------------------------------------------

!  the function result

integer :: u

! **********************************************************************

!  get_next_unit() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  get the unit

   u = next_unit
   next_unit = next_unit + 1

! ----------------------------------------------------------------------

!  get_next_unit() exit

return

! **********************************************************************

!  get_next_unit()

end function get_next_unit

! **********************************************************************
! **********************************************************************

!  is_named() true if a file is connected

function is_named( file) result( in)

! **********************************************************************

!  is_named() interface

! ----------------------------------------------------------------------

!  the function result

logical :: in

! ----------------------------------------------------------------------

!  the file to query

class( file_t), intent( in) :: file

! **********************************************************************

!  is_named() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  query the unit

   has_name: select type( file)

   type is( input_file_t) has_name

      in = len_trim( file% name_str) > 0

   type is( output_file_t) has_name

      in = len_trim( file% name_str) > 0

   type is( log_file_t) has_name

      in = len_trim( file% name_str) > 0

   class default has_name

      in = .false.

   end select has_name

! ----------------------------------------------------------------------

!  is_named() exit

return

! **********************************************************************

!  is_named()

end function is_named

! **********************************************************************
! **********************************************************************

!  is_connected() true if a file is connected

function is_connected( file) result( ic)

! **********************************************************************

!  is_connected() interface

! ----------------------------------------------------------------------

!  the function result

logical :: ic

! ----------------------------------------------------------------------

!  the file to query

class( file_t), intent( in) :: file

! **********************************************************************

!  is_connected() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  query the unit

   ic = file% io_unit > not_a_unit

! ----------------------------------------------------------------------

!  is_connected() exit

return

! **********************************************************************

!  is_connected()

end function is_connected

! **********************************************************************
! **********************************************************************

!  is_preconnected() true if a file is preconnected

function is_preconnected( file) result( ip)

! **********************************************************************

!  is_preconnected() interface

! ----------------------------------------------------------------------

!  the function result

logical :: ip

! ----------------------------------------------------------------------

!  the file to query

class( file_t), intent( in) :: file

! **********************************************************************

!  is_preconnected() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  query the unit

   ip = any( file% io_unit == [ input_unit, output_unit, error_unit])

! ----------------------------------------------------------------------

!  is_preconnected() exit

return

! **********************************************************************

!  is_preconnected()

end function is_preconnected

! **********************************************************************
! **********************************************************************

!  char_to_set_file() assign a name to a set_file_name_t

subroutine char_to_set_file( set_file_name, file_name)

! **********************************************************************

!  char_to_set_file() interface

! ----------------------------------------------------------------------

!  the name to be assigned

character( len= *), intent( in) :: file_name

!  the set_file_name_t to get the value

type( set_file_name_t), intent( out) :: set_file_name

! **********************************************************************

!  char_to_set_file() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

   set_file_name% name_str = file_name
   set_file_name% named = .true.

! ----------------------------------------------------------------------

!  char_to_set_file() exit

return

! **********************************************************************

!  char_to_set_file()

end subroutine char_to_set_file

! **********************************************************************
! **********************************************************************

!  open_input_file() open a file for reading and remark

subroutine open_input_file( file)

! **********************************************************************

!  open_input_file() interface

! ----------------------------------------------------------------------

!  the file to be opened

class( input_file_t), target, intent( in out) :: file

! **********************************************************************

!  open_input_file() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  open the file if file is named

   file_has_name: if( is_named( file) )then

!  get a valid unit

      file% io_unit = get_next_unit()

!  open this file

      open( unit= file% io_unit, &
            file= file% name_str, &
            status= 'old', &
            action= 'read', &
            iostat= state% status, &
            iomsg= state% message)

      named_status: if( state% status > 0 )then

         call msg_quit( "can't open input file: " // trim( file% name_str))

      else if( options% verbose_mode )then named_status

         call msg_continue( "opened input file: " // trim( file% name_str) )

      end if named_status

   else file_has_name

      file% name_str = '<stdin>'
      file% io_unit = input_unit

   end if file_has_name

!  set the file pointers

   current_file => file

   file% line => line

! ----------------------------------------------------------------------

!  open_input_file() exit

return

! **********************************************************************

!  open_input_file()

end subroutine open_input_file

! **********************************************************************
! **********************************************************************

!  open_output_file() open a file and remark

subroutine open_output_file( file)

! **********************************************************************

!  open_output_file() interface

! ----------------------------------------------------------------------

!  the file to be opened

class( output_file_t), target, intent( in out) :: file

! **********************************************************************

!  open_output_file() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  open the file if file is named

   file_has_name: if( is_named( file) )then

!  get a valid unit

      file% io_unit = get_next_unit()

!  open this file

      open( unit= file% io_unit, &
            file= file% name_str, &
            status= 'replace', &
            action= 'write', &
            iostat= state% status, &
            iomsg= state% message)

      named_status: if( state% status > 0 )then

         call msg_quit( "can't open output file: " // trim( file% name_str))

      else if( options% verbose_mode )then named_status

         call msg_continue( "opened output file: " // trim( file% name_str) )

      end if named_status

   else file_has_name

      file% name_str = '<stdout>'
      file% io_unit = output_unit

   end if file_has_name

!  output files are the compile file

   file% line => line

! ----------------------------------------------------------------------

!  open_output_file() exit

return

! **********************************************************************

!  open_output_file()

end subroutine open_output_file

! **********************************************************************
! **********************************************************************

!  open_log_file() open a file and remark

subroutine open_log_file( file)

! **********************************************************************

!  open_log_file() interface

! ----------------------------------------------------------------------

!  the file to be opened

class( log_file_t), target, intent( in out) :: file

! **********************************************************************

!  open_log_file() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  open the file if file is named

   file_has_name: if( is_named( file) )then

!  get a valid unit

      file% io_unit = get_next_unit()

!  open this file

      open( unit= file% io_unit, &
            file= file% name_str, &
            status= 'replace', &
            action= 'write', &
            iostat= state% status, &
            iomsg= state% message)

      named_status: if( state% status > 0 )then

         call msg_quit( "can't open output file: " // trim( file% name_str))

      else if( options% verbose_mode )then named_status

         call msg_continue( "opened output file: " // trim( file% name_str) )

      end if named_status

   else file_has_name

      file% name_str = '<stderr>'
      file% io_unit = error_unit

   end if file_has_name

!  the log file receives error messages

   file% line => log_line

! ----------------------------------------------------------------------

!  open_log_file() exit

return

! **********************************************************************

!  open_log_file()

end subroutine open_log_file

! **********************************************************************
! **********************************************************************

!  open_scratch_file() open an unformatted scratch file

subroutine open_scratch_file( file)

! **********************************************************************

!  open_scratch_file() interface

! ----------------------------------------------------------------------

!  the scratch file to be opened

class( scratch_file_t), target, intent( in out) :: file

! **********************************************************************

!  open_scratch_file() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  get a valid unit

   file% io_unit = get_next_unit()

!  open the file

   open( unit= file% io_unit, &
         status= 'scratch', &
         action= 'readwrite', &
         form= 'unformatted', &
         iostat= state% status, &
         iomsg= state% message)

   scratch_status: if( state% status > 0 )then

      current_file => file

      call msg_quit( "can't open scratch file")

   end if scratch_status

!  link to line buffer

   file% line => line

! ----------------------------------------------------------------------

!  open_scratch_file() exit

return

! **********************************************************************

!  open_scratch_file()

end subroutine open_scratch_file

! **********************************************************************
! **********************************************************************

!  close_input_file() close a file and remark

subroutine close_input_file( file)

! **********************************************************************

!  close_input_file() interface

! ----------------------------------------------------------------------

!  the file to be closed

class( input_file_t), target, intent( in out) :: file

! **********************************************************************

!  close_input_file() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  close the named file

   close_named: if( file% io_unit >= starting_unit )then

      close( unit= file% io_unit, &
             status= 'keep', &
             iostat= state% status, &
             iomsg= state% message)

      close_status: if( state% status > 0 )then

         call msg_quit( "can't close input file: " // trim( file% name_str))

      else if( options% verbose_mode )then close_status

        call msg_continue( "closed input file: " // trim( file% name_str) )

      end if close_status

   end if close_named

!  current file is not connected

   nullify( current_file)

! ----------------------------------------------------------------------

!  close_input_file() exit

return

! **********************************************************************

!  close_input_file()

end subroutine close_input_file

! **********************************************************************
! **********************************************************************

!  close_output_file() close a file and remark

subroutine close_output_file( file)

! **********************************************************************

!  close_output_file() interface

! ----------------------------------------------------------------------

!  the file to be closed

class( output_file_t), target, intent( in out) :: file

! **********************************************************************

!  close_output_file() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  close the named file

   close_named: if( file% io_unit >= starting_unit )then

      close( unit= file% io_unit, &
             status= 'keep', &
             iostat= state% status, &
             iomsg= state% message)

      close_status: if( state% status > 0 )then

         call msg_quit( "can't close output file: " // trim( file% name_str))

      else if( options% verbose_mode )then close_status

        call msg_continue( "closed file: " // trim( file% name_str) )

      end if close_status

   end if close_named

! ----------------------------------------------------------------------

!  close_output_file() exit

return

! **********************************************************************

!  close_output_file()

end subroutine close_output_file

! **********************************************************************
! **********************************************************************

!  close_log_file() close a file and remark

subroutine close_log_file( file)

! **********************************************************************

!  close_log_file() interface

! ----------------------------------------------------------------------

!  the file to be closed

class( log_file_t), target, intent( in out) :: file

! **********************************************************************

!  close_log_file() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  close the named file

   close_named: if( file% io_unit >= starting_unit )then

      close( unit= file% io_unit, &
             status= 'keep', &
             iostat= state% status, &
             iomsg= state% message)

!  error messages always go somewhere

      file% io_unit = error_unit

      close_status: if( state% status > 0 )then

         call msg_quit( "can't close output file: " // trim( file% name_str))

      else if( options% verbose_mode )then close_status

        call msg_continue( "closed file: " // trim( file% name_str) )

      end if close_status

   end if close_named

! ----------------------------------------------------------------------

!  close_log_file() exit

return

! **********************************************************************

!  close_log_file()

end subroutine close_log_file

! **********************************************************************
! **********************************************************************

!  close_scratch_file() close a file and remark

subroutine close_scratch_file( file)

! **********************************************************************

!  close_scratch_file() interface

! ----------------------------------------------------------------------

!  the scratch file to be closed

class( scratch_file_t), target, intent( in out) :: file

! **********************************************************************

!  close_scratch_file() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  close the scratch file

   close( unit= file% io_unit, &
          status= 'delete', &
          iostat= state% status, &
          iomsg= state% message)

   close_status: if( state% status > 0 )then

      call msg_quit( "can't close scratch file")

   end if close_status

! ----------------------------------------------------------------------

!  close_scratch_file() exit

return

! **********************************************************************

!  close_scratch()

end subroutine close_scratch_file

! **********************************************************************
! **********************************************************************

!  read_input_file() read line from a file

subroutine read_input_file( file)

! **********************************************************************

!  read_input_file() interface

! ----------------------------------------------------------------------

!  the file to be read

class( input_file_t), target, intent( in out) :: file

! **********************************************************************

!  read_input_file() constants

character( len= *), parameter :: read_fmt = '( a)'

! **********************************************************************

!  read_input_file() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  read the input file

   read( unit= file% io_unit, &
         fmt= read_fmt, &
         iostat= state% status, &
         iomsg= state% message) file% line

   read_status: if( is_status_error( state) )then

      call msg_quit( "can't read file " // file% name_str)

   end if read_status

!  count input file lines

   file% lines_transfered = file% lines_transfered + 1

! ----------------------------------------------------------------------

!  read_input_file() exit

return

! **********************************************************************

!  read_input_file()

end subroutine read_input_file

! **********************************************************************
! **********************************************************************

!  read_scratch_file() read line from a file

subroutine read_scratch_file( file, string)

! **********************************************************************

!  read_scratch_file() interface

! ----------------------------------------------------------------------

!  the file to be read

class( scratch_file_t), target, intent( in out) :: file

!  the line to be read

character( len= *), intent( out) :: string

! **********************************************************************

!  read_scratch_file() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  read the scratch file

   read( unit= file% io_unit, &
         iostat= state% status, &
         iomsg= state% message) string

   read_status: if( is_status_error( state) )then

      call msg_quit( "can't read scratch file")

   end if read_status

! ----------------------------------------------------------------------

!  read_scratch_file() exit

return

! **********************************************************************

!  read_scratch_file()

end subroutine read_scratch_file

! **********************************************************************
! **********************************************************************

!  rewind_scratch_file() rewind a file

subroutine rewind_scratch_file( file)

! **********************************************************************

!  rewind_scratch_file() interface

! ----------------------------------------------------------------------

!  the file to be rewound

class( scratch_file_t), target, intent( in out) :: file

! **********************************************************************

!  rewind_scratch_file() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  rewind the scratch file

   rewind( unit= file% io_unit, &
           iostat= state% status, &
           iomsg= state% message)

   rewind_status: if( is_status_error( state) )then

      call msg_quit( "can't rewind scratch file")

   end if rewind_status

! ----------------------------------------------------------------------

!  rewind_scratch_file() exit

return

! **********************************************************************

!  rewind_scratch_file()

end subroutine rewind_scratch_file

! **********************************************************************
! **********************************************************************

!  get_class_file_name() overwrites the logical value with one from the command line

pure function get_class_file_name( file) result( name_str)

! **********************************************************************

!  get_class_file_name() interface

character( len= file_name_len) :: name_str

! ----------------------------------------------------------------------

!  keys to check

class( file_t), intent( in) :: file

! ----------------------------------------------------------------------

!  get_class_file_name() text

continue

! ----------------------------------------------------------------------

!  check the list

   get_name: select type( file)

   type is( input_file_t) get_name

      name_str = file% name_str

   type is( output_file_t) get_name

      name_str = file% name_str

   type is( scratch_file_t) get_name

      name_str = '<scratch>'

   end select get_name

! ----------------------------------------------------------------------

!  get_class_file_name() exit

return

! **********************************************************************

!  get_class_file_name()

end function get_class_file_name

! **********************************************************************
! **********************************************************************

!  char_to_file_name() set a file name

subroutine char_to_file_name( file, string)

! **********************************************************************

!  char_to_file_name() interface

character( len= file_name_len), intent( in) :: string

! ----------------------------------------------------------------------

!  keys to check

class( file_t), intent( in out) :: file

! ----------------------------------------------------------------------

!  char_to_file_name() text

continue

! ----------------------------------------------------------------------

!  check the list

   get_name: select type( file)

   type is( input_file_t) get_name

      file% name_str = string

   type is( output_file_t) get_name

      file% name_str = string

   end select get_name

! ----------------------------------------------------------------------

!  char_to_file_name() exit

return

! **********************************************************************

!  char_to_file_name()

end subroutine char_to_file_name

! **********************************************************************
! **********************************************************************

!  msg_quit() process error and stop

subroutine msg_quit( msg)

! **********************************************************************

!  msg_quit() interface

! ----------------------------------------------------------------------

!  the error message

character( len= *), intent( in) :: msg

! **********************************************************************

!  msg_quit() local

! ----------------------------------------------------------------------

!  strings conatining the line number and iostat of the failed operation

   character( len= conversion_len) :: number_str

   character( len= conversion_len) :: iostat_str

!  construct a message that might include a processor message

   character( len= buffer_len) :: quit_msg

! **********************************************************************

!  msg_quit() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  is there a processor message to include?

   have_processor_message: if( is_status_error( state) )then

      quit_msg = msg // blank // state% message

   else have_processor_message

      quit_msg = msg

   end if have_processor_message

!  if file is associated with this error

   file_msg: if( associated( current_file) )then

!  if a line is associated with this error

      line_msg: if( associated( current_file% line) )then

         write( unit= log_file% io_unit, fmt= string_fmt) trim( current_file% line)

      end if line_msg

!  if io error caused this error

      io_error: if( is_status_error( state) )then

!  decode line number & iostat

         write( unit= number_str, fmt= conversion_fmt) current_file% lines_transfered

         write( unit= iostat_str, fmt= conversion_fmt) state% status

!  write message with file data

         write( unit= log_file% io_unit, fmt= string_fmt) 'coco message: file: ' // trim( get_class_file_name( current_file)) &
                     // ', line: ' // trim( adjustl( number_str)) // ': ' // trim( quit_msg)

!  if io error caused not this error

      else io_error

!  decode line number

         write( unit= number_str, fmt= conversion_fmt) current_file% lines_transfered

!  write message without file data

         write( unit= log_file% io_unit, fmt= string_fmt) 'coco message: file: ' // trim( get_class_file_name( current_file)) &
                     // ', line: ' // trim( adjustl( number_str)) // ': ' // trim( quit_msg)

      end if io_error

!  if file associated not with this error

   else file_msg

!  write error message without file data

      write( unit= log_file% io_unit, fmt= string_fmt) 'coco error: ' // trim( quit_msg)

   end if file_msg

! ----------------------------------------------------------------------

!  msg_quit() exit

stop 'coco error exit'

! **********************************************************************

!  msg_quit()

end subroutine msg_quit

! **********************************************************************
! **********************************************************************

!  msg_continue() print message or continue processing

subroutine msg_continue( msg)

! **********************************************************************

!  msg_continue() interface

! ----------------------------------------------------------------------

!  the warning or informational message

character( len= *), intent( in) :: msg

! **********************************************************************

!  msg_continue() local

! ----------------------------------------------------------------------

!  string containing the current input line number

   character( len= conversion_len) :: number_str

! **********************************************************************

!  msg_continue() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  if a file is associated with this message

   file_msg: if( associated( current_file) )then

!  decode line number

      write( unit= number_str, fmt= conversion_fmt) current_file% lines_transfered

!  write message with file data

      write( unit= log_file% io_unit, fmt= string_fmt) 'coco message: file: ' &
             // trim( get_class_file_name( current_file)) // ', line: ' // trim( adjustl( number_str)) // ': ' // trim( msg)

!  if no file is associated with this message

   else file_msg

!  write message without file data

      write( unit= log_file% io_unit, fmt= string_fmt) 'coco message: ' // trim( msg)

   end if file_msg

! ----------------------------------------------------------------------

!  msg_continue() exit

return

! **********************************************************************

!  msg_continue()

end subroutine msg_continue

! **********************************************************************
! ----------------------------------------------------------------------

!  files

! $Id: coco.f90,v 2.11 2013/04/21 16:23:20 dan Exp $
! **********************************************************************
! eof
end module files
