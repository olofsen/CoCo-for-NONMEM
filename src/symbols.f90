! bof
! **********************************************************************
! Fortran module symbols

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

! To report bugs, suggest enhancements, and so on, to the Authors,
! Contact:
!    Dan Nagle
!                               send email to dannagle@verizon.net
!                                  or mail to 731 Spruce St
!                                             Boulder CO 80302 USA

! ----------------------------------------------------------------------

!  symbols

! **********************************************************************

module symbols

! ----------------------------------------------------------------------

!  symbols uses modules

use :: constants, only: file_name_len, null_string, buffer_len, conversion_len, conversion_fmt, blank

use :: values, only: value_t, operator( //)

use :: targets, only: target_t, operator( //), get_name_str

use :: files, only: output_file_t, log_file, string_fmt, msg_quit

! ----------------------------------------------------------------------

!  no implicit typing: require explicit declarations

implicit none

! ----------------------------------------------------------------------

!  all names are private: require explicit exports

private

! ----------------------------------------------------------------------

!  symbols RCS strings

! ----------------------------------------------------------------------

!  source file identifier supplied by RCS

character( len= *), public, parameter :: symbols_rcs_id = &
   '$Id: coco.f90,v 2.11 2013/04/21 16:23:20 dan Exp $'

! ----------------------------------------------------------------------

!  symbols constants

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------

!  subscripts of predefined macros

integer, parameter, public :: file_ss = 1

integer, parameter, public :: line_ss = 2

integer, parameter, public :: date_ss = 3

integer, parameter, public :: time_ss = 4

integer, parameter, public :: coco_ss = 5

integer, parameter, public :: setfile_ss = 6

integer, parameter, public :: logfile_ss = 7

integer, parameter, public :: output_ss = 8

integer, parameter, public :: cmdline_ss = 9

integer, parameter, public :: user_ss = 10

integer, parameter, public :: cwd_ss = 11

integer, parameter, public :: incpath_ss = 12

integer, parameter, public :: freeze_ss = 13

integer, parameter, public :: null_ss = 14

integer, parameter, public :: blank_ss = 15

integer, parameter, public :: eval_ss = 16

integer, parameter, public :: predefined_size = eval_ss - file_ss + 1

! ----------------------------------------------------------------------

!  symbols types

! ----------------------------------------------------------------------

! **********************************************************************

!  this derived type is used to store coco predefined macros

! ----------------------------------------------------------------------

!  type stores a predefined macro

type, public :: predefined_t

   type( target_t) :: name_str

   logical :: referenced

   character( len= file_name_len) :: referenced_file

   integer :: referenced_line

   type( value_t) :: macro_value

end type predefined_t

! **********************************************************************

!  these derived types are used to store coco constants or variables

! ----------------------------------------------------------------------

!  type stores a generic coco symbol

type, abstract, public :: symbol_t

   type( target_t) :: name_str

   character( len= file_name_len) :: declared_file = null_string

   integer :: declared_line = 0

   logical :: referenced = .false.

   character( len= file_name_len) :: referenced_file = null_string

   integer :: referenced_line = 0

   class( symbol_t), pointer :: next => null()

end type symbol_t

!  type stores a logical coco symbol

type, extends( symbol_t), public :: logical_t

   logical :: defined = .false.

   character( len= file_name_len) :: defined_file = null_string

   integer :: defined_line = 0

   logical :: constant = .false.

   logical :: sf_defined = .false.

   logical :: cl_defined = .false.

   logical :: logical_value = .false.

end type logical_t

!  type stores an integer coco symbol

type, extends( symbol_t), public :: integer_t

   logical :: defined = .false.

   character( len= file_name_len) :: defined_file = null_string

   integer :: defined_line = 0

   logical :: constant = .false.

   logical :: sf_defined = .false.

   logical :: cl_defined = .false.

   integer :: integer_value = 0

end type integer_t

!  type stores a macro coco symbol

type, extends( symbol_t), public :: macro_t

   logical :: args_in_parens = .false.

   type( target_t), dimension( :), allocatable :: dummy_args

   character( len= buffer_len), dimension( :), allocatable :: actual_args

   character( len= buffer_len) :: macro_value = null_string

end type macro_t

!  type stores a text coco symbol

type, extends( symbol_t), public :: text_t

   logical :: args_in_parens = .false.

   type( target_t), dimension( :), allocatable :: dummy_args

   character( len= buffer_len), dimension( :), allocatable :: actual_args

   character( len= buffer_len), dimension( :), allocatable :: text_lines

end type text_t

!  type stores a file coco symbol

type, extends( symbol_t), public :: file_symbol_t

   logical :: active = .false.

   type( output_file_t) :: file

end type file_symbol_t

! ----------------------------------------------------------------------

!  symbols data

! ----------------------------------------------------------------------

type( file_symbol_t), pointer, public :: alternate_output => null()

! ----------------------------------------------------------------------

!  predefined macros: file, line, date, time, coco, setfile, logfile, output, cmdline, incpath, freeze, null, blank

type( predefined_t), dimension( 1: predefined_size), save, public :: predefined_macros

! ----------------------------------------------------------------------

!  coco symbols are stored in a singly linked list

class( symbol_t), pointer, save, public :: first_symbol => null()
class( symbol_t), pointer, save, public :: last_symbol => null()

! ----------------------------------------------------------------------

!  coco symbols from the set file

class( symbol_t), pointer, save, public :: first_sf_symbol => null()
class( symbol_t), pointer, save, public :: last_sf_symbol => null()

! ----------------------------------------------------------------------

!  coco symbols from the command line

class( symbol_t), pointer, save, public :: first_cl_symbol => null()
class( symbol_t), pointer, save, public :: last_cl_symbol => null()

! ----------------------------------------------------------------------

!  symbols library

! ----------------------------------------------------------------------

public :: write_symbols

public :: seek_symbol_name
public :: seek_cl_symbol_name
public :: seek_sf_symbol_name

public :: get_next_symbol
public :: get_next_integer
public :: get_next_logical
public :: get_next_macro
public :: get_next_file

! **********************************************************************

!  module procedures

! **********************************************************************

contains

! **********************************************************************

!  write_symbols() write symbol list to the log file

subroutine write_symbols()

! **********************************************************************

!  write_symbols() local

! ----------------------------------------------------------------------

!  symbol to be written to the log file

   class( symbol_t), pointer :: symbol_ptr

!  convert line numbers to strings

   character( len= conversion_len) :: line_no_str

!  print dummy arguments

   character( len= buffer_len) :: args_line

!  predefined macros index and block line index

   integer :: i

! **********************************************************************

!  write_symbols() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  predefined symbols

   write( unit= log_file% io_unit, fmt= string_fmt) 'predefined symbols'

   all_predefined: do i = 1, size( predefined_macros)

      write( unit= log_file% io_unit, fmt= string_fmt) 'name: ' // predefined_macros( i)% name_str

      write( unit= log_file% io_unit, fmt= string_fmt) 'value: ' // predefined_macros( i)% macro_value

      predefined_referenced: if( predefined_macros( i)% referenced )then

         write( unit= line_no_str, fmt= conversion_fmt) predefined_macros( i)% referenced_line

         write( unit= log_file% io_unit, fmt= string_fmt) 'referenced in file: ' &
                                                          // trim( predefined_macros( i)% referenced_file) &
                                                          // ' at line: ' // trim( adjustl( line_no_str))

      else predefined_referenced

         write( unit= log_file% io_unit, fmt= string_fmt) 'never referenced'

      end if predefined_referenced

   end do all_predefined

! ----------------------------------------------------------------------

   symbol_ptr => first_symbol

   write( unit= log_file% io_unit, fmt= string_fmt) 'symbols'

   all_symbols: do

      if( .not. associated( symbol_ptr) ) exit all_symbols

      write( unit= log_file% io_unit, fmt= string_fmt) 'next symbol'

! ----------------------------------------------------------------------

!  data all symbols have

      write( unit= log_file% io_unit, fmt= string_fmt) 'name: ' // symbol_ptr% name_str

      write( unit= line_no_str, fmt= conversion_fmt) symbol_ptr% declared_line

      write( unit= log_file% io_unit, fmt= string_fmt) 'declared in file: ' // trim( symbol_ptr% declared_file) &
                                                            // ' at line: ' // trim( adjustl( line_no_str))

      ever_referenced: if( symbol_ptr% referenced )then

         write( unit= line_no_str, fmt= conversion_fmt) symbol_ptr% referenced_line

         write( unit= log_file% io_unit, fmt= string_fmt) 'referenced in file: ' // trim( symbol_ptr% referenced_file) &
                                                                 // ' at line: ' // trim( adjustl( line_no_str))

      else ever_referenced

         write( unit= log_file% io_unit, fmt= string_fmt) 'never referenced'

      end if ever_referenced

! ----------------------------------------------------------------------

!  type-specific data

      print_type: select type( symbol_ptr)

! ----------------------------------------------------------------------

!  logical data

      type is( logical_t)

         write( unit= log_file% io_unit, fmt= string_fmt, advance= 'no') 'type: logical '

         log_const_or_var: if( symbol_ptr% constant )then

            write( unit= log_file% io_unit, fmt= string_fmt) 'constant'

         else log_const_or_var

            write( unit= log_file% io_unit, fmt= string_fmt) 'variable'

         end if log_const_or_var

         logical_defined: if( symbol_ptr% defined )then

            true_false: if( symbol_ptr% logical_value )then

               write( unit= log_file% io_unit, fmt= string_fmt) 'value: .true.'

            else true_false

               write( unit= log_file% io_unit, fmt= string_fmt) 'value: .false.'

            end if true_false

            write( unit= line_no_str, fmt= conversion_fmt) symbol_ptr% defined_line

            write( unit= log_file% io_unit, fmt= string_fmt) 'defined in file: ' // trim( symbol_ptr% defined_file) &
                                                             // ' at line: ' // trim( adjustl( line_no_str))

         else logical_defined

            write( unit= log_file% io_unit, fmt= string_fmt) 'value: <undefined>'

         end if logical_defined

! ----------------------------------------------------------------------

!  integer data

      type is( integer_t)

         write( unit= log_file% io_unit, fmt= string_fmt, advance= 'no') 'type: integer '

         int_const_or_var: if( symbol_ptr% constant )then

            write( unit= log_file% io_unit, fmt= string_fmt) 'constant'

         else int_const_or_var

            write( unit= log_file% io_unit, fmt= string_fmt) 'variable'

         end if int_const_or_var

         integer_defined: if( symbol_ptr% defined )then

            write( unit= log_file% io_unit, fmt= string_fmt) 'value: ', symbol_ptr% integer_value

            write( unit= line_no_str, fmt= conversion_fmt) symbol_ptr% defined_line

            write( unit= log_file% io_unit, fmt= string_fmt) 'defined in file: ' // trim( symbol_ptr% defined_file) &
                                                             // ' at line: ' // trim( adjustl( line_no_str))

         else integer_defined

            write( unit= log_file% io_unit, fmt= string_fmt) 'value: <undefined>'

         end if integer_defined

! ----------------------------------------------------------------------

!  macro data

      type is( macro_t)

         macro_args: if( allocated( symbol_ptr% dummy_args) )then

            macro_parens: if( symbol_ptr% args_in_parens )then

               write( unit= log_file% io_unit, fmt= string_fmt) 'type: macro with arguments in parenthesis'

            else macro_parens

               write( unit= log_file% io_unit, fmt= string_fmt) 'type: macro with arguments'

            end if macro_parens

            args_line = null_string

            line_macro: do i = 1, size( symbol_ptr% dummy_args)

               args_line = trim( args_line) // blank // symbol_ptr% dummy_args( i)

            end do line_macro

            write( unit= log_file% io_unit, fmt= string_fmt) 'dummy arguments: ' // trim( args_line)

         else macro_args

            write( unit= log_file% io_unit, fmt= string_fmt) 'type: macro'

         end if macro_args

         write( unit= log_file% io_unit, fmt= string_fmt) 'value: ' // trim( symbol_ptr% macro_value)

! ----------------------------------------------------------------------

!  text data

      type is( text_t)

         text_args: if( allocated( symbol_ptr% dummy_args) )then

            text_parens: if( symbol_ptr% args_in_parens )then

               write( unit= log_file% io_unit, fmt= string_fmt) 'type: text with arguments in parenthesis'

            else text_parens

               write( unit= log_file% io_unit, fmt= string_fmt) 'type: text with arguments'

            end if text_parens

            args_line = null_string

            line_text: do i = 1, size( symbol_ptr% dummy_args)

               args_line = trim( args_line) // blank // symbol_ptr% dummy_args( i)

            end do line_text

            write( unit= log_file% io_unit, fmt= string_fmt) 'dummy arguments: ' // trim( args_line)

         else text_args

            write( unit= log_file% io_unit, fmt= string_fmt) 'type: text'

         end if text_args

         write( unit= log_file% io_unit, fmt= string_fmt) 'block:'

         write_block: do i = 1, size( symbol_ptr% text_lines)

            write( unit= log_file% io_unit, fmt= string_fmt) trim( symbol_ptr% text_lines( i))

         end do write_block

! ----------------------------------------------------------------------

!  file variable

      type is( file_symbol_t)

         write( unit= log_file% io_unit, fmt= string_fmt) 'type: file '

         write( unit= log_file% io_unit, fmt= string_fmt) 'file name: ' // trim( symbol_ptr% file% name_str)

         write( unit= log_file% io_unit, fmt= string_fmt) 'lines: ', symbol_ptr% file% lines_transfered

! ----------------------------------------------------------------------

      class default

         call msg_quit( 'error: type is unknown')

      end select print_type

      symbol_ptr => symbol_ptr% next

   end do all_symbols

   write( unit= log_file% io_unit, fmt= string_fmt) 'end symbols'

! ----------------------------------------------------------------------

!  write_symbols() exit

return

! **********************************************************************

!  write_symbols()

end subroutine write_symbols

! **********************************************************************
! **********************************************************************

!  seek_symbol_name() seek symbol on symbol list

subroutine seek_symbol_name( name_str, symbol_ptr)

! **********************************************************************

!  seek_symbol_name() interface

! ----------------------------------------------------------------------

!  the name of the symbol being sought

character( len= *), intent( in) :: name_str

!  a pointer to the symbol found

class( symbol_t), pointer :: symbol_ptr

! **********************************************************************

!  entry: symbol_str is blank_compress_lower_case logical symbol directive
!         "name..."

!  exit: symbol found or not in logical symbol array

! **********************************************************************

!  seek_symbol_name() text

continue

! ----------------------------------------------------------------------

!  search symbol list

   symbol_ptr => first_symbol

   all_symbols: do

      if( .not. associated( symbol_ptr) ) exit all_symbols

      name_match: if( name_str == get_name_str( symbol_ptr% name_str) )then

         exit all_symbols

      end if name_match

      symbol_ptr => symbol_ptr% next

   end do all_symbols

! ----------------------------------------------------------------------

!  seek_symbol_name() exit

return

! **********************************************************************

!  seek_symbol_name()

end subroutine seek_symbol_name

! **********************************************************************
! **********************************************************************

!  seek_cl_symbol_name() seek symbol on symbol list

subroutine seek_cl_symbol_name( name_str, symbol_ptr)

! **********************************************************************

!  seek_cl_symbol_name() interface

! ----------------------------------------------------------------------

!  the name of the symbol being sought

character( len= *), intent( in) :: name_str

!  a pointer to the symbol found

class( symbol_t), pointer :: symbol_ptr

! **********************************************************************

!  entry: symbol_str is blank_compress_lower_case logical symbol directive
!         "name..."

!  exit: symbol found or not in logical symbol array

! **********************************************************************

!  seek_cl_symbol_name() text

continue

! ----------------------------------------------------------------------

!  search symbol list

   symbol_ptr => first_cl_symbol

   all_symbols: do

      if( .not. associated( symbol_ptr) ) exit all_symbols

      name_match: if( name_str == get_name_str( symbol_ptr% name_str) )then

         exit all_symbols

      end if name_match

      symbol_ptr => symbol_ptr% next

   end do all_symbols

! ----------------------------------------------------------------------

!  seek_cl_symbol_name() exit

return

! **********************************************************************

!  seek_cl_symbol_name()

end subroutine seek_cl_symbol_name

! **********************************************************************
! **********************************************************************

!  seek_sf_symbol_name() seek symbol on symbol list

subroutine seek_sf_symbol_name( name_str, symbol_ptr)

! **********************************************************************

!  seek_sf_symbol_name() interface

! ----------------------------------------------------------------------

!  the name of the symbol being sought

character( len= *), intent( in) :: name_str

!  a pointer to the symbol found

class( symbol_t), pointer :: symbol_ptr

! **********************************************************************

!  entry: symbol_str is blank_compress_lower_case logical symbol directive
!         "name..."

!  exit: symbol found or not in logical symbol array

! **********************************************************************

!  seek_sf_symbol_name() text

continue

! ----------------------------------------------------------------------

!  search symbol list

   symbol_ptr => first_sf_symbol

   all_symbols: do

      if( .not. associated( symbol_ptr) ) exit all_symbols

      name_match: if( name_str == get_name_str( symbol_ptr% name_str) )then

         exit all_symbols

      end if name_match

      symbol_ptr => symbol_ptr% next

   end do all_symbols

! ----------------------------------------------------------------------

!  seek_sf_symbol_name() exit

return

! **********************************************************************

!  seek_sf_symbol_name()

end subroutine seek_sf_symbol_name

! **********************************************************************
! **********************************************************************

!  get_next_symbol() seek symbol on symbol list

subroutine get_next_symbol( symbol_ptr)

! **********************************************************************

!  get_next_symbol() interface

! ----------------------------------------------------------------------

!  a pointer to the next symbol on the symbol list

class( symbol_t), pointer :: symbol_ptr

! **********************************************************************

!  entry: symbol_str is blank_compress_lower_case logical symbol directive
!         "name..."

!  exit: symbol found or not in logical symbol array

! **********************************************************************

!  get_next_symbol() text

continue

! ----------------------------------------------------------------------

!  start at the symbol list head or continue from previous symbol

   start_or_continue: if( associated( symbol_ptr) )then

      symbol_ptr => symbol_ptr% next

   else start_or_continue

      symbol_ptr => first_symbol

   end if start_or_continue

! ----------------------------------------------------------------------

!  get_next_symbol() exit

return

! **********************************************************************

!  get_next_symbol()

end subroutine get_next_symbol

! **********************************************************************
! **********************************************************************

!  get_next_integer() seek symbol on symbol list

subroutine get_next_integer( integer_ptr)

! **********************************************************************

!  get_next_integer() interface

! ----------------------------------------------------------------------

!  a pointer to the next integer on the symbol list

type( integer_t), pointer :: integer_ptr

! **********************************************************************

!  entry: symbol_str is blank_compress_lower_case logical symbol directive
!         "name..."

!  exit: symbol found or not in logical symbol array

! **********************************************************************

!  get_next_integer() local

   class( symbol_t), pointer :: symbol_ptr

! **********************************************************************

!  get_next_integer() text

continue

! ----------------------------------------------------------------------

!  start at the symbol list head or continue from previous integer

   start_or_continue: if( associated( integer_ptr) )then

      symbol_ptr => integer_ptr% next

   else start_or_continue

      symbol_ptr => first_symbol

   end if start_or_continue

!  scan to the next integer

   find_next: do

      if( .not. associated( symbol_ptr) ) exit find_next

      check_next: select type( symbol_ptr)

      type is( integer_t) check_next

         integer_ptr => symbol_ptr

         return

      end select check_next

      symbol_ptr => symbol_ptr% next

   end do find_next

   nullify( integer_ptr)

! ----------------------------------------------------------------------

!  get_next_integer() exit

return

! **********************************************************************

!  get_next_integer()

end subroutine get_next_integer

! **********************************************************************
! **********************************************************************

!  get_next_logical() seek symbol on symbol list

subroutine get_next_logical( logical_ptr)

! **********************************************************************

!  get_next_logical() interface

! ----------------------------------------------------------------------

!  a pointer to the next logical on the symbol list

type( logical_t), pointer :: logical_ptr

! **********************************************************************

!  entry: symbol_str is blank_compress_lower_case logical symbol directive
!         "name..."

!  exit: symbol found or not in logical symbol array

! **********************************************************************

!  get_next_logical() local

   class( symbol_t), pointer :: symbol_ptr

! **********************************************************************

!  get_next_logical() text

continue

! ----------------------------------------------------------------------

!  start at the symbol list head or continue from previous logical

   start_or_continue: if( associated( logical_ptr) )then

      symbol_ptr => logical_ptr% next

   else start_or_continue

      symbol_ptr => first_symbol

   end if start_or_continue

!  scan to the next logical

   find_next: do

      if( .not. associated( symbol_ptr) ) exit find_next

      check_next: select type( symbol_ptr)

      type is( logical_t) check_next

         logical_ptr => symbol_ptr

         return

      end select check_next

      symbol_ptr => symbol_ptr% next

   end do find_next

   nullify( logical_ptr)

! ----------------------------------------------------------------------

!  get_next_logical() exit

return

! **********************************************************************

!  get_next_logical()

end subroutine get_next_logical

! **********************************************************************
! **********************************************************************

!  get_next_macro() seek symbol on symbol list

subroutine get_next_macro( macro_ptr)

! **********************************************************************

!  get_next_macro() interface

! ----------------------------------------------------------------------

!  a pointer to the next macro on the symbol list

type( macro_t), pointer :: macro_ptr

! **********************************************************************

!  entry: symbol_str is blank_compress_lower_case logical symbol directive
!         "name..."

!  exit: symbol found or not in logical symbol array

! **********************************************************************

!  get_next_macro() local

   class( symbol_t), pointer :: symbol_ptr

! **********************************************************************

!  get_next_macro() text

continue

! ----------------------------------------------------------------------

!  start at the symbol list head or continue from previous macro

   start_or_continue: if( associated( macro_ptr) )then

      symbol_ptr => macro_ptr% next

   else start_or_continue

      symbol_ptr => first_symbol

   end if start_or_continue

!  scan to the next macro

   find_next: do

      if( .not. associated( symbol_ptr) ) exit find_next

      check_next: select type( symbol_ptr)

      type is( macro_t) check_next

         macro_ptr => symbol_ptr

         return

      end select check_next

      symbol_ptr => symbol_ptr% next

   end do find_next

   nullify( macro_ptr)

! ----------------------------------------------------------------------

!  get_next_macro() exit

return

! **********************************************************************

!  get_next_macro()

end subroutine get_next_macro

! **********************************************************************
! **********************************************************************

!  get_next_file() seek symbol on symbol list

subroutine get_next_file( file_ptr)

! **********************************************************************

!  get_next_file() interface

! ----------------------------------------------------------------------

!  a pointer to the next file on the symbol list

type( file_symbol_t), pointer :: file_ptr

! **********************************************************************

!  entry: symbol_str is blank_compress_lower_case symbol directive
!         "name..."

!  exit: symbol found or not in symbol array

! **********************************************************************

!  get_next_file() local

   class( symbol_t), pointer :: symbol_ptr

! **********************************************************************

!  get_next_file() text

continue

! ----------------------------------------------------------------------

!  start at the symbol list head or continue from previous file

   start_or_continue: if( associated( file_ptr) )then

      symbol_ptr => file_ptr% next

   else start_or_continue

      symbol_ptr => first_symbol

   end if start_or_continue

!  scan to the next macro

   find_next: do

      if( .not. associated( symbol_ptr) ) exit find_next

      check_next: select type( symbol_ptr)

      type is( file_symbol_t) check_next

         file_ptr => symbol_ptr

         return

      end select check_next

      symbol_ptr => symbol_ptr% next

   end do find_next

   nullify( file_ptr)

! ----------------------------------------------------------------------

!  get_next_file() exit

return

! **********************************************************************

!  get_next_file()

end subroutine get_next_file

! **********************************************************************
! ----------------------------------------------------------------------

!  symbols

! $Id: coco.f90,v 2.11 2013/04/21 16:23:20 dan Exp $
! **********************************************************************
! eof
end module symbols
