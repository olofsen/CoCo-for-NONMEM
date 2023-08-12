! bof
! **********************************************************************
! Fortran program coco

! **********************************************************************

! $Id: coco.f90,v 2.11 2013/04/21 16:23:20 dan Exp $

! **********************************************************************
!  Copyright 2003-2013 Dan Nagle
!  All Rights Reserved

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

! To report bugs, suggest enhancements, or make other technical comments,
! to the Authors, Contact:
!                               Dan Nagle
!                               send email to dannagle@verizon.net
!                                  or mail to 731 Spruce St
!                                             Boulder CO 80302 USA

! **********************************************************************
! coco implements Part 3: Conditional Compilation

! **********************************************************************

!  coco compilation:

!  $ fc -std coco.f90 -o coco

!  where fc is your Fortran compiler
!  and -std is your compiler's option to check for standards conformance

!  coco has been compiled using

!  nagfor -f2008 -w=all -C=all coco.f90 -o coco
!  nagfor -V
!  NAG Fortran Compiler Release 5.3(854)
!  NAG Fortran Compiler Release 5.3.1(907)

!  coco has been compiled using

!  ifort -std -warn all -check all coco.f90 -o coco
!  ifort --version
!  ifort (IFORT) 12.1.0 20111011
!  ifort (IFORT) 12.1.2 20111207
!  ifort (IFORT) 12.1.3 20120130
!  ifort (IFORT) 13.0.1 20121010
!  ifort (IFORT) 13.0.2 20130314

!  coco has been compiled using

!  gfortran -std=f2008 -Wall -fcheck=all coco.f90 -o coco
!  gfortran --version
!  GNU Fortran (GCC) 4.6.2 20111019 (prerelease)
!  GNU Fortran (GCC) 4.7.2
!  GNU Fortran (GCC) 4.8.0 20130316 (prerelease)

!  Earlier compilers generally compiled earlier versions of CoCo.

!  No reason is known why other standards-complaint compilers would not work.

! **********************************************************************

!  coco reads

!     input source file(s)- named via command line or set file, or stdin
!     named set file- name taken from the output file name, or coco.set
!     freeze file- named on the command line or in the set file

!  coco writes

!     output source file- named via command line or set file, or stdout
!     alternate source files- named via open directives
!     freeze file- named on a source file freeze directive
!     log file- named via ??logfile directive, or stderr

! **********************************************************************

!  coco

! **********************************************************************

program coco

!  coco implements ISO/IEC 1539-3 Conditional Compilation with extensions

!  coco steps

!  0. call initialize_coco() to nullify most pointers and set others
!  1. call process_command_line() to read command line, get file names & options
!  2. call process_set_file() to read the set file, if there is one
!  3. open the output file, if named, use stdout if not
!  4. open the input file(s), if named, use stdin if not
!  5. call process_input_file() to process the input file(s) & write the output file
!  6. copy the set file contents to the output file
!  7. close all files
!  8. call write_report() to print summary information

! **********************************************************************

!  coco uses modules

! **********************************************************************

!  get preconnected units from intrinsic environment module
!  if your compiler doesn't support the intrinsic module,
!  define these below before the other units
!  a popular choice is input_unit= 5, output_unit= 6, error_unit= 0

use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, error_unit

!  if your compiler does not support f03 command line processing (most do)
!  you might use f2kcli available from http://www.winteracter.com/f2kcli

!  coco modules

use :: constants

use :: char_utilities

use :: expressions

use :: getopt_m

use :: values

use :: switches

use :: targets

use :: files

use :: symbols

! **********************************************************************

!  explicit declaration

implicit none

! **********************************************************************

!  coco RCS strings

! **********************************************************************

!  program source file name and version supplied by RCS

character( len= *), parameter :: coco_rcs_id = &
   '$Id: coco.f90,v 2.11 for NM-TRAN 2023/08/09 14:00:00 dan/erik Exp $'
!   '$Id: coco.f90,v 2.11 for 2013/04/21 16:23:20 dan Exp $'

! **********************************************************************

!  coco constants

! **********************************************************************

! **********************************************************************

!  coco character lengths

! **********************************************************************

! ----------------------------------------------------------------------

!  mark beginning of the set file in the output

character( len= *), parameter :: mark_set_file = '?? This was produced using the following SET file'

!  mark beginning of the next input file in the output

character( len= *), parameter :: mark_prev_file = '?? End of file '

character( len= *), parameter :: mark_next_file = ' - start of file '

! **********************************************************************

!  coco directives constants

! **********************************************************************

!  many character string constants' lengths are used to count past
!  the string as coco processes each coco statement

!  coco line and statement syntax uses the next set of character constants

! **********************************************************************

! ----------------------------------------------------------------------

!  characters defining coco directives, comments, separators, and so on

! ----------------------------------------------------------------------

!  coco line key ??coco_directive

character( len= *), parameter :: coco_key = '??'

!  continuation character

character( len= *), parameter :: continuation = '&'

!  ascii tab character

character( len= *), parameter :: tab = achar( 9)

!  whitespace is blank or tab

character( len= *), parameter :: white_space = blank // tab

!  coco comment initializer - changed for NONMEM!

character( len= *), parameter :: comment = ';'

!  separates items within a list

character( len= *), parameter :: comma = ','

! **********************************************************************

!  process_logical_declaration() constants

! ----------------------------------------------------------------------

!  process name[=value][,name[=value]]...

character( len= *), parameter :: end_of_decl = comma // blank

! ----------------------------------------------------------------------

!  directives which must appear in the set file

! ----------------------------------------------------------------------

!  alter directive

character( len= *), parameter :: alter_str = 'alter:'

!  directory declaration

character( len= *), parameter :: directory_str = 'directory'

!  form directive allows setting free form or fixed form from the set file

character( len= *), parameter :: form_str = 'form:'

!  log file declaration

character( len= *), parameter :: logfile_str = 'logfile'

!  mark directive controls placing message in output for each new input read after the first

character( len= *), parameter :: mark_str = 'mark:'

!  number directive controls placing "! file: line" strings on source lines

character( len= *), parameter :: number_str = 'number:'

!  post directive controls postpending the set_file at the end of the output

character( len= *), parameter :: post_str = 'post:'

!  report directive causes writing a report report after processing

character( len= *), parameter :: summary_str = 'report:'

!  verbose directive causes operation with more messages issued

character( len= *), parameter :: verbose_str = 'verbose:'

!  warning directive controls warning messages

character( len= *), parameter :: warning_str = 'warning:'

!  wrap directive controls line wrapping

character( len= *), parameter :: wrap_str = 'wrap:'

! ----------------------------------------------------------------------

!  name input files in set file directive

character( len= *), parameter :: input_str = 'input'

! ----------------------------------------------------------------------

!  directives which may appear in the set file or source file

! ----------------------------------------------------------------------

!  name the output file in set file directive, or direct output to the named file or the default file

character( len= *), parameter :: output_str = 'output'

!  freeze directive stops declarations, or reads or writes a freeze file

character( len= *), parameter :: freeze_str = 'freeze'

!  integer declaration

character( len= *), parameter :: integer_str = 'integer::'

!  integer constant declaration

character( len= *), parameter :: integer_constant_str = 'integer,parameter::'

!  logical declaration

character( len= *), parameter :: logical_str = 'logical::'

!  logical constant declaration

character( len= *), parameter :: logical_constant_str = 'logical,parameter::'

! ----------------------------------------------------------------------

!  directives which must appear in the source file

! ----------------------------------------------------------------------

!  include directive

character( len= *), parameter :: include_str = 'include'

! ----------------------------------------------------------------------

!  stop directive

character( len= *), parameter :: stop_str = 'stop'

! ----------------------------------------------------------------------

!  message directive

character( len= *), parameter :: message_str = 'message'

! ----------------------------------------------------------------------

!  if directive

character( len= *), parameter :: if_str = 'if('

! ----------------------------------------------------------------------

!  else if directive

character( len= *), parameter :: elseif_str = 'elseif('

! ----------------------------------------------------------------------

!  )then must close an if( or elseif(

character( len= *), parameter :: then_str = ')then'

! ----------------------------------------------------------------------

!  else directive

character( len= *), parameter :: else_str = 'else'

! ----------------------------------------------------------------------

!  endif directive

character( len= *), parameter :: endif_str = 'endif'

! ----------------------------------------------------------------------

!  directives which are extensions

! ----------------------------------------------------------------------

!  macro declaration

character( len= *), parameter :: macro_str = 'macro::'

!  macro declaration

character( len= *), parameter :: macro_parens_str = 'macro,parens::'

!  macro declaration from environment

character( len= *), parameter :: getenv_str = 'getenv::'

! ----------------------------------------------------------------------

!  assert directive (the open paren is treated as part of the assert expression)

character( len= *), parameter :: assert_str = 'assert'

! ----------------------------------------------------------------------

!  cmdline directive

character( len= *), parameter :: cmdline_str = 'cmdline'

!  document directive

character( len= *), parameter :: document_str = 'document'

!  options directive

character( len= *), parameter :: options_str = 'options'

!  repport directive

character( len= *), parameter :: report_str = 'report'

!  symbols directive

character( len= *), parameter :: symbols_str = 'symbols'

! ----------------------------------------------------------------------

!  text directive (the end text string is in process_text_directive() )

character( len= *), parameter :: text_str = 'text::'

!  text directive

character( len= *), parameter :: text_parens_str = 'text,parens::'

!  copy directive

character( len= *), parameter :: copy_str = 'copy::'

! ----------------------------------------------------------------------

!  endfile directive

character( len= *), parameter :: endfile_str = 'endfile'

!  open directive

character( len= *), parameter :: open_str = 'open::'

! ----------------------------------------------------------------------

!  these strings are parts of directives

! ----------------------------------------------------------------------

!  on directive

character( len= *), parameter :: on_str = 'on'

!  off directive

character( len= *), parameter :: off_str = 'off'

!  form free directive

character( len= *), parameter :: free_str = 'free'

!  form fixed directive

character( len= *), parameter :: fixed_str = 'fixed'

! **********************************************************************

!  possible states encountered during execution

! ----------------------------------------------------------------------

!  codes for possible alter states

integer, parameter :: alter_none = 0

integer, parameter :: alter_delete = 1

integer, parameter :: alter_blank = 2

integer, parameter :: alter_shift_0 = 3

integer, parameter :: alter_shift_1 = 4

integer, parameter :: alter_shift_3 = 5

! ----------------------------------------------------------------------

!  codes for possible if construct phases

integer, parameter :: outside_block = 0

integer, parameter :: if_block = 1

integer, parameter :: elseif_block = 2

integer, parameter :: else_block = 3

integer, parameter :: text_block = 4

integer, parameter :: include_block = 5

integer, parameter :: output_block = 6

! **********************************************************************

!  coco usage (error message)

character( len= *), parameter :: usage_msg = &
      'usage: coco [ -V  | -h | [[ opts ] [--] [ basename | output input [...]]]'

! **********************************************************************

!  string constants

! ----------------------------------------------------------------------

!  suffix used to construct output file name if one name is on the command line

!  changed for NONMEM!

character( len= *), parameter :: output_suffix = '.ctl'

!  suffix when fixed form processing is requested

!  changed for NONMEM!

character( len= *), parameter :: fixed_suffix = '.ctl'

! **********************************************************************

! **********************************************************************

!  coco types

! **********************************************************************

!  search location type

type :: path_t

   character( len= file_name_len) :: name_str

   integer :: times_accessed

   type( path_t), pointer :: next

end type path_t

! **********************************************************************

!  if_t stores the state of an if block

! ----------------------------------------------------------------------

!  if_t

type :: if_t

   logical :: now_selected

   logical :: ever_selected

   integer :: phase

   type( if_t), pointer :: nested

   type( if_t), pointer :: enclosing

end type if_t

! **********************************************************************

! **********************************************************************

!  report_t stores coco statistics

! ----------------------------------------------------------------------

!  report_t records the source and sink of lines

type :: report_t

   integer :: input_lines

   integer :: input_files

   integer :: include_files

   integer :: coco_lines

   integer :: selected_lines

   integer :: elided_lines

   integer :: text_blocks

   integer :: text_lines

   integer :: copied_lines

end type report_t

! **********************************************************************

!  coco variables

! **********************************************************************

!  report totals of event counts

type( report_t) :: total

! ----------------------------------------------------------------------

!  if construct outside any if construct

type( if_t), target :: outside_any_if_construct

!  if construct status

type( if_t), pointer :: if_construct

! ----------------------------------------------------------------------

!  mark when non constants are used to provide a value for a constant

logical :: all_constants

! **********************************************************************

!  coco file name variables

! ----------------------------------------------------------------------

!  a list of source file names for reports

type( input_file_t), allocatable, dimension( :) :: source_file_list

!  scratch file to count source names found in the set file

type( scratch_file_t) :: input_name_file

! ----------------------------------------------------------------------

!  some directive processing peeks at the as-is statement being decoded

character( len= buffer_len) :: asis_stmt

!  length of the as is statement

integer :: asis_len

! ----------------------------------------------------------------------

!  list of include directories is initially . only

type( path_t), pointer :: first_directory

integer :: count_include_in_dot

! **********************************************************************

!  coco local

! ----------------------------------------------------------------------

!  loop index of file name args

   integer :: i

! **********************************************************************

!  coco text

! **********************************************************************

continue

! ----------------------------------------------------------------------

!  initialize coco program variables

   call initialize_coco()

! ----------------------------------------------------------------------

!  process command line to get options and file names

   call process_command_line()

! ----------------------------------------------------------------------

!  see if set file exists and process it if it does

   call seek_set_file()

! ----------------------------------------------------------------------

!  set option to default values if the command line or the set file hasn't set them

   call set_option_defaults()

! ----------------------------------------------------------------------

!  open the output file but link not current_file

   call output_file% open()

!  copy the freeze file to the output if one has been read

   read_freeze: if( options% freeze_declarations )then

      call copy_freeze_file()

   end if read_freeze

! **********************************************************************

!  read all input file(s)

   number_of_input_files: if( allocated( source_file_list) )then

!  process several input files

      read_all_files: do i = 1, size( source_file_list)

!  if marking subsequent input files

         marking_input: if( options% mark_input .and. i > 1 )then

            line = mark_prev_file // trim( source_file_list( i - 1)% name_str) &
                // mark_next_file // trim( source_file_list( i)% name_str)

            call write_coco_line( output_file)

         end if marking_input

!  process the input using coco default units

         call process_input_file( source_file_list( i))

!  repeat for each input file

      end do read_all_files

!  end of input

   else number_of_input_files

!  process the input file

      call process_input_file( input_file)

!  end of input

   end if number_of_input_files

! **********************************************************************

!  if the output file has content and the copy is wanted, copy the set file to it

   made_output: if( output_file% lines_transfered > 0 .and. options% postpend_set_file )then

!  mark the set file in the output (whether it is present or not)

      line = mark_set_file

      call write_coco_line( output_file)

! ----------------------------------------------------------------------

!  if processed a set file

      append_set_file: if( is_connected( set_file) )then

!  copy set file file to output

         call copy_set_file()

!  if processed set file

      end if append_set_file

   end if made_output

! ----------------------------------------------------------------------

!  close the output file

   call output_file% close()

! ----------------------------------------------------------------------

!  report to log file

   log_report: if( options% print_report )then

      call write_report()

   end if log_report

! ----------------------------------------------------------------------

!  close the log file

   call log_file% close()

! ----------------------------------------------------------------------

!  coco exit

stop 'coco normal exit'

! **********************************************************************

!  coco library

! **********************************************************************

contains

! **********************************************************************
! **********************************************************************

!  initialize_coco() prepares coco for execution

subroutine initialize_coco()

! **********************************************************************

!  initialize_coco() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  always want a log file of some sort

   log_file% io_unit = error_unit

! ----------------------------------------------------------------------

!  initialize pointers

! ----------------------------------------------------------------------

!  magic if-block outside any if-block

   outside_any_if_construct = if_t( now_selected= .true., ever_selected= .true., phase= outside_block, &
                                    nested= null(), enclosing= null() )

!  initialize if-block structures

   nullify( if_construct)

   if_construct => outside_any_if_construct

! ----------------------------------------------------------------------

!  set files and lines

   nullify( current_file)

!  initialize include file search path

   nullify( first_directory)

   count_include_in_dot = 0

! ----------------------------------------------------------------------

!  initialize predefined macro names

   predefined_macros% referenced = .false.
   predefined_macros% referenced_file = blank
   predefined_macros% referenced_line = 0

   predefined_macros( file_ss)% name_str = 'file'

   predefined_macros( line_ss)% name_str = 'line'

   predefined_macros( date_ss)% name_str = 'date'

   predefined_macros( time_ss)% name_str = 'time'

   predefined_macros( coco_ss)% name_str = 'coco'

   predefined_macros( setfile_ss)% name_str = 'setfile'

   predefined_macros( logfile_ss)% name_str = 'logfile'

   predefined_macros( output_ss)% name_str = 'output'

   predefined_macros( cmdline_ss)% name_str = 'cmdline'

   predefined_macros( user_ss)% name_str = 'user'

   predefined_macros( cwd_ss)% name_str = 'cwd'

   predefined_macros( incpath_ss)% name_str = 'incpath'

   predefined_macros( freeze_ss)% name_str = 'freeze'

   predefined_macros( null_ss)% name_str = 'null'

   predefined_macros( blank_ss)% name_str = 'blank'

!  experimental extension

   predefined_macros( eval_ss)% name_str = 'eval'

! ----------------------------------------------------------------------

!  initialize_coco() exit

return

! **********************************************************************

!  initialize_coco()

end subroutine initialize_coco

! **********************************************************************
! **********************************************************************

!  %%% write log file messages, parse command line

! **********************************************************************
! **********************************************************************

!  set_option_defaults() set options to their default values

subroutine set_option_defaults()

! **********************************************************************

!  Some options are initially set to absurd values in order to allow
!  the command line option to override the corresponding set file directive.
!  These options need to be set to useful values after the set file
!  has been initially read but before coco further executes.

!  These options are: the alter mode and the wrap length.

!  The options selected are also made mutually consistent.

! **********************************************************************

!  set_option_defaults() local

! ----------------------------------------------------------------------

!  index of output file name suffix

   integer :: suffix_idx

   integer :: name_len

!  strings to be edited into the line- they are the exact length needed

   character( len= 8) :: today_str

   character( len= 10) :: now_str

! **********************************************************************

!  set_option_defaults() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  if input file names didn't come from the command line, get them from the set file

   input_directives: if( is_connected( input_name_file) )then

      call input_name_file% rewind()

      count_names: select case( input_name_file% lines_transfered)

      case( 1) count_names

         call input_name_file% read( input_file% name_str)

      case( 2: ) count_names

         allocate( source_file_list( input_name_file% lines_transfered), stat= state% status, errmsg= state% message)

         alloc_error: if( is_status_error( state) )then

            call msg_quit( "can't allocate input file list")

         end if alloc_error

         name_each: do i = 1, input_name_file% lines_transfered

            call input_name_file% read( source_file_list( i)% name_str)

         end do name_each

      end select count_names

      call input_name_file% close()

   end if input_directives

! ----------------------------------------------------------------------

!  if the command line or the set file hasn't set the alter state, set it to the default

   cl_alter_set: if( cl_options% alter_state /= alter_none )then

      options% alter_state = cl_options% alter_state

   end if cl_alter_set

   alter_default: if( options% alter_state == alter_none )then

      options% alter_state = alter_shift_3

   end if alter_default

! ----------------------------------------------------------------------

!  free form unless either the command line or the set file requests fixed form

   options% free_form = options% free_form .and. cl_options% free_form

!  set wrapping length and reset output file name if needed

   wrap_default: if( options% free_form )then

      options% wrap_len = free_form_len

   else wrap_default

      options% wrap_len = fixed_form_len

      name_len = len_trim( output_file% name_str)

      name_from_basename: if( name_len > 0 )then

         suffix_idx = index( output_file% name_str( 1: name_len), output_suffix, back= .true.)

         if( suffix_idx == 0 ) suffix_idx = name_len + 1

         output_file% name_str( suffix_idx: ) = fixed_suffix

      end if name_from_basename

   end if wrap_default

! ----------------------------------------------------------------------

!  set the rest of the options from the command line

   options% mark_input = options% freeze_declarations .or. cl_options% freeze_declarations
   options% mark_input = options% mark_input .or. cl_options% mark_input
   options% number_source = options% number_source .or. cl_options% number_source
   options% postpend_set_file = options% postpend_set_file .and. cl_options% postpend_set_file
   options% print_report = options% print_report .or. cl_options% print_report
   options% verbose_mode = options% verbose_mode .or. cl_options% verbose_mode

!  verbose mode makes a summary report as well

   options% print_report = options% print_report .or. options% verbose_mode

! ----------------------------------------------------------------------

!  the command line can only disable line wrapping

   options% wrapping_lines = options% wrapping_lines .and. cl_options% wrapping_lines

! ----------------------------------------------------------------------

!  set values in the prdefined macros

   call date_and_time( date= today_str, time= now_str)

   predefined_macros( date_ss)% macro_value = format_date( today_str)

   predefined_macros( time_ss)% macro_value = format_time( now_str)

   predefined_macros( coco_ss)% macro_value = coco_rcs_id

   predefined_macros( setfile_ss)% macro_value = set_file% name_str

   predefined_macros( logfile_ss)% macro_value = log_file% name_str

   predefined_macros( output_ss)% macro_value = output_file% name_str

   predefined_macros( freeze_ss)% macro_value = options% freeze_file_name

   predefined_macros( null_ss)% macro_value = null_string

   predefined_macros( blank_ss)% macro_value = set_value( blank)

!  set value from the coco command line

   call get_command_line_value( predefined_macros( cmdline_ss)% macro_value)

!  build incpath value from the include directory list

   call build_incpath_value( predefined_macros( incpath_ss)% macro_value)

! added by erik, not sure why these were missing, but useful for ?? DOCUMENT

   call get_user_environment_value( predefined_macros( user_ss)% macro_value)

   call get_pwd_environment_value( predefined_macros( cwd_ss)% macro_value)

! ----------------------------------------------------------------------

!  try to find the OS file name separator character

   call seek_sep_char( predefined_macros( cwd_ss)% macro_value)

   have_sep_char: if( options% got_sep_char )then

      call check_incpath_dirs()

   end if have_sep_char

! ----------------------------------------------------------------------

!  ensure a log file is open

   have_log_file: if( .not. is_connected( log_file) )then

      call log_file% open()

   end if have_log_file

! ----------------------------------------------------------------------

!  set_option_defaults() exit

return

! **********************************************************************

!  set_option_defaults()

end subroutine set_option_defaults

! **********************************************************************
! **********************************************************************

!  get_command_line_value() get command line string

subroutine get_command_line_value( macro_value)

! **********************************************************************

!  get_command_line_value() interface

! ----------------------------------------------------------------------

!  the warning or informational message

type( value_t), intent( out) :: macro_value

! **********************************************************************

!  get_command_line_value() local

! ----------------------------------------------------------------------

!  test command line status and length

   integer :: cmdline_len

   integer :: cl_stat

   character( len= buffer_len) :: value_buffer

! **********************************************************************

!  get_command_line_value() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

   call get_command( length= cmdline_len, status= cl_stat)

   cl_status_error: if( cl_stat > 0 )then

      call msg_quit( "can't access the command line")

   end if cl_status_error

   command_line_too_long: if( cmdline_len > len( value_buffer) )then

      call msg_quit( "command line too long for macro value")

   end if command_line_too_long

   call get_command( command= value_buffer)

   macro_value = value_buffer

! ----------------------------------------------------------------------

!  get_command_line_value() exit

return

! **********************************************************************

!  get_command_line_value()

end subroutine get_command_line_value

! **********************************************************************
! **********************************************************************

!  get_user_environment_value() get command line string

subroutine get_user_environment_value( macro_value)

! **********************************************************************

!  get_user_environment_value() interface

! ----------------------------------------------------------------------

!  the warning or informational message

type( value_t), intent( out) :: macro_value

! **********************************************************************

!  get_user_environment_value() constants

! ----------------------------------------------------------------------

!  name of environment variable to fetch

character( len= *), parameter :: env_var_name = 'USER'

character( len= *), parameter :: alt_env_var_name = 'LOGNAME'

! **********************************************************************

!  get_user_environment_value() local

! ----------------------------------------------------------------------

!  test command line status and length

   integer :: env_stat

   character( len= buffer_len) :: value_buffer

! **********************************************************************

!  get_user_environment_value() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  try to get USER

   call get_environment_variable( name= env_var_name, status= env_stat)

   got_user: if( env_stat == 0 )then

      call get_environment_variable( name= env_var_name, value= value_buffer)

      macro_value = value_buffer

      return

   else got_user

      env_status_error: if( env_stat > 0 )then

         call msg_continue( "can't access environment variable: " // env_var_name)

      end if env_status_error

      env_var_too_long: if( env_stat < 0 )then

         call msg_quit( "environment variable value too long for macro value: " // env_var_name)

      end if env_var_too_long

   end if got_user

!  if no USER, try to get LOGNAME

   call get_environment_variable( name= alt_env_var_name, status= env_stat)

   got_logname: if( env_stat == 0 )then

      call get_environment_variable( name= alt_env_var_name, value= value_buffer)

      macro_value = value_buffer

   else got_logname

      alt_env_status_error: if( env_stat > 0 )then

         call msg_continue( "can't access environment variable: " // alt_env_var_name)

      end if alt_env_status_error

      alt_env_var_too_long: if( env_stat < 0 )then

         call msg_quit( "environment variable value too long for macro value: " // alt_env_var_name)

      end if alt_env_var_too_long

   end if got_logname

! ----------------------------------------------------------------------

!  get_user_environment_value() exit

return

! **********************************************************************

!  get_user_environment_value()

end subroutine get_user_environment_value

! **********************************************************************
! **********************************************************************

!  get_pwd_environment_value() get command line string

subroutine get_pwd_environment_value( macro_value)

! **********************************************************************

!  get_pwd_environment_value() interface

! ----------------------------------------------------------------------

!  the warning or informational message

type( value_t), intent( out) :: macro_value

! **********************************************************************

!  get_pwd_environment_value() constants

! ----------------------------------------------------------------------

!  name of environment variable to fetch

character( len= *), parameter :: env_var_name = 'PWD'

! **********************************************************************

!  get_pwd_environment_value() local

! ----------------------------------------------------------------------

!  test command line status and length

   integer :: env_stat

   character( len= buffer_len) :: value_buffer

! **********************************************************************

!  get_pwd_environment_value() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  try to get PWD

   call get_environment_variable( name= env_var_name, status= env_stat)

   env_status_error: if( env_stat > 0 )then

      call msg_continue( "can't access environment variable: " // env_var_name)

   end if env_status_error

   env_var_too_long: if( env_stat < 0 )then

      call msg_quit( "environment variable value too long for macro value: " // env_var_name)

   end if env_var_too_long

   call get_environment_variable( name= env_var_name, value= value_buffer)

   macro_value = value_buffer

! ----------------------------------------------------------------------

!  get_pwd_environment_value() exit

return

! **********************************************************************

!  get_pwd_environment_value()

end subroutine get_pwd_environment_value

! **********************************************************************
! **********************************************************************

!  build_incpath_value() get command line string

subroutine build_incpath_value( macro_value)

! **********************************************************************

!  build_incpath_value() interface

! ----------------------------------------------------------------------

!  the warning or informational message

type( value_t), intent( out) :: macro_value

! **********************************************************************

!  build_incpath_value() constants

! ----------------------------------------------------------------------

character( len= *), parameter :: default_path = dot

! **********************************************************************

!  build_incpath_value() local

! ----------------------------------------------------------------------

   type( path_t), pointer :: path_ptr

   character( len= buffer_len) :: path_buffer

! **********************************************************************

!  build_incpath_value() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

   path_buffer = default_path

   nullify( path_ptr)

   path_ptr => first_directory

   scan_path: do

      if( .not. associated( path_ptr) ) exit scan_path

      path_buffer = trim( path_buffer) // blank // path_ptr% name_str

      path_ptr => path_ptr% next

   end do scan_path

   macro_value = path_buffer

! ----------------------------------------------------------------------

!  build_incpath_value() exit

return

! **********************************************************************

!  build_incpath_value()

end subroutine build_incpath_value

! **********************************************************************
! **********************************************************************

!  seek_sep_char() try to find the OS file name separator character

subroutine seek_sep_char( pwd)

! **********************************************************************

!  seek_sep_char() interface

! ----------------------------------------------------------------------

!  the warning or informational message

type( value_t), intent( in) :: pwd

! **********************************************************************

!  seek_sep_char() constants

character( len= *), parameter :: slash = '/'
character( len= *), parameter :: backslash = '\'

! ----------------------------------------------------------------------

!  test command line status and length

! **********************************************************************

!  seek_sep_char() local

! ----------------------------------------------------------------------

!  test command line status and length

   integer :: char_idx

   character( len= buffer_len) :: pwd_buffer

! **********************************************************************

!  seek_sep_char() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

   pwd_buffer = pwd

!  first seek slash in pwd

   char_idx = index( string= pwd_buffer, substring= slash)

   got_slash: if( char_idx > 0 )then

      options% sep_char = slash
      options% got_sep_char = .true.

      return

   end if got_slash

!  next seek backslash in pwd

   char_idx = index( string= pwd_buffer, substring= backslash)

   got_backslash: if( char_idx > 0 )then

      options% sep_char = backslash
      options% got_sep_char = .true.

      return

   end if got_backslash

!  next seek colon in pwd

   char_idx = index( string= pwd_buffer, substring= colon)

   got_colon: if( char_idx > 0 )then

      options% sep_char = colon
      options% got_sep_char = .true.

      return

   end if got_colon

! ----------------------------------------------------------------------

!  seek_sep_char() exit

return

! **********************************************************************

!  seek_sep_char()

end subroutine seek_sep_char

! **********************************************************************
! **********************************************************************

!  check_incpath_dirs() get command line string

subroutine check_incpath_dirs()

! **********************************************************************

!  check_incpath_dirs() interface

! ----------------------------------------------------------------------

! **********************************************************************

!  check_incpath_dirs() local

! ----------------------------------------------------------------------

   type( path_t), pointer :: path_ptr

   integer :: dir_lt

! **********************************************************************

!  check_incpath_dirs() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

   nullify( path_ptr)

   path_ptr => first_directory

   scan_path: do

      if( .not. associated( path_ptr) ) exit scan_path

      dir_lt = len_trim( path_ptr% name_str)

      fix_missing_sep: if( path_ptr% name_str( dir_lt: dir_lt) /= options% sep_char )then

         path_ptr% name_str( dir_lt + 1: dir_lt + 1) = options% sep_char

      end if fix_missing_sep

      path_ptr => path_ptr% next

   end do scan_path

! ----------------------------------------------------------------------

!  check_incpath_dirs() exit

return

! **********************************************************************

!  check_incpath_dirs()

end subroutine check_incpath_dirs

! **********************************************************************
! **********************************************************************

!  process_command_line() process command line

subroutine process_command_line()

! **********************************************************************

!  process_command_line calls getopt() to get any options, then
!  process_command_line gets file names from the command line

! **********************************************************************

!  default coco file names constants

! **********************************************************************

!  input file name constants

! ----------------------------------------------------------------------

!  suffix used to construct input file name if one name is on the command line

!  changed for NONMEM!

character( len= *), parameter :: input_suffix = dot // 'nmt'

! **********************************************************************

!  set file constants

! ----------------------------------------------------------------------

!  suffix used to construct set file name if name is on the command line

character( len= *), parameter :: set_suffix = dot // 'set'

! **********************************************************************

!  suffix length

integer, parameter :: suffix_len = &
         max( len( input_suffix), len( output_suffix), len( set_suffix) )

! **********************************************************************

!  other command line constants

! ----------------------------------------------------------------------

!  coco communicate with getopt()

! ----------------------------------------------------------------------

!  valid option letters

character( len= *), parameter :: opt_letters = 'a:D:f:FhI:l:mnprs:SU:vVwW '

! **********************************************************************

!  process_command_line local

! ----------------------------------------------------------------------

!  getopt() option letter

   integer :: optltr

!  input file names

   character( len= file_name_len) :: argword

!  dot divides basename and suffix

   integer :: basename_len

!  remaining words are file names

   integer :: words_left

!  loop through input file names

   integer :: i

! **********************************************************************

!  process_command_line() text

! ----------------------------------------------------------------------

continue

!  get number of command line args

   nargs = command_argument_count()

!  do until end of args is returned

   optltr = getopt( opt_letters)

!  process options

   cl_args: do

      if( optltr == end_of_args) exit cl_args

!  select which option

      which_option: select case( char( optltr))

! ----------------------------------------------------------------------

!  set the alter state

      case( 'a') which_option

         call process_alter_option( optarg)

!  declare a symbol

      case( 'D') which_option

         call process_define_option( optarg)

!  set freeze mode and read freeze file

      case( 'f') which_option

         call process_freeze_option( optarg)

!  set source form to fixed

      case( 'F') which_option

         call process_fixed_option()

!  help

      case( 'h') which_option

         call print_help()

         stop 'coco normal exit'

!  set directories to search for include files

      case( 'I') which_option

         call process_include_option( optarg)

!  set log file (NOTE: optarg has len= file_name_len, so no overflow can occur.)

      case( 'l') which_option

         call process_log_file_option( optarg)

!  turn on marking input files

      case( 'm') which_option

         call process_mark_option()

!  turn on line numbers

      case( 'n') which_option

         call process_number_option()

!  turn off copy of set_file

      case( 'p') which_option

         call process_postpend_option()

!  write summary report

      case( 'r') which_option

         call process_report_option()

!  name set file

      case( 's') which_option

         call process_set_file_option( optarg)

!  do not seek a set file

      case( 'S') which_option

         call process_no_set_file_option()

!  declare a symbol, but set it to false

      case( 'U') which_option

         call process_undefine_option( optarg)

!  turn on verbose

      case( 'v') which_option

         call process_verbose_option()

!  print coco version data

      case( 'V') which_option

         write( unit= error_unit, fmt= string_fmt) coco_rcs_id

         stop 'coco normal exit'

!  turn off line wrapping

      case( 'w') which_option

         call process_wrap_option()

!  turn on warnings

      case( 'W') which_option

         call process_warning_option()

!  command line error

      case default which_option

         write( unit= error_unit, fmt= string_fmt) usage_msg

         stop 'coco error exit'

      end select which_option

! ----------------------------------------------------------------------

      optltr = getopt( opt_letters)

   end do cl_args

! ----------------------------------------------------------------------

!  the rest of the command line words (if any) must be file names

! ----------------------------------------------------------------------

!  number of command line args left unprocessed

   args_left: if( optarg == unknown_option )then

!  found a word not --

      words_left = nargs - optind

      optind = optind + 1

!  refetch it so check its length

      no_more_args: if( words_left > 0 )then

         call get_cl_arg_check_len( optind, optarg)

      end if no_more_args

   else args_left

!  found --

      words_left = nargs - optind + 1

   end if args_left

! ----------------------------------------------------------------------

!  process file names

   file_names: select case( words_left)

! ----------------------------------------------------------------------

!  one file name arg

   case( 1) file_names

!  check that basename is not too long

      base_too_long: if( ( len_trim( optarg) + suffix_len) > file_name_len )then

         call msg_quit( 'file name too long: ' // trim( optarg) )

      end if base_too_long

!  use basename to make input file name

      input_file% name_str = trim( optarg) // input_suffix

!  use basename to make output file name

      output_file% name_str = trim( optarg) // output_suffix

!  use basename to make set file file name

      base_name = trim( optarg) // set_suffix

! ----------------------------------------------------------------------

!  more than one file name arg

   case( 2: ) file_names

!  the first name is the output file name

      output_file% name_str = optarg

      words_left = words_left - 1

!  allocate source file list

      allocate( source_file_list( 1: words_left), stat= state% status, errmsg= state% message)

      alloc_error: if( is_status_error( state) )then

         call msg_quit( "can't allocate input file array")

      end if alloc_error

!  compute set file name

      basename_len = index( output_file% name_str, dot, back= .true.)

      no_dot: if( basename_len == 0 )then

         basename_len = len_trim( output_file% name_str) + len( dot)

      end if no_dot

!  check that set file name is not too long

      set_too_long: if( basename_len + suffix_len > file_name_len )then

         call msg_quit( 'set file name too long: ' // trim( output_file% name_str) // set_suffix )

      end if set_too_long

!  use output file name to make set file file name

      base_name = output_file% name_str( 1: basename_len - len( dot)) // set_suffix

!  record input files in source file list

      list_inputs: do i = 1, words_left

!  get next arg string

         optind = optind + 1

         call get_cl_arg_check_len( optind, argword)

         source_file_list( i)% name_str = argword

      end do list_inputs

!  only possible values

   end select file_names

! ----------------------------------------------------------------------

!  process_command_line() exit

return

! **********************************************************************

!  process_command_line()

end subroutine process_command_line

! **********************************************************************
! **********************************************************************

!  %%% process particular command line options

! **********************************************************************
! **********************************************************************

!  process_alter_option() process alter arguments

subroutine process_alter_option( alter_opt)

! **********************************************************************

!  process_alter_option() interface

! ----------------------------------------------------------------------

!  the alter option from the command line

character( len= *), intent( in) :: alter_opt

! **********************************************************************

!  entry: alter_opt is command line arg following -a
!         "d" | "b" | "0" | "1" | "3"

!  exit: alter_opt is processed or error exit

! **********************************************************************

!  process_alter_option() constants

! ----------------------------------------------------------------------

!  possible alter option strings

character( len= *), parameter :: delete_str = 'd'

character( len= *), parameter :: blank_str = 'b'

character( len= *), parameter :: shift0_str = '0'

character( len= *), parameter :: shift1_str = '1'

character( len= *), parameter :: shift3_str = '3'

! **********************************************************************

!  process_alter_option() local

! ----------------------------------------------------------------------

!  decoding the option is done in lower case which may require a case change

   character( len= 1) :: lower_case_opt

! ----------------------------------------------------------------------

!  allow only one -a per command line

   logical, save :: too_many_alter_options = .false.

! **********************************************************************

!  process_alter_option() text

continue

! ----------------------------------------------------------------------

!  execute only once

   too_many: if( too_many_alter_options )then

      call msg_quit( "too many alter options on command line")

   else too_many

      too_many_alter_options = .true.

   end if too_many

! ----------------------------------------------------------------------

!  check for unknown option

   too_long: if( len_trim( alter_opt) > 1 )then

      call msg_quit( "garbled -a option: " // trim( alter_opt) )

   end if too_long

!  force arg to lower case

   lower_case_opt = to_lower( alter_opt( 1: 1))

! ----------------------------------------------------------------------

!  legal alter argument or error

   alter_value_str: select case( lower_case_opt)

!  alter delete

   case( delete_str) alter_value_str

      cl_options% alter_state = alter_delete

!  alter blank

   case( blank_str) alter_value_str

      cl_options% alter_state = alter_blank

!  alter shift1

   case( shift1_str) alter_value_str

      cl_options% alter_state = alter_shift_1

!  alter shift0

   case( shift0_str) alter_value_str

      cl_options% alter_state = alter_shift_0

!  alter shift3

   case( shift3_str) alter_value_str

      cl_options% alter_state = alter_shift_3

!  unknown alter code ( not one of { b, d, 0, 1, 3 } )

   case default alter_value_str

      call msg_quit( "unknown -a option: " // trim( alter_opt) )

!  legal alter statement or error

   end select alter_value_str

! ----------------------------------------------------------------------

!  process_alter_option() exit

return

! **********************************************************************

!  process_alter_option()

end subroutine process_alter_option

! **********************************************************************
! **********************************************************************

!  process_define_option() process define arguments -Dname[=n]

subroutine process_define_option( symbol_opt)

! **********************************************************************

!  process_define_option() interface

! ----------------------------------------------------------------------

!  the symbol string from the command line

character( len= *), intent( in) :: symbol_opt

! **********************************************************************

!  entry: symbol_opt is string following -D { log | int=val }

!  exit: symbol_opt is processed or error exit

! **********************************************************************

!  process_define_option() local

! ----------------------------------------------------------------------

!  find characters

   integer :: eq_idx

!  construct a declaration string to process

   character( len= file_name_len) :: decl_string

! **********************************************************************

!  process_define_option() text

continue

! ----------------------------------------------------------------------

!  force names to lower case

   decl_string = to_lower( symbol_opt)

! ----------------------------------------------------------------------

!  an equal sign must separate a value from the name

   eq_idx = index( decl_string, equals)

!  if there's an equals, it's an integer

   int_or_log: if( eq_idx > 0 )then

!  check name

      call valid_new_cl_name( decl_string( 1: eq_idx - 1))

!  declare the integer constant

      call add_cl_integer( decl_string( eq_idx: ), decl_string( 1: eq_idx - 1))

!  if there's no equals, it's a logical ( = .true.)

   else int_or_log

!  check name

      call valid_new_cl_name( trim( decl_string))

!  declare the logical constant

      call add_cl_logical( decl_string, .true.)

!  integer or logical

   end if int_or_log

! ----------------------------------------------------------------------

!  process_define_option() exit

return

! **********************************************************************

!  process_define_option()

end subroutine process_define_option

! **********************************************************************
! **********************************************************************

!  process_undefine_option() process define arguments -Uname[=n]

subroutine process_undefine_option( symbol_opt)

! **********************************************************************

!  process_undefine_option() interface

! ----------------------------------------------------------------------

!  the symbol string from the command line

character( len= *), intent( in) :: symbol_opt

! **********************************************************************

!  entry: symbol_opt is string following -D { log | int=val }

!  exit: symbol_opt is processed or error exit

! **********************************************************************

!  process_undefine_option() local

! ----------------------------------------------------------------------

!  find characters

   integer :: eq_idx

!  construct a declaration string to process

   character( len= file_name_len) :: decl_string

! **********************************************************************

!  process_undefine_option() text

continue

! ----------------------------------------------------------------------

!  force names to lower case

   decl_string = to_lower( symbol_opt)

! ----------------------------------------------------------------------

!  an equal sign must separate a value from the name

   eq_idx = index( decl_string, equals)

!  if there's an equals, it's an integer, but that's not allowed with '-U'

   int_or_log: if( eq_idx > 0 )then

      write( unit= error_unit, fmt= string_fmt) usage_msg

      stop 'coco error exit'

!  if there's no equals, it's a logical ( = .true.)

   else int_or_log

!  check name

      call valid_new_cl_name( trim( decl_string))

!  declare the logical constant

      call add_cl_logical( decl_string, .false.)

!  integer or logical

   end if int_or_log

! ----------------------------------------------------------------------

!  process_undefine_option() exit

return

! **********************************************************************

!  process_undefine_option()

end subroutine process_undefine_option

! **********************************************************************
! **********************************************************************

!  add_cl_integer() store integer from the command line in cl symbol table

subroutine add_cl_integer( int_decl_str, integer_name)

! **********************************************************************

!  add_cl_integer() interface

! ----------------------------------------------------------------------

!  the statement containing the declaration

character( len= *), intent( in) :: int_decl_str

!  the symbol name

character( len= *), intent( in) :: integer_name

! **********************************************************************

!  entry: int_decl_str is blank_compress_lower_case integer declaration statement past the name
!         "" | "=..."
!         sym_name is the symbol name

!  exit: integer declaration is added to the integer symbol list or error exit

! **********************************************************************

!  add_cl_integer() local

! ----------------------------------------------------------------------

!  type pointer to allocate

   type( integer_t), pointer :: integer_ptr

! **********************************************************************

!  add_cl_integer() text

continue

! ----------------------------------------------------------------------

!  allocate new integer

   allocate( integer_ptr, stat= state% status, errmsg= state% message)

   alloc_error: if( is_status_error( state) )then

      call msg_quit( "can't allocate -D integer: " // trim( integer_name))

   end if alloc_error

!  build new integer on list

   subsequent_or_first: if( associated( first_cl_symbol) )then

      last_cl_symbol% next => integer_ptr

      last_cl_symbol => last_cl_symbol% next

   else subsequent_or_first

      first_cl_symbol => integer_ptr

      last_cl_symbol => first_cl_symbol

   end if subsequent_or_first

   nullify( integer_ptr% next)

! ----------------------------------------------------------------------

!  set symbol members

   integer_ptr% name_str = integer_name

! ----------------------------------------------------------------------

!  decode the value

   call eval_int_expr( int_decl_str( len( equals) + 1: ), integer_ptr% integer_value)

! ----------------------------------------------------------------------

!  add_cl_integer() exit

return

! **********************************************************************

!  add_cl_integer()

end subroutine add_cl_integer

! **********************************************************************
! **********************************************************************

!  add_cl_logical() store logical from the command line in cl symbol table

subroutine add_cl_logical( logical_name, logical_value)

! **********************************************************************

!  add_logical() interface

! ----------------------------------------------------------------------

!  the valid logical name

character( len= *), intent( in) :: logical_name
logical, intent( in) :: logical_value

! **********************************************************************

!  entry: int_decl_str is blank_compress_lower_case logical declaration statement past the name
!         "" | "=..."
!         sym_name is the symbol name

!  exit: logical declaration is added to the logical symbol list or error exit

! **********************************************************************

!  add_logical() local

! ----------------------------------------------------------------------

!  type pointer to allocate

   type( logical_t), pointer :: logical_ptr

! **********************************************************************

!  add_logical() text

continue

! ----------------------------------------------------------------------

!  allocate new logical

   allocate( logical_ptr, stat= state% status, errmsg= state% message)

   alloc_error: if( is_status_error( state) )then

      call msg_quit( "can't allocate -D logical: " // trim( logical_name))

   end if alloc_error

!  build new logical on list

   subsequent_or_first: if( associated( first_cl_symbol) )then

      last_cl_symbol% next => logical_ptr

      last_cl_symbol => last_cl_symbol% next

   else subsequent_or_first

      first_cl_symbol => logical_ptr

      last_cl_symbol => first_cl_symbol

   end if subsequent_or_first

   nullify( logical_ptr% next)

! ----------------------------------------------------------------------

!  set symbol members

   logical_ptr% name_str = logical_name

! ----------------------------------------------------------------------

!  logicals on the command line are true unless specified by -U (extension)

   logical_ptr% logical_value = logical_value

! ----------------------------------------------------------------------

!  add_cl_logical() exit

return

! **********************************************************************

!  add_cl_logical()

end subroutine add_cl_logical

! **********************************************************************
! **********************************************************************

!  process_freeze_option() process freeze argument 'filename'

subroutine process_freeze_option( file_name)

! **********************************************************************

!  process_freeze_option() interface

! ----------------------------------------------------------------------

!  the symbol string from the command line

character( len= *), intent( in) :: file_name

! **********************************************************************

!  entry: file_name is string following -f 'filename'

!  exit: file_name is processed or error exit

! **********************************************************************

!  process_freeze_option() local

! ----------------------------------------------------------------------

!  allow only one -f per command line

   logical, save :: too_many_freeze_options = .false.

! **********************************************************************

!  process_freeze_option() text

continue

! ----------------------------------------------------------------------

!  execute only once

   too_many: if( too_many_freeze_options )then

      call msg_quit( "too many freeze options on command line")

   else too_many

      too_many_freeze_options = .true.

   end if too_many

!  allocate freeze file

   allocate( freeze_file, source= input_freeze_file, stat= state% status, errmsg= state% message)

   alloc_error: if( is_status_error( state) )then

      call msg_quit( "can't allocate freeze file for reading: " // trim( file_name))

   end if alloc_error

!  read freeze file

   call read_freeze_file()

! ----------------------------------------------------------------------

!  process_freeze_option() exit

return

! **********************************************************************

!  process_freeze_option()

end subroutine process_freeze_option

! **********************************************************************
! **********************************************************************

!  read_freeze_file() open, process, close a freeze file

subroutine read_freeze_file()

! **********************************************************************

!  read_freeze_file() steps

!  1. open the freeze file
!  2. read the set file line by line
!  3. call blank_compress_lower_case() to construct a coco statement
!  4. ignore coco comments
!  5. call process_freeze_statement() to process coco set statement
!  6. close freeze file

! **********************************************************************

!  read_freeze_file() local

! ----------------------------------------------------------------------

!  process the set file statement by statement

   character( len= buffer_len) :: freeze_statement

! ----------------------------------------------------------------------

!  signal complete statement

   logical :: complete

! **********************************************************************

!  read_freeze_file() text

continue

! ----------------------------------------------------------------------

!  "current file" is the command line

!   current_file => command_line

!  open the freeze file for reading

   call freeze_file% open()

! ----------------------------------------------------------------------

!  count files processed

   total% input_files = total% input_files + 1

!  as if finished a complete statement at beginning of file

   complete = .true.

! ----------------------------------------------------------------------

!  main read set file lines loop

   read_lines: do

! ----------------------------------------------------------------------

!  read a freeze file line

      freeze_input: select type( freeze_file)

      type is( input_file_t) freeze_input

         call freeze_file% read()

      end select freeze_input

! ----------------------------------------------------------------------

!  read until end of file

      read_eof: if( is_status_info( state) )then

!  reset statement processing for the next file

         call blank_compress_lower_case( freeze_statement, null_string)

!  if in a statement continuation sequence

         premature_eof: if( .not. complete )then

            call msg_quit( "end of file encountered within a continuation sequence")

         end if premature_eof

!  exit the read lines loop

         exit read_lines

      end if read_eof

! ----------------------------------------------------------------------

!  process set file lines or error if source lines

      coco_line: if( line( 1: len( coco_key)) == coco_key )then

!  count coco lines

         total% coco_lines = total% coco_lines + 1

!  process set file lines, ignore coco comments

         coco_statement: if( is_coco_statement( line( len( coco_key) + 1: )) )then

! ----------------------------------------------------------------------

!  read a complete statement line by line

            call gather_coco_statement( line, freeze_statement, complete)

!  if not yet a complete statement go get the rest of it

            get_statement: if( .not. complete )then

               cycle read_lines

            end if get_statement

!  process the complete set file statement

            call process_freeze_statement( freeze_statement)

!  process set file lines, ignore coco comments

         end if coco_statement

!  source line in set file

      else coco_line

         allow_blank_lines: if( line /= blank .or. .not. complete )then

            call msg_quit( "source lines are not allowed in the freeze file")

         end if allow_blank_lines

!  end processing set statements

      end if coco_line

! ----------------------------------------------------------------------

!  end main read set file lines loop

   end do read_lines

   total% input_lines = total% input_lines + freeze_file% lines_transfered

! ----------------------------------------------------------------------

!  close the set file

   call freeze_file% close()

! ----------------------------------------------------------------------

!  no more source file declarations

   options% freeze_declarations = .true.

! ----------------------------------------------------------------------

!  read_freeze_file() exit

return

! **********************************************************************

!  read_freeze_file()

end subroutine read_freeze_file

! **********************************************************************
! **********************************************************************

!  copy_freeze_file() copy freeze file to output file

subroutine copy_freeze_file()

! **********************************************************************

!  copy_freeze_file() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  open the freeze file

   call freeze_file% open()

! ----------------------------------------------------------------------

!  count lines correctly when appending the freeze file to the output

   freeze_file% lines_transfered = 0

!  copy each line

   copy_lines: do

!  read a line

      freeze_input: select type( freeze_file)

      type is( input_file_t) freeze_input

         call freeze_file% read()

      end select freeze_input

!  read entire set file

      end_freeze_file: if( is_status_info( state) )then

         exit copy_lines

      end if end_freeze_file

!  write a line

      call write_coco_line( output_file)

   end do copy_lines

! ----------------------------------------------------------------------

!  close the freeze file

   call freeze_file% close()

! ----------------------------------------------------------------------

!  copy_freeze_file() exit

return

! **********************************************************************

!  copy_freeze_file()

end subroutine copy_freeze_file

! **********************************************************************
! **********************************************************************

!  process_freeze_statement() process freeze line

subroutine process_freeze_statement( freeze_stmt)

! **********************************************************************

!  process_freeze_statement() interface

! ----------------------------------------------------------------------

!  the statement string from the freeze file

character( len= *), intent( in) :: freeze_stmt

! **********************************************************************

!  entry: freeze_stmt is blank_compress_lower_case set statement past the coco key
!         "integer..." | "logical..." | "macro..." | "text..."

!  exit: freeze_stmt is processed or error exit

! **********************************************************************

!  process_freeze_statement() text

continue

! ----------------------------------------------------------------------

!  catergorize freeze file statement: integer, logical, macro, text

! ----------------------------------------------------------------------

!  if the directive is a freeze file integer declaration

   which_directive: if( freeze_stmt( 1: len( integer_str)) == integer_str )then

      call process_integer_declaration( freeze_stmt( len( integer_str) + 1: ), .false.)

! ----------------------------------------------------------------------

!  integer constant declaration

   else if( freeze_stmt( 1: len( integer_constant_str)) == integer_constant_str )then which_directive

      call process_integer_declaration( freeze_stmt( len( integer_constant_str) + 1: ), .true.)

! ----------------------------------------------------------------------

!  if the directive is a freeze file logical declaration

   else if( freeze_stmt( 1: len( logical_str)) == logical_str )then which_directive

      call process_logical_declaration( freeze_stmt( len( logical_str) + 1: ), .false.)

! ----------------------------------------------------------------------

!  logical constant declaration

   else if( freeze_stmt( 1: len( logical_constant_str)) == logical_constant_str )then which_directive

      call process_logical_declaration( freeze_stmt( len( logical_constant_str) + 1: ), .true.)

! ----------------------------------------------------------------------

!  macro declaration

   else if( freeze_stmt( 1: len( macro_str)) == macro_str )then which_directive

      call process_macro_declaration( freeze_stmt( len( macro_str) + 1: ), .false.)

! ----------------------------------------------------------------------

!  macro, parens declaration

   else if( freeze_stmt( 1: len( macro_parens_str)) == macro_parens_str )then which_directive

      call process_macro_declaration( freeze_stmt( len( macro_parens_str) + 1: ), .true.)

! ----------------------------------------------------------------------

!  text directive

   else if( freeze_stmt( 1: len( text_str)) == text_str )then which_directive

      call process_text_directive( freeze_stmt( len( text_str) + 1: ), .false.)

! ----------------------------------------------------------------------

!  text, parens directive

   else if( freeze_stmt( 1: len( text_parens_str)) == text_parens_str )then which_directive

      call process_text_directive( freeze_stmt( len( text_parens_str) + 1: ), .true.)

! ----------------------------------------------------------------------

!  otherwise complain about the unknown directive

   else which_directive

      call msg_quit( "unknown freeeze file directive: " // trim( freeze_stmt))

!  catergorize set file statement: alter or integer or logical or extension

   end if which_directive

! ----------------------------------------------------------------------

!  process_freeze_statement() exit

return

! **********************************************************************

!  process_freeze_statement()

end subroutine process_freeze_statement

! **********************************************************************
! **********************************************************************

!  process_fixed_option() process fixed arguments

subroutine process_fixed_option()

! **********************************************************************

!  entry:

!  exit: -F is processed or error exit

! **********************************************************************

!  process_fixed_option() local

! ----------------------------------------------------------------------

!  allow only one -F per command line

   logical, save :: too_many_fixed_options = .false.

! **********************************************************************

!  process_fixed_option() text

continue

! ----------------------------------------------------------------------

!  execute only once

   too_many: if( too_many_fixed_options )then

      call msg_quit( "too many fixed options on command line")

   else too_many

      too_many_fixed_options = .true.

   end if too_many

! ----------------------------------------------------------------------

   cl_options% free_form = .false.

! ----------------------------------------------------------------------

!  process_fixed_option() exit

return

! **********************************************************************

!  process_fixed_option()

end subroutine process_fixed_option

! **********************************************************************
! **********************************************************************

!  print_help() write usage message and options to stderr

subroutine print_help()

! **********************************************************************

!  entry: in response to -h command line option

!  exit: print help message

! **********************************************************************

!  print_help() constants

character( len= *), parameter :: help_fmt = '( a)'

! ----------------------------------------------------------------------

!  the options message

character( len= *), dimension( 1: 19), parameter :: options_msg = [ &
                      ' -a ?           set alter state, ? = { b, d, 0, 1, 3}             ', &
                      ' -D name[=n]    provide integer =n or logical true value          ', &
                      ' -f freeze-file read a freeze file and disallow declarations      ', &
                      ' -F             treat source as fixed form source                 ', &
                      ' -h             print this help message and quit                  ', &
                      ' -I directory   search directory for include files (after .)      ', &
                      ' -l log-file    write log messages to log-file (default stderr)   ', &
                      ' -m             mark subsequent input files in the output file    ', &
                      ' -n             print line numbers on source lines                ', &
                      ' -p             disable the copy of set file at end of output     ', &
                      ' -r             print summary report at end of processing         ', &
                      ' -s set-file    read set-file as the set file (default coco.set)  ', &
                      ' -S             seek no default set file                          ', &
                      ' -U name        provide logical false value                       ', &
                      ' -v             report file opening and closing                   ', &
                      ' -V             print coco version and quit                       ', &
                      ' -w             disable line wrapping                             ', &
                      ' -W             enable warning messages                           ', &
                      ' --             optionally separate options from source file names' ]

! **********************************************************************

!  print_help() local

! ----------------------------------------------------------------------

!  implied do variable

   integer :: i

! **********************************************************************

!  print_help() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

   write( unit= error_unit, fmt= help_fmt) usage_msg

   write( unit= error_unit, fmt= help_fmt) ( trim( options_msg( i)), i = 1, size( options_msg))

! ----------------------------------------------------------------------

!  print_help() exit

return

! **********************************************************************

!  print_help()

end subroutine print_help

! **********************************************************************
! **********************************************************************

!  process_include_option() process include directory options

subroutine process_include_option( directory_opt)

! **********************************************************************

!  process_include_option() interface

! ----------------------------------------------------------------------

!  the directory string from the command line

character( len= *), intent( in) :: directory_opt

! **********************************************************************

!  entry: directory_opt is a directory to be added to the list
!         of directories to be searched for inlcude files

!  exit: directory_opt is on the list

! **********************************************************************

!  process_include_option() local

! ----------------------------------------------------------------------

!  point to a directory type

   type( path_t), pointer :: path_ptr

!  unquote the directory name if needed

   character( len= file_name_len) :: directory_str

!  lengths of quoted string

   integer :: quoted_len

!  lengths of unquoted string

   integer :: unquoted_len

! **********************************************************************

!  process_include_option() text

continue

! ----------------------------------------------------------------------

!  if the directory is quoted, unquote it

   call unquote_string( directory_opt, directory_str, quoted_len, unquoted_len)

   badly_quoted: if( unquoted_len == 0 )then

      call msg_quit( "null name passed to -I option")

   else if( quoted_len > len_trim( directory_opt) )then badly_quoted

      call msg_quit( "badly quoted name passed to -I option")

   end if badly_quoted

! ----------------------------------------------------------------------

!  if name is already on the path

   nullify( path_ptr)

   call seek_directory( directory_str, path_ptr)

   on_list_or_add: if( associated( path_ptr) )then

      call msg_continue( "redundant include directory ignored: " // trim( directory_opt) )

   else on_list_or_add

      call add_directory( directory_str)

   end if on_list_or_add

! ----------------------------------------------------------------------

!  process_include_option() exit

return

! **********************************************************************

!  process_include_option()

end subroutine process_include_option

! **********************************************************************
! **********************************************************************

!  process_log_file_option() process log_file arguments

subroutine process_log_file_option( log_file_arg)

! **********************************************************************

!  process_log_file_option

character( len= *), intent( in) :: log_file_arg

! **********************************************************************

!  entry: log_file_arg is log file name

!  exit: -l is processed or error exit

! **********************************************************************

!  process_log_file_option() local

! ----------------------------------------------------------------------

!  allow only one -l per command line

   logical, save :: too_many_log_file_options = .false.

! **********************************************************************

!  process_log_file_option() text

continue

! ----------------------------------------------------------------------

!  execute only once

   too_many: if( too_many_log_file_options )then

      call msg_quit( "too many log file options on command line")

   else too_many

      too_many_log_file_options = .true.

   end if too_many

! ----------------------------------------------------------------------

   log_file% name_str = log_file_arg

   call log_file% open()

! ----------------------------------------------------------------------

!  process_log_file_option() exit

return

! **********************************************************************

!  process_log_file_option()

end subroutine process_log_file_option

! **********************************************************************
! **********************************************************************

!  process_mark_option() process number arguments

subroutine process_mark_option()

! **********************************************************************

!  entry: from -m encountered

!  exit: -m is processed or error exit

! **********************************************************************

!  process_mark_option() local

! ----------------------------------------------------------------------

!  allow only one -m per command line

   logical, save :: too_many_mark_options = .false.

! **********************************************************************

!  process_mark_option() text

continue

! ----------------------------------------------------------------------

!  execute only once

   too_many: if( too_many_mark_options )then

      call msg_quit( "too many mark options on command line")

   else too_many

      too_many_mark_options = .true.

   end if too_many

! ----------------------------------------------------------------------

   cl_options% mark_input = .true.

! ----------------------------------------------------------------------

!  process_mark_option() exit

return

! **********************************************************************

!  process_mark_option()

end subroutine process_mark_option

! **********************************************************************
! **********************************************************************

!  process_number_option() process number arguments

subroutine process_number_option()

! **********************************************************************

!  entry: from -n encountered

!  exit: -n is processed or error exit

! **********************************************************************

!  process_number_option() local

! ----------------------------------------------------------------------

!  allow only one -n per command line

   logical, save :: too_many_number_options = .false.

! **********************************************************************

!  process_number_option() text

continue

! ----------------------------------------------------------------------

!  execute only once

   too_many: if( too_many_number_options )then

      call msg_quit( "too many number options on command line")

   else too_many

      too_many_number_options = .true.

   end if too_many

! ----------------------------------------------------------------------

   cl_options% number_source = .true.

! ----------------------------------------------------------------------

!  process_number_option() exit

return

! **********************************************************************

!  process_number_option()

end subroutine process_number_option

! **********************************************************************
! **********************************************************************

!  process_postpend_option() process postpend arguments

subroutine process_postpend_option()

! **********************************************************************

!  entry: from -p encountered

!  exit: -p is processed or error exit

! **********************************************************************

!  process_postpend_option() local

! ----------------------------------------------------------------------

!  allow only one -p per command line

   logical, save :: too_many_postpend_options = .false.

! **********************************************************************

!  process_postpend_option() text

continue

! ----------------------------------------------------------------------

!  execute only once

   too_many: if( too_many_postpend_options )then

      call msg_quit( "too many postpend options on command line")

   else too_many

      too_many_postpend_options = .true.

   end if too_many

! ----------------------------------------------------------------------

   cl_options% postpend_set_file = .false.

! ----------------------------------------------------------------------

!  process_postpend_option() exit

return

! **********************************************************************

!  process_postpend_option()

end subroutine process_postpend_option

! **********************************************************************
! **********************************************************************

!  process_report_option() process report arguments

subroutine process_report_option()

! **********************************************************************

!  entry: from -r encountered

!  exit: -r is processed or error exit

! **********************************************************************

!  process_report_option() local

! ----------------------------------------------------------------------

!  allow only one -r per command line

   logical, save :: too_many_report_options = .false.

! **********************************************************************

!  process_report_option() text

continue

! ----------------------------------------------------------------------

!  execute only once

   too_many: if( too_many_report_options )then

      call msg_quit( "too many report options on command line")

   else too_many

      too_many_report_options = .true.

   end if too_many

! ----------------------------------------------------------------------

   cl_options% print_report = .true.

! ----------------------------------------------------------------------

!  process_report_option() exit

return

! **********************************************************************

!  process_report_option()

end subroutine process_report_option

! **********************************************************************
! **********************************************************************

!  process_set_file_option() process set_file arguments

subroutine process_set_file_option( set_file_opt)

! **********************************************************************

!  process_set_file_option

character( len= *), intent( in) :: set_file_opt

! **********************************************************************

!  entry: set_file_arg is set file name

!  exit: -s is processed or error exit

! **********************************************************************

!  process_set_file_option() local

! ----------------------------------------------------------------------

!  allow only one -s per command line

   logical, save :: too_many_set_file_options = .false.

! **********************************************************************

!  process_set_file_option() text

continue

! ----------------------------------------------------------------------

!  execute only once

   too_many: if( too_many_set_file_options )then

      call msg_quit( "too many set file options on command line")

   else too_many

      too_many_set_file_options = .true.

   end if too_many

! ----------------------------------------------------------------------

   dash_s_name = set_file_opt

! ----------------------------------------------------------------------

!  process_set_file_option() exit

return

! **********************************************************************

!  process_set_file_option()

end subroutine process_set_file_option

! **********************************************************************
! **********************************************************************

!  process_no_set_file_option() process no set file options

subroutine process_no_set_file_option()

! **********************************************************************

!  entry:

!  exit: -S is processed or error exit

! **********************************************************************

!  process_no_set_file_option() local

! ----------------------------------------------------------------------

!  allow only one -S per command line

   logical, save :: too_many_set_file_options = .false.

! **********************************************************************

!  process_no_set_file_option() text

continue

! ----------------------------------------------------------------------

!  execute only once

   too_many: if( too_many_set_file_options )then

      call msg_quit( "too many -S options on command line")

   else too_many

      too_many_set_file_options = .true.

   end if too_many

! ----------------------------------------------------------------------

!  this sets the options rather than the cl_options

   options% seek_set_file = .false.

! ----------------------------------------------------------------------

!  process_no_set_file_option() exit

return

! **********************************************************************

!  process_no_set_file_option()

end subroutine process_no_set_file_option

! **********************************************************************
! **********************************************************************

!  process_verbose_option() process verbose arguments

subroutine process_verbose_option()

! **********************************************************************

!  entry:

!  exit: -v is processed or error exit

! **********************************************************************

!  process_verbose_option() local

! ----------------------------------------------------------------------

!  allow only one -v per command line

   logical, save :: too_many_verbose_options = .false.

! **********************************************************************

!  process_verbose_option() text

continue

! ----------------------------------------------------------------------

!  execute only once

   too_many: if( too_many_verbose_options )then

      call msg_quit( "too many verbose options on command line")

   else too_many

      too_many_verbose_options = .true.

   end if too_many

! ----------------------------------------------------------------------

   cl_options% verbose_mode = .true.

! ----------------------------------------------------------------------

!  process_verbose_option() exit

return

! **********************************************************************

!  process_verbose_option()

end subroutine process_verbose_option

! **********************************************************************
! **********************************************************************

!  process_wrap_option() process wrap arguments

subroutine process_wrap_option()

! **********************************************************************

!  entry:

!  exit: -v is processed or error exit

! **********************************************************************

!  process_wrap_option() local

! ----------------------------------------------------------------------

!  allow only one -w per command line

   logical, save :: too_many_wrap_options = .false.

! **********************************************************************

!  process_wrap_option() text

continue

! ----------------------------------------------------------------------

!  execute only once

   too_many: if( too_many_wrap_options )then

      call msg_quit( "too many wrap options on command line")

   else too_many

      too_many_wrap_options = .true.

   end if too_many

! ----------------------------------------------------------------------

   cl_options% wrapping_lines = .false.

! ----------------------------------------------------------------------

!  process_wrap_option() exit

return

! **********************************************************************

!  process_wrap_option()

end subroutine process_wrap_option

! **********************************************************************
! **********************************************************************

!  process_warning_option() process wrap arguments

subroutine process_warning_option()

! **********************************************************************

!  entry:

!  exit: -W is processed or error exit

! **********************************************************************

!  process_warning_option() local

! ----------------------------------------------------------------------

!  allow only one -w per command line

   logical, save :: too_many_warning_options = .false.

! **********************************************************************

!  process_warning_option() text

continue

! ----------------------------------------------------------------------

!  execute only once

   too_many: if( too_many_warning_options )then

      call msg_quit( "too many warning options on command line")

   else too_many

      too_many_warning_options = .true.

   end if too_many

! ----------------------------------------------------------------------

   cl_options% warning = .true.

! ----------------------------------------------------------------------

!  process_warning_option() exit

return

! **********************************************************************

!  process_warning_option()

end subroutine process_warning_option

! **********************************************************************
! **********************************************************************

!  %%% diagnostic directives write to the log file

! **********************************************************************
! **********************************************************************

!  process_cmdline_directive() write command line to the log file

subroutine process_cmdline_directive( cmdline_dir)

! **********************************************************************

!  process_cmdline_directive() text

! ----------------------------------------------------------------------

character( len= *), intent( in) :: cmdline_dir

! **********************************************************************

!  process_cmdline_directive() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  check for well formed directive

   extra_chars: if( cmdline_dir /= blank )then

      call msg_quit( "extra characters at end of cmdline directive: " // trim( cmdline_dir))

   end if extra_chars

! ----------------------------------------------------------------------

!  if an active line

   active_line: if( if_construct% now_selected )then

!  write command line to stderr or the log file

      write( unit= log_file% io_unit, fmt= string_fmt) 'coco command line: ' &
                                                               // predefined_macros( cmdline_ss)% macro_value

   end if active_line

! ----------------------------------------------------------------------

!  process_cmdline_directive() exit

return

! **********************************************************************

!  process_cmdline_directive()

end subroutine process_cmdline_directive

! **********************************************************************
! **********************************************************************

!  write_options() write options in effect to the log file

subroutine write_options( log_file)

! **********************************************************************

!  write_options() interface

type( log_file_t), intent( in) :: log_file

! **********************************************************************

!  write_options() constants

! ----------------------------------------------------------------------

!  possible alter states

integer, parameter :: lower_alter = min( alter_delete, alter_blank, alter_shift_1, alter_shift_0, alter_shift_3)

integer, parameter :: upper_alter = max( alter_delete, alter_blank, alter_shift_1, alter_shift_0, alter_shift_3)

!  possible alter state labels

character( len= 16), dimension( lower_alter: upper_alter), parameter :: alter_labels = [ &
                                       'deleted         ', &
                                       'blank line      ', &
                                       'initial !       ', &
                                       'shifted 1 + !   ', &
                                       'shifted 3 + !?> ' ]

! **********************************************************************

!  write_options() local

! ----------------------------------------------------------------------

!  construct output lines

   character( len= buffer_len) :: output_line

! **********************************************************************

!  write_options() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  write a header

   write( unit= log_file% io_unit, fmt= string_fmt) "coco options:"

! ----------------------------------------------------------------------

!  identify the alter state

   check_index: select case( options% alter_state)

   case( lower_alter: upper_alter) check_index

      output_line = 'alter state causes lines to be ' // alter_labels( options% alter_state)

   case default check_index

      output_line = 'alter state is undefined'

   end select check_index

   write( unit= log_file% io_unit, fmt= string_fmt) trim( output_line)

!  identify whether free form or fixed form

   rpt_form: if( options% free_form )then

      output_line = 'processing free form source with wrap len:'

   else rpt_form

      output_line = 'processing fixed form source with wrap len:'

   end if rpt_form

   write( unit= log_file% io_unit, fmt= string_fmt) trim( output_line) // blank, options% wrap_len

!  identify whether printing new input file mark

   rpt_mark: if( options% mark_input )then

      output_line = 'marking subsequent input files'

   else rpt_mark

      output_line = 'not marking subsequent input files'

   end if rpt_mark

   write( unit= log_file% io_unit, fmt= string_fmt) trim( output_line)

!  identify whether printing "! file: line" on source lines

   rpt_number: if( options% number_source )then

      output_line = 'numbering source lines'

   else rpt_number

      output_line = 'not numbering source lines'

   end if rpt_number

   write( unit= log_file% io_unit, fmt= string_fmt) trim( output_line)

!  identify whether postpending the set file

   rpt_post: if( options% postpend_set_file )then

      output_line = 'appending set file ' // set_file% name_str

   else rpt_post

      output_line = 'not appending set file ' // set_file% name_str

   end if rpt_post

   write( unit= log_file% io_unit, fmt= string_fmt) trim( output_line)

!  identify whether printing coco report

   rpt_prt: if( options% print_report )then

      output_line = 'printing coco report'

   else rpt_prt

      output_line = 'not printing coco report'

   end if rpt_prt

   write( unit= log_file% io_unit, fmt= string_fmt) trim( output_line)

!  identify whether verbose mode is on

   rpt_verbose: if( options% verbose_mode )then

      output_line = 'verbose mode is on'

   else rpt_verbose

      output_line = 'verbose mode is off'

   end if rpt_verbose

   write( unit= log_file% io_unit, fmt= string_fmt) trim( output_line)

!  identify whether wrapping lines

   rpt_wrap: if( options% wrapping_lines )then

      output_line = 'wrapping source lines'

   else rpt_wrap

      output_line = 'not wrapping source lines'

   end if rpt_wrap

   write( unit= log_file% io_unit, fmt= string_fmt) trim( output_line)

!  found a file name directory separator character

   rpt_sep_char: if( options% got_sep_char )then

      output_line = 'found directory separator character ' // options% sep_char

   else rpt_sep_char

      output_line = 'no directory separator character found'

   end if rpt_sep_char

   write( unit= log_file% io_unit, fmt= string_fmt) trim( output_line)

! ----------------------------------------------------------------------

!  write_options() exit

return

! **********************************************************************

!  write_options()

end subroutine write_options

! **********************************************************************
! **********************************************************************

!  write_report() write summary report to the log file

subroutine write_report()

! **********************************************************************

!  write_report() local

! ----------------------------------------------------------------------

!  print date and time in header

   character( len= 8) :: today_str

   character( len= 10) :: now_str

! ----------------------------------------------------------------------

!  print include path

   type( path_t), pointer :: path_ptr

! ----------------------------------------------------------------------

!  print input files

   integer :: i

! **********************************************************************

!  write_report() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  banner includes the date and time

   call date_and_time( date= today_str, time= now_str)

   write( unit= log_file% io_unit, fmt= string_fmt) 'coco executed: ' // format_date( today_str) &
                                                                                // blank // format_time( now_str)

! ----------------------------------------------------------------------

!  identify the set file

   write( unit= log_file% io_unit, fmt= string_fmt) 'set file: ' // trim( set_file% name_str)

! ----------------------------------------------------------------------

!  identify the output file

   write( unit= log_file% io_unit, fmt= string_fmt) 'output: ' // trim( output_file% name_str)

! ----------------------------------------------------------------------

!  identify the input file(s)

   one_or_more: if( allocated( source_file_list) )then

      write( unit= log_file% io_unit, fmt= string_fmt, advance= 'NO') 'input:'

      more_than_one: do i = 1, size( source_file_list)

         write( unit= log_file% io_unit, fmt= string_fmt, advance= 'NO') &
                blank // trim( source_file_list( i)% name_str)

      end do more_than_one

      write( unit= log_file% io_unit, fmt= string_fmt)

   else one_or_more

      write( unit= log_file% io_unit, fmt= string_fmt) 'input: ' // trim( input_file% name_str)

   end if one_or_more

! ----------------------------------------------------------------------

!  identify the include path

   write_sep_char: if( options% got_sep_char )then

      write( unit= log_file% io_unit, fmt= string_fmt, advance= 'no') &
         'include path: .' // options% sep_char // ' (', count_include_in_dot, ')'

   else write_sep_char

      write( unit= log_file% io_unit, fmt= string_fmt, advance= 'no') 'include path: . (', count_include_in_dot, ')'

   end if write_sep_char

   nullify( path_ptr)

   path_ptr => first_directory

   inc_path: do

      if( .not. associated( path_ptr) ) exit inc_path

      write( unit= log_file% io_unit, fmt= string_fmt, advance= 'no') &
             comma // blank // trim( path_ptr% name_str) // blank // open_paren, path_ptr% times_accessed, close_paren

      path_ptr => path_ptr% next

   end do inc_path

!  end line using null string

   write( unit= log_file% io_unit, fmt= string_fmt)

! ----------------------------------------------------------------------

!  number of files read

   write( unit= log_file% io_unit, fmt= string_fmt) &
          'files read: ', total% input_files

   write( unit= log_file% io_unit, fmt= string_fmt) &
          'include files read: ', total% include_files

!  number of set file lines read

   write( unit= log_file% io_unit, fmt= string_fmt) &
          'set lines read: ', set_file% lines_transfered

!  number of coco lines read

   write( unit= log_file% io_unit, fmt= string_fmt) &
          'coco lines read: ', total% coco_lines

!  number of source lines read

   write( unit= log_file% io_unit, fmt= string_fmt) &
          'source lines read: ', total% input_lines

!  number of lines written

   write( unit= log_file% io_unit, fmt= string_fmt) &
          'source lines written: ', output_file% lines_transfered

!  number of selected lines written

   write( unit= log_file% io_unit, fmt= string_fmt) &
          'selected source lines: ', total% selected_lines

!  number of elided lines

   write( unit= log_file% io_unit, fmt= string_fmt) &
          'elided source lines: ', total% elided_lines

!  number of text blocks read

   write( unit= log_file% io_unit, fmt= string_fmt) &
          'text blocks read: ', total% text_blocks

!  number of text lines read

   write( unit= log_file% io_unit, fmt= string_fmt) &
          'text lines read: ', total% text_lines

!  number of text lines written

   write( unit= log_file% io_unit, fmt= string_fmt) &
          'text lines written: ', total% copied_lines

! ----------------------------------------------------------------------

!  write_report() exit

return

! **********************************************************************

!  write_report()

end subroutine write_report

! **********************************************************************
! **********************************************************************

!  %%% seek and process the set file (if any)

! **********************************************************************
! **********************************************************************

!  seek_set_file() find a set file to open or don't read one

subroutine seek_set_file()

! **********************************************************************

!  seek_set_file constants

! ----------------------------------------------------------------------

!  default set_file name

character( len= *), parameter :: default_name = 'coco.set'

!  default set_file name environment variable name

character( len= *), parameter :: env_var_name = 'COCO_SET_FILE'

! **********************************************************************

!  seek_set_file data

! ----------------------------------------------------------------------

!  return whether named file exists

   logical :: file_exists

!  whether set file has been found

   logical :: file_found

!  return the length of the environment variable's value

   integer :: ev_len

!  get name from environment variable

   character( len= file_name_len) :: ev_str

! **********************************************************************

!  seek_set_file() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  not found yet

   file_found = .false.
   file_exists = .false.

!  see if there is name for the set_file from the command line -s name

   dash_s_named_set_file: if( dash_s_name% named )then

!  inquire by file looking for named set file

      inquire( file= dash_s_name% name_str, exist= file_exists, iostat= state% status, iomsg= state% message)

      dash_s_inquire: if( is_status_error( state) )then

         call msg_continue( "can't inquire set file named on command line: " // trim( dash_s_name% name_str))

         file_exists = .false.

      end if dash_s_inquire

!  if found, use it

      use_dash_s: if( file_exists )then

         set_file% name_str = dash_s_name% name_str

         file_found = .true.

      end if use_dash_s

   end if dash_s_named_set_file

! ----------------------------------------------------------------------

!  if not seeking a set file, quit now

   if( .not. options% seek_set_file ) return

! ----------------------------------------------------------------------

!  see if there is name for the set_file from the source file names

   base_name_named_set_file: if( ( .not. file_found) .and. base_name% named )then

!  inquire by file looking for named set file

      inquire( file= base_name% name_str, exist= file_exists, iostat= state% status, iomsg= state% message)

      inq_named: if( is_status_error( state) )then

         call msg_continue( "can't inquire set file: " // trim( base_name% name_str))

         file_exists = .false.

      end if inq_named

!  if found, use it

      use_basename: if( file_exists )then

         set_file% name_str = base_name% name_str

         file_found = .true.

      end if use_basename

!  done checking command line set file name

   end if base_name_named_set_file

! ----------------------------------------------------------------------

!  if not found

   environment_named_set_file: if( .not. file_found )then

!  next check the environment variable

      call get_environment_variable( name= env_var_name, value= ev_str, length= ev_len, status= state% status)

      found_env_var: if( is_status_ok( state) .and. ev_len > 0 )then

!  inquire by file looking for named set file

         inquire( file= ev_str, exist= file_exists, iostat= state% status, iomsg= state% message)

         env_inq_named: if( is_status_error( state) )then

            call msg_continue( "can't inquire set file named from environment: " // trim( ev_str))

            file_exists = .false.

         end if env_inq_named

!  if found, use it

         use_ev_name: if( file_exists )then

            set_file% name_str = ev_str

            file_found = .true.

         end if use_ev_name

      end if found_env_var

!  now have checked all sources

   end if environment_named_set_file

!  if no named set file then try to find the default set file

   default_set_file: if( .not. file_exists )then

!  inquire by file looking for default set file

      inquire( file= default_name, exist= file_exists, iostat= state% status, iomsg= state% message)

      inq_default: if( is_status_error( state) )then

         call msg_quit( "can't inquire default set file: " // default_name)

         file_exists = .false.

      end if inq_default

!  if found the default set file ensure the variable correctly specifies it

      use_default: if( file_exists )then

         set_file% name_str = default_name

         file_found = .true.

      end if use_default

   end if default_set_file

! ----------------------------------------------------------------------

!  if have set file, open it, process it, close it

   read_set_file: if( file_found )then

      call process_set_file()

   end if read_set_file

! ----------------------------------------------------------------------

!  seek_set_file() exit

return

! **********************************************************************

!  seek_set_file()

end subroutine seek_set_file

! **********************************************************************
! **********************************************************************

!  process_sefile() open, process, close the coco set file

subroutine process_set_file()

! **********************************************************************

!  process_set_file() steps

!  1. open the set file
!  2. open the set scratch file
!  2. read the set file line by line
!  3. call blank_compress_lower_case() to construct a coco statement
!  4. ignore coco comments
!  5. call process_set_statement() to process coco set statement
!  6. close set file

! **********************************************************************

!  process_set_file() local

! ----------------------------------------------------------------------

!  process the set file statement by statement

   character( len= buffer_len) :: set_statement

! ----------------------------------------------------------------------

!  signal complete statement

   logical :: complete

! **********************************************************************

!  process_set_file() text

continue

! ----------------------------------------------------------------------

!  open the set file for reading

   call set_file% open()

! ----------------------------------------------------------------------

!  count files processed

   total% input_files = total% input_files + 1

!  as if finished a complete statement at beginning of file

   complete = .true.

! ----------------------------------------------------------------------

!  main read set file lines loop

   read_lines: do

! ----------------------------------------------------------------------

!  read a set file line

      call set_file% read()

! ----------------------------------------------------------------------

!  read until end of file

      read_eof: if( is_status_info( state) )then

!  reset statement processing for the next file

         call blank_compress_lower_case( set_statement, null_string)

!  if in a statement continuation sequence

         premature_eof: if( .not. complete )then

            call msg_quit( "end of file encountered within a continuation sequence")

         end if premature_eof

!  exit the read lines loop

         exit read_lines

      end if read_eof

! ----------------------------------------------------------------------

!  process set file lines or error if source lines

      coco_line: if( line( 1: len( coco_key)) == coco_key )then

!  count coco lines

         total% coco_lines = total% coco_lines + 1

!  process set file lines, ignore coco comments

         coco_statement: if( is_coco_statement( line( len( coco_key) + 1: )) )then

! ----------------------------------------------------------------------

!  read a complete statement line by line

            call gather_coco_statement( line, set_statement, complete)

!  if not yet a complete statement go get the rest of it

            get_statement: if( .not. complete )then

               cycle read_lines

            end if get_statement

!  process the complete set file statement

            call process_set_statement( set_statement)

!  process set file lines, ignore coco comments

         end if coco_statement

!  source line in set file

      else coco_line

         allow_blank_lines: if( line /= blank .or. .not. complete )then

            call msg_quit( "source lines are not allowed in the set file")

         end if allow_blank_lines

!  end processing set statements

      end if coco_line

! ----------------------------------------------------------------------

!  end main read set file lines loop

   end do read_lines

   total% input_lines = total% input_lines + set_file% lines_transfered

! ----------------------------------------------------------------------

!  close the set file

   call set_file% close()

! ----------------------------------------------------------------------

!  process_set_file() exit

return

! **********************************************************************

!  process_set_file()

end subroutine process_set_file

! **********************************************************************
! **********************************************************************

!  copy_set_file() copy set file to output file

subroutine copy_set_file()

! **********************************************************************

!  copy_set_file() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  open the set file

   call set_file% open()

! ----------------------------------------------------------------------

!  count lines correctly when appending the set file to the output

   set_file% lines_transfered = 0

!  copy each line

   copy_lines: do

!  read a line

      call set_file% read()

!  read entire set file

      read_set_file: if( is_status_info( state) )then

         exit copy_lines

      end if read_set_file

!  count lines for possible line numbering

      set_file% lines_transfered = set_file% lines_transfered + 1

!  write a line

      call write_coco_line( output_file)

   end do copy_lines

! ----------------------------------------------------------------------

!  close the set file

   call set_file% close()

! ----------------------------------------------------------------------

!  copy_set_file() exit

return

! **********************************************************************

!  copy_set_file()

end subroutine copy_set_file

! **********************************************************************
! **********************************************************************

!  %%% process statements many of which may appear in the set file

! **********************************************************************
! **********************************************************************

!  process_set_statement() process set line

subroutine process_set_statement( set_stmt)

! **********************************************************************

!  process_set_statement() interface

! ----------------------------------------------------------------------

!  the statement string from the set file

character( len= *), intent( in) :: set_stmt

! **********************************************************************

!  entry: set_stmt is blank_compress_lower_case set statement past the coco key
!         "alter:..." | "integer..." | "logical..." | "directory'...'" | "form:..." |
!         "input'...'" | "keys:..." | "logfile'...'" | "mark:..." | "number:..." |
!         "output'...'" | "post:..." | "report:..." | "verbose:..." | "wrap:..."

!  exit: set_stmt is processed or error exit

! **********************************************************************

!  process_set_statement() text

continue

! ----------------------------------------------------------------------

!  catergorize set file statement: alter, integer, logical, directory, wrap

! ----------------------------------------------------------------------

!  if the directive is an alter directive

   which_directive: if( set_stmt( 1: len( alter_str)) == alter_str )then

      call process_alter_directive( set_stmt( len( alter_str) + 1: ))

! ----------------------------------------------------------------------

!  if the directive is a set file integer declaration

   else if( set_stmt( 1: len( integer_str)) == integer_str )then which_directive

      call process_sf_integer_declaration( set_stmt( len( integer_str) + 1: ), .false.)

! ----------------------------------------------------------------------

!  integer constant declaration

   else if( set_stmt( 1: len( integer_constant_str)) == integer_constant_str )then which_directive

      call process_sf_integer_declaration( set_stmt( len( integer_constant_str) + 1: ), .true.)

! ----------------------------------------------------------------------

!  if the directive is a set file logical declaration

   else if( set_stmt( 1: len( logical_str)) == logical_str )then which_directive

      call process_sf_logical_declaration( set_stmt( len( logical_str) + 1: ), .false.)

! ----------------------------------------------------------------------

!  logical constant declaration

   else if( set_stmt( 1: len( logical_constant_str)) == logical_constant_str )then which_directive

      call process_sf_logical_declaration( set_stmt( len( logical_constant_str) + 1: ), .true.)

! ----------------------------------------------------------------------

!  if the directive is a set file directory directive

   else if( set_stmt( 1: len( directory_str)) == directory_str )then which_directive

      call process_directory_directive( set_stmt( len( directory_str) + 1: ))

! ----------------------------------------------------------------------

!  if the directive is a set file form directive

   else if( set_stmt( 1: len( form_str)) == form_str )then which_directive

      call process_form_directive( set_stmt( len( form_str) + 1: ))

! ----------------------------------------------------------------------

!  if the directive is a set file input directive

   else if( set_stmt( 1: len( input_str)) == input_str )then which_directive

      call process_input_directive( set_stmt( len( input_str) + 1: ))

! ----------------------------------------------------------------------

!  if the directive is a set file log file directive

   else if( set_stmt( 1: len( logfile_str)) == logfile_str )then which_directive

      call process_logfile_directive( set_stmt( len( logfile_str) + 1: ))

! ----------------------------------------------------------------------

!  if the directive is a set file mark: directive

   else if( set_stmt( 1: len( mark_str)) == mark_str )then which_directive

      call process_mark_directive( set_stmt( len( mark_str) + 1: ))

! ----------------------------------------------------------------------

!  if the directive is a set file number: directive

   else if( set_stmt( 1: len( number_str)) == number_str )then which_directive

      call process_number_directive( set_stmt( len( number_str) + 1: ))

! ----------------------------------------------------------------------

!  if the directive is a set file output directive

   else if( set_stmt( 1: len( output_str)) == output_str )then which_directive

      call process_sf_output_directive( set_stmt( len( output_str) + 1: ))

! ----------------------------------------------------------------------

!  if the directive is a set file post: directive

   else if( set_stmt( 1: len( post_str)) == post_str )then which_directive

      call process_post_directive( set_stmt( len( post_str) + 1: ))

! ----------------------------------------------------------------------

!  if the directive is a set file report: directive

   else if( set_stmt( 1: len( summary_str)) == summary_str )then which_directive

      call process_summary_directive( set_stmt( len( summary_str) + 1: ))

! ----------------------------------------------------------------------

!  if the directive is a set file verbose directive

   else if( set_stmt( 1: len( verbose_str)) == verbose_str )then which_directive

      call process_verbose_directive( set_stmt( len( verbose_str) + 1: ))

! ----------------------------------------------------------------------

!  if the directive is a set file warning directive

   else if( set_stmt( 1: len( warning_str)) == warning_str )then which_directive

      call process_warning_directive( set_stmt( len( warning_str) + 1: ))

! ----------------------------------------------------------------------

!  if the directive is a set file wrap directive

   else if( set_stmt( 1: len( wrap_str)) == wrap_str )then which_directive

      call process_wrap_directive( set_stmt( len( wrap_str) + 1: ))

! ----------------------------------------------------------------------

!  otherwise complain about the unknown directive

   else which_directive

      call msg_quit( "unknown set file directive: " // trim( set_stmt))

!  catergorize set file statement: alter or integer or logical or extension

   end if which_directive

! ----------------------------------------------------------------------

!  process_set_statement() exit

return

! **********************************************************************

!  process_set_statement()

end subroutine process_set_statement

! **********************************************************************
! **********************************************************************

!  process_alter_directive() process alter directives

subroutine process_alter_directive( alter_dir)

! **********************************************************************

!  process_alter_directive() interface

! ----------------------------------------------------------------------

!  the alter directive from the set file

character( len= *), intent( in) :: alter_dir

! **********************************************************************

!  entry: alter_dir is blank_compress_lower_case alter directive past the colon
!         "delete" | "blank" | "shift0" | "shift1" | "shift3"

!  exit: alter_dir is processed or error exit

! **********************************************************************

!  process_alter_directive() constants

! ----------------------------------------------------------------------

!  possible alter strings

character( len= *), parameter :: delete_str = 'delete'

character( len= *), parameter :: blank_str = 'blank'

character( len= *), parameter :: shift0_str = 'shift0'

character( len= *), parameter :: shift1_str = 'shift1'

character( len= *), parameter :: shift3_str = 'shift3'

! **********************************************************************

!  process_alter_directive() local

! ----------------------------------------------------------------------

!  count number of some statements to disallow more than one

   logical, save :: too_many_alter_statements = .false.

! **********************************************************************

!  process_alter_directive() text

continue

! ----------------------------------------------------------------------

!  only one alter directive per set file

   too_many_alters: if( too_many_alter_statements )then

      call msg_quit( "too many alter statements")

   else too_many_alters

      too_many_alter_statements = .true.

   end if too_many_alters

!  if the alter state has not been set from the command line

   not_set: if( options% alter_state == alter_none )then

! ----------------------------------------------------------------------

!  legal alter statement or error

! ----------------------------------------------------------------------

!  decode alter state

      alter_value_str: select case( alter_dir)

 !  alter delete

     case( delete_str) alter_value_str

         options% alter_state = alter_delete

!  alter blank

      case( blank_str) alter_value_str

         options% alter_state = alter_blank

!  alter shift1

      case( shift1_str) alter_value_str

         options% alter_state = alter_shift_1

!  alter shift0

      case( shift0_str) alter_value_str

         options% alter_state = alter_shift_0

!  alter shift3

      case( shift3_str) alter_value_str

         options% alter_state = alter_shift_3

!  unknown alter

      case default alter_value_str

         call msg_quit( "unknown alter directive: " // trim( alter_dir))

!  legal alter statement or error

      end select alter_value_str

   end if not_set

! ----------------------------------------------------------------------

!  process_alter_directive() exit

return

! **********************************************************************

!  process_alter_directive()

end subroutine process_alter_directive

! **********************************************************************
! **********************************************************************

!  process_form_directive() process form directives

subroutine process_form_directive( form_dir)

! **********************************************************************

!  process_form_directive() interface

! ----------------------------------------------------------------------

!  the wrap directive from the set file

character( len= *), intent( in) :: form_dir

! **********************************************************************

!  entry: wrap_dir is blank_compress_lower_case wrap directive
!         it must be a number string

!  exit: wrap_dir is processed or error exit

! **********************************************************************

!  process_form_directive() local

! ----------------------------------------------------------------------

!  count number of some statements to disallow more than one

   logical, save :: too_many_form_statements = .false.

!  check for characters after directive

   integer :: next_char

! **********************************************************************

!  process_form_directive() text

continue

! ----------------------------------------------------------------------

!  only one form statement per set file

   too_many_forms: if( too_many_form_statements )then

      call msg_quit( "too many form statements")

   else too_many_forms

      too_many_form_statements = .true.

   end if too_many_forms

! ----------------------------------------------------------------------

!  process form value if not already set on command line

   forms: if( form_dir( 1: len( free_str)) == free_str )then

!  check directive for correctness and use value from command line or default

      next_char = len( free_str) + 1

   else if( form_dir( 1: len( fixed_str)) == fixed_str )then forms

!  check directive for correctness and use fixed form

      options% free_form = .false.

      next_char = len( fixed_str) + 1

   else forms

!  directive not correct

      call msg_quit( "bad form: " // trim( form_dir))

   end if forms

!  error if characters after directive

   error_chars: if( form_dir( next_char: ) /= blank )then

      call msg_quit( "extra characters after form directive: " // trim( form_dir))

   end if error_chars

! ----------------------------------------------------------------------

!  process_form_directive() exit

return

! **********************************************************************

!  process_form_directive()

end subroutine process_form_directive

! **********************************************************************
! **********************************************************************

!  process_directory_directive() process include directory options

subroutine process_directory_directive( directory_dir)

! **********************************************************************

!  process_directory_directive() interface

! ----------------------------------------------------------------------

!  the directory directive from the set file

character( len= *), intent( in) :: directory_dir

! **********************************************************************

!  entry: directory_opt is a directory to be added to the list
!         of directories to be searched for inlcude files

!  exit: directory_opt is on the list

! **********************************************************************

!  process_directory_directive() local

! ----------------------------------------------------------------------

!  point to a directory type

   type( path_t), pointer :: directory_ptr

!  the name of a directory

   character( len= file_name_len) :: name_str

!  count length of quoted string

   integer :: directive_len

!  count length of unquoted string

   integer :: name_len

! **********************************************************************

!  process_directory_directive() text

continue

! ----------------------------------------------------------------------

!  unquote string to find path string

   call unquote_string( directory_dir, name_str, directive_len, name_len )

   badly_quoted: if( name_len == 0 )then

      call msg_quit( "null directory name: " // trim( directory_dir) )

   else if( directive_len > len_trim( directory_str) )then badly_quoted

      call msg_quit( "badly quoted directory name: " // trim( directory_dir) )

   end if badly_quoted

! ----------------------------------------------------------------------

!  if name is already on the path

   call seek_directory( name_str, directory_ptr)

   on_list_or_add: if( associated( directory_ptr) )then

      call msg_continue( "redundant include directory ignored: " // trim( directory_dir) )

!  if name is not already on the path

   else on_list_or_add

      call add_directory( name_str)

   end if on_list_or_add

! ----------------------------------------------------------------------

!  process_directory_directive() exit

return

! **********************************************************************

!  process_directory_directive()

end subroutine process_directory_directive

! **********************************************************************
! **********************************************************************

!  seek_directory() return a pointer to directory_str or null()

subroutine seek_directory( name_str, directory_ptr)

! **********************************************************************

!  seek_directory() interface

! ----------------------------------------------------------------------

!  the name of the directory to seek

character( len= *), intent( in) :: name_str

!  a pointer to the directory entry if found or null()

type( path_t), pointer :: directory_ptr

! **********************************************************************

!  entry: directory_str is a directory to be added to the list
!         of directories to be searched for inlcude files

!  exit: directory_str is on the list

! **********************************************************************

!  seek_directory() text

continue

! ----------------------------------------------------------------------

!  search from beginning to end of path list

   nullify( directory_ptr)

   directory_ptr => first_directory

!  if the name is already in the path

   scan_path: do

      if( .not. associated( directory_ptr) ) exit scan_path

      found_name: if( name_str == directory_ptr% name_str )then

         exit scan_path

      end if found_name

      directory_ptr => directory_ptr% next

   end do scan_path

! ----------------------------------------------------------------------

!  seek_directory() exit

return

! **********************************************************************

!  seek_directory()

end subroutine seek_directory

! **********************************************************************
! **********************************************************************

!  add_directory() return a pointer to directory_str or null()

subroutine add_directory( directory_str)

! **********************************************************************

!  add_directory() interface

! ----------------------------------------------------------------------

!  the name of the directory to add to the directory list

character( len= *), intent( in) :: directory_str

! **********************************************************************

!  entry: directory_str is a directory to be added to the list
!         of directories to be searched for inlcude files

!  exit: directory_str is on the list

! **********************************************************************

!  add_directory() local

! ----------------------------------------------------------------------

!  end of linked list, null() if no linked list yet

   type( path_t), save, pointer :: current_directory => null()

! **********************************************************************

!  add_directory() text

continue

! ----------------------------------------------------------------------

!  append to list

   start_or_append: if( associated( first_directory) )then

      allocate( current_directory% next, stat= state% status, errmsg= state% message)

      append_status: if( is_status_error( state) )then

         call msg_quit( "can't append to include path list: " // trim( directory_str) )

      end if append_status

      current_directory => current_directory% next

!  start list

   else start_or_append

      allocate( first_directory, stat= state% status, errmsg= state% message)

      start_status: if( is_status_error( state) )then

         call msg_quit( "can't start path list: " // trim( directory_str) )

      end if start_status

      current_directory => first_directory

   end if start_or_append

!  update new entry

   current_directory% name_str = directory_str

   current_directory% times_accessed = 0

   nullify( current_directory% next)

! ----------------------------------------------------------------------

!  add_directory() exit

return

! **********************************************************************

!  add_directory()

end subroutine add_directory

! **********************************************************************
! **********************************************************************

!  process_input_directive() process input file directives

subroutine process_input_directive( input_dir)

! **********************************************************************

!  process_input_directive() interface

! ----------------------------------------------------------------------

!  the input directive from the set file

character( len= *), intent( in) :: input_dir

! **********************************************************************

!  entry: input_dir is a file file to be listed as an input file

!  exit: input_dir is on the list

! **********************************************************************

!  process_input_directive() local

! ----------------------------------------------------------------------

!  the name of the file to be opened

   character( len= file_name_len) :: input_name

!  the length of the quoted string

   integer :: quoted_len

!  the length of the unquoted string

   integer :: unquoted_len

! **********************************************************************

!  process_input_directive() text

continue

! ----------------------------------------------------------------------

!  unquote string on directive

   call unquote_string( input_dir, input_name, unquoted_len, quoted_len)

   badly_quoted: if( quoted_len == 0 )then

      call msg_quit( "null name found on input directive: " // trim( input_dir) )

   else if( unquoted_len > len_trim( input_dir) )then badly_quoted

      call msg_quit( "badly quoted name found on input directive: " // trim( input_dir) )

   end if badly_quoted

! ----------------------------------------------------------------------

!  if input named on command line ignore the directive

   already_named: if( is_named( input_file) )then

      call msg_continue( "command line overrides set file, input directive ignored: " // trim( input_dir) )

!  if input not named on command line open the named file

   else already_named

      open_first: if( .not. is_connected( input_name_file) )then

         call input_name_file% open()

      end if open_first

      write( unit= input_name_file% io_unit, iostat= state% status, iomsg= state% message) input_name

      input_name_file% lines_transfered = input_name_file% lines_transfered + 1

   end if already_named

! ----------------------------------------------------------------------

!  process_input_directive() exit

return

! **********************************************************************

!  process_input_directive()

end subroutine process_input_directive

! **********************************************************************
! **********************************************************************

!  process_logfile_directive() process log file directive

subroutine process_logfile_directive( logfile_dir)

! **********************************************************************

!  process_logfile_directive() interface

! ----------------------------------------------------------------------

!  the logfile directive from the set file

character( len= *), intent( in) :: logfile_dir

! **********************************************************************

!  entry: logfile_opt is a file file to be opened as the log file

!  exit: logfile_opt is on the list

! **********************************************************************

!  process_logfile_directive() local

! ----------------------------------------------------------------------

!  the name of the file to be opened

   character( len= file_name_len) :: logfile_name

!  the length of the quoted string

   integer :: quoted_len

!  the length of the unquoted string

   integer :: unquoted_len

!  count number of some statements to disallow more than one

   logical, save :: too_many_logfile_statements = .false.

! **********************************************************************

!  process_logfile_directive() text

continue

! ----------------------------------------------------------------------

!  only one log file statement per set file

   too_many_logfiles: if( too_many_logfile_statements )then

      call msg_quit( "too many log file statements")

   else too_many_logfiles

      too_many_logfile_statements = .true.

   end if too_many_logfiles

!  unquote string on directive

   call unquote_string( logfile_dir, logfile_name, unquoted_len, quoted_len)

   badly_quoted: if( quoted_len == 0 )then

      call msg_quit( "null name found on log file directive: " // trim( logfile_dir))

   else if( unquoted_len > len_trim( logfile_dir) )then badly_quoted

      call msg_quit( "badly quoted name found on log file directive: " // trim( logfile_dir))

   end if badly_quoted

! ----------------------------------------------------------------------

!  if log file named on command line ignore the directive

   already_named: if( is_named( log_file) )then

      call msg_continue( "command line overrides set file, log file directive ignored: " // trim( logfile_dir) )

!  if log file not named on command line open the named file

   else already_named

      log_file% name_str = logfile_name

      call log_file% open()

   end if already_named

! ----------------------------------------------------------------------

!  process_logfile_directive() exit

return

! **********************************************************************

!  process_logfile_directive()

end subroutine process_logfile_directive

! **********************************************************************
! **********************************************************************

!  process_mark_directive() process mark directives

subroutine process_mark_directive( mark_dir)

! **********************************************************************

!  process_mark_directive() interface

! ----------------------------------------------------------------------

!  the mark directive from the set file

character( len= *), intent( in) :: mark_dir

! **********************************************************************

!  entry: mark_dir is blank_compress_lower_case mark directive
!         it must be "on" or "off"

!  exit: mark_dir is processed or error exit

! **********************************************************************

!  process_mark_directive() local

! ----------------------------------------------------------------------

!  count mark of some statements to disallow more than one

   logical, save :: too_many_mark_statements = .false.

! **********************************************************************

!  process_mark_directive() text

continue

! ----------------------------------------------------------------------

!  only one mark statement per set file

   too_many_mark: if( too_many_mark_statements )then

      call msg_quit( "too many mark statements")

   else too_many_mark

      too_many_mark_statements = .true.

   end if too_many_mark

! ----------------------------------------------------------------------

!  process mark switch

   on_off: if( mark_dir == on_str )then

      options% mark_input = .true.

   else if( mark_dir == off_str )then on_off

      options% mark_input = .false.

   else on_off

      call msg_quit( "unknown option on mark directive: " // trim( mark_dir) )

   end if on_off

! ----------------------------------------------------------------------

!  process_mark_directive() exit

return

! **********************************************************************

!  process_mark_directive()

end subroutine process_mark_directive

! **********************************************************************
! **********************************************************************

!  process_number_directive() process number directives

subroutine process_number_directive( number_dir)

! **********************************************************************

!  process_number_directive() interface

! ----------------------------------------------------------------------

!  the number directive from the set file

character( len= *), intent( in) :: number_dir

! **********************************************************************

!  entry: number_dir is blank_compress_lower_case number directive
!         it must be "on" or "off"

!  exit: number_dir is processed or error exit

! **********************************************************************

!  process_number_directive() local

! ----------------------------------------------------------------------

!  count number of some statements to disallow more than one

   logical, save :: too_many_number_statements = .false.

! **********************************************************************

!  process_number_directive() text

continue

! ----------------------------------------------------------------------

!  only one number statement per set file

   too_many_number: if( too_many_number_statements )then

      call msg_quit( "too many number statements")

   else too_many_number

      too_many_number_statements = .true.

   end if too_many_number

! ----------------------------------------------------------------------

!  process number switch

   on_off: if( number_dir == on_str )then

      options% number_source = .true.

   else if( number_dir == off_str )then on_off

      options% number_source = .false.

   else on_off

      call msg_quit( "unknown option on number directive: " // trim( number_dir) )

   end if on_off

! ----------------------------------------------------------------------

!  process_number_directive() exit

return

! **********************************************************************

!  process_number_directive()

end subroutine process_number_directive

! **********************************************************************
! **********************************************************************

!  process_sf_output_directive() process output directive

subroutine process_sf_output_directive( output_dir)

! **********************************************************************

!  process_sf_output_directive() interface

! ----------------------------------------------------------------------

!  the output directive from the set file

character( len= *), intent( in) :: output_dir

! **********************************************************************

!  entry: output_dir is a file file to be opened as the output file

!  exit: output_dir is set

! **********************************************************************

!  process_sf_output_directive() local

! ----------------------------------------------------------------------

!  the name of the file to be opened

   character( len= file_name_len) :: output_name

!  the length of the quoted string

   integer :: quoted_len

!  the length of the unquoted string

   integer :: unquoted_len

!  count number of some statements to disallow more than one

   logical, save :: too_many_output_statements = .false.

! **********************************************************************

!  process_sf_output_directive() text

continue

! ----------------------------------------------------------------------

!  only one output statement per set file

   too_many_outputs: if( too_many_output_statements )then

      call msg_quit( "too many output statements")

   else too_many_outputs

      too_many_output_statements = .true.

   end if too_many_outputs

!  unquote string on directive

   call unquote_string( output_dir, output_name, unquoted_len, quoted_len)

   badly_quoted: if( quoted_len == 0 )then

      call msg_quit( "null name found on output directive: " // trim( output_dir) )

   else if( unquoted_len > len_trim( output_dir) )then badly_quoted

      call msg_quit( "badly quoted name found on output directive: " // trim( output_dir) )

   end if badly_quoted

! ----------------------------------------------------------------------

!  if log file named on command line ignore the directive

   already_named: if( is_named( output_file) )then

      call msg_continue( "command line overrides set file, output directive ignored: " // trim( output_dir) )

!  if output file not named on command line open the named file

   else already_named

      output_file% name_str = output_name

   end if already_named

! ----------------------------------------------------------------------

!  process_sf_output_directive() exit

return

! **********************************************************************

!  process_sf_output_directive()

end subroutine process_sf_output_directive

! **********************************************************************
! **********************************************************************

!  process_post_directive() process post directives

subroutine process_post_directive( post_dir)

! **********************************************************************

!  process_post_directive() interface

! ----------------------------------------------------------------------

!  the post directive from the set file

character( len= *), intent( in) :: post_dir

! **********************************************************************

!  entry: post_dir is blank_compress_lower_case post directive
!         it must be "on" or "off"

!  exit: post_dir is processed or error exit

! **********************************************************************

!  process_post_directive() local

! ----------------------------------------------------------------------

!  count post of some statements to disallow more than one

   logical, save :: too_many_post_statements = .false.

! **********************************************************************

!  process_post_directive() text

continue

! ----------------------------------------------------------------------

!  only one post statement per set file

   too_many_post: if( too_many_post_statements )then

      call msg_quit( "too many post statements")

   else too_many_post

      too_many_post_statements = .true.

   end if too_many_post

! ----------------------------------------------------------------------

!  process post switch

   on_off: if( post_dir == on_str )then

      options% postpend_set_file = .true.

   else if( post_dir == off_str )then on_off

      options% postpend_set_file = .false.

   else on_off

      call msg_quit( "unknown option on post directive: " // trim( post_dir) )

   end if on_off

! ----------------------------------------------------------------------

!  process_post_directive() exit

return

! **********************************************************************

!  process_post_directive()

end subroutine process_post_directive

! **********************************************************************
! **********************************************************************

!  process_summary_directive() process report directives

subroutine process_summary_directive( report_dir)

! **********************************************************************

!  process_summary_directive() interface

! ----------------------------------------------------------------------

!  the srap directive from the set file

character( len= *), intent( in) :: report_dir

! **********************************************************************

!  entry: report_dir is blank_compress_lower_case report directive
!         it must be "on" or "off"

!  exit: report_dir is processed or error exit

! **********************************************************************

!  process_summary_directive() local

! ----------------------------------------------------------------------

!  count number of some statements to disallow more than one

   logical, save :: too_many_report_statements = .false.

! **********************************************************************

!  process_summary_directive() text

continue

! ----------------------------------------------------------------------

!  only one report statement per set file

   too_many_reports: if( too_many_report_statements )then

      call msg_quit( "too many report statements")

   else too_many_reports

      too_many_report_statements = .true.

   end if too_many_reports

! ----------------------------------------------------------------------

!  process report switch

   on_off: if( report_dir == on_str )then

      options% print_report = .true.

   else if( report_dir == off_str)then on_off

      options% print_report = .false.

   else on_off

      call msg_quit( "unknown option on report directive: " // trim( report_dir) )

   end if on_off

! ----------------------------------------------------------------------

!  process_summary_directive() exit

return

! **********************************************************************

!  process_summary_directive()

end subroutine process_summary_directive

! **********************************************************************
! **********************************************************************

!  process_verbose_directive() process verbose directives

subroutine process_verbose_directive( verbose_dir)

! **********************************************************************

!  process_verbose_directive() interface

! ----------------------------------------------------------------------

!  the verbose directive from the set file

character( len= *), intent( in) :: verbose_dir

! **********************************************************************

!  entry: verbose_dir is blank_compress_lower_case verbose directive
!         it must be "on" or "off"

!  exit: verbose_dir is processed or error exit

! **********************************************************************

!  process_verbose_directive() local

! ----------------------------------------------------------------------

!  count number of some statements to disallow more than one

   logical, save :: too_many_verbose_statements = .false.

! **********************************************************************

!  process_verbose_directive() text

continue

! ----------------------------------------------------------------------

!  only one verbose statement per set file

   too_many_verboses: if( too_many_verbose_statements )then

      call msg_quit( "too many verbose statements")

   else too_many_verboses

      too_many_verbose_statements = .true.

   end if too_many_verboses

! ----------------------------------------------------------------------

!  process verbose switch if not already set on command line

   on_off: if( verbose_dir == on_str )then

      options% verbose_mode = .true.

   else if( verbose_dir == off_str)then on_off

      options% verbose_mode = .false.

   else on_off

      call msg_quit( "unknown option on verbose directive: " // trim( verbose_dir) )

   end if on_off

! ----------------------------------------------------------------------

!  process_verbose_directive() exit

return

! **********************************************************************

!  process_verbose_directive()

end subroutine process_verbose_directive

! **********************************************************************
! **********************************************************************

!  process_wrap_directive() process wrap directives

subroutine process_wrap_directive( wrap_dir)

! **********************************************************************

!  process_wrap_directive() interface

! ----------------------------------------------------------------------

!  the wrap directive from the set file

character( len= *), intent( in) :: wrap_dir

! **********************************************************************

!  entry: wrap_dir is blank_compress_lower_case wrap directive
!         it must be "on" or "off"

!  exit: wrap_dir is processed or error exit

! **********************************************************************

!  process_wrap_directive() local

! ----------------------------------------------------------------------

!  count number of some statements to disallow more than one

   logical, save :: too_many_wrap_statements = .false.

! **********************************************************************

!  process_wrap_directive() text

continue

! ----------------------------------------------------------------------

!  only one wrap statement per set file

   too_many_wrap: if( too_many_wrap_statements )then

      call msg_quit( "too many wrap statements")

   else too_many_wrap

      too_many_wrap_statements = .true.

   end if too_many_wrap

! ----------------------------------------------------------------------

!  process wrap switch

   on_off: if( wrap_dir == on_str )then

      options% wrapping_lines = .true.

   else if( wrap_dir == off_str )then on_off

      options% wrapping_lines = .false.

   else on_off

      call msg_quit( "unknown option on wrap directive: " // trim( wrap_dir) )

   end if on_off

! ----------------------------------------------------------------------

!  process_wrap_directive() exit

return

! **********************************************************************

!  process_wrap_directive()

end subroutine process_wrap_directive

! **********************************************************************
! **********************************************************************

!  process_warning_directive() process wrap directives

subroutine process_warning_directive( warning_dir)

! **********************************************************************

!  process_warning_directive() interface

! ----------------------------------------------------------------------

!  the warning directive from the set file

character( len= *), intent( in) :: warning_dir

! **********************************************************************

!  entry: warning_dir is blank_compress_lower_case warning directive
!         it must be "on" or "off"

!  exit: warning_dir is processed or error exit

! **********************************************************************

!  process_warning_directive() local

! ----------------------------------------------------------------------

!  count number of some statements to disallow more than one

   logical, save :: too_many_warning_statements = .false.

! **********************************************************************

!  process_warning_directive() text

continue

! ----------------------------------------------------------------------

!  only one wrap statement per set file

   too_many_warn: if( too_many_warning_statements )then

      call msg_quit( "too many warning statements")

   else too_many_warn

      too_many_warning_statements = .true.

   end if too_many_warn

! ----------------------------------------------------------------------

!  process wrap switch

   on_off: if( warning_dir == on_str )then

      options% warning = .true.

   else if( warning_dir == off_str )then on_off

      options% warning = .false.

   else on_off

      call msg_quit( "unknown option on warning directive: " // trim( warning_dir) )

   end if on_off

! ----------------------------------------------------------------------

!  process_warning_directive() exit

return

! **********************************************************************

!  process_warning_directive()

end subroutine process_warning_directive

! **********************************************************************
! **********************************************************************

!  get_sf_symbol_name() extract symbol name and determine its length

subroutine get_sf_symbol_name( decl_stmt, symbol_name, name_len)

! **********************************************************************

!  get_sf_symbol_name() interface

! ----------------------------------------------------------------------

!  a declaration statement with a symbol name

character( len= *), intent( in) :: decl_stmt

!  the name of the symbol

character( len= *), intent( out) :: symbol_name

!  the length of the symbol name

integer, intent( out) :: name_len

! **********************************************************************

!  entry: decl_stmt is blank_compress_lower_case declaration statement past the double colon
!         "name" | "name=..."

!  exit: a valid symbol name and its length or error exit

! **********************************************************************

!  get_sf_symbol_name() constants

! ----------------------------------------------------------------------

!  characters which must end a symbol name

character( len= *), parameter :: end_of_name = equals // blank

! **********************************************************************

!  get_sf_symbol_name() local

! ----------------------------------------------------------------------

!  pointers to characters in decl_stmt

   integer :: char_idx

! **********************************************************************

!  get_sf_symbol_name() text

continue

! ----------------------------------------------------------------------

!  look for equals following separator

   char_idx = scan( decl_stmt, end_of_name)

   name_error: if( char_idx == 0 )then

      call msg_quit( "can't find name in set file declaration: " // trim( decl_stmt))

   end if name_error

!  length of name is one less than first character past name

   name_len = char_idx - 1

! ----------------------------------------------------------------------

!  check that name is a valid new name

   call valid_new_sf_name( decl_stmt( 1: name_len))

!  return name

   symbol_name = decl_stmt( 1: name_len)

! ----------------------------------------------------------------------

!  get_sf_symbol_name() exit

return

! **********************************************************************

!  get_sf_symbol_name()

end subroutine get_sf_symbol_name

! **********************************************************************
! **********************************************************************

!  process_sf_integer_declaration() process integer declarations

subroutine process_sf_integer_declaration( integer_stmt, is_const)

! **********************************************************************

!  process_sf_integer_declaration() interface

! ----------------------------------------------------------------------

!  the statement containing the declaration

character( len= *), intent( in) :: integer_stmt

!  true if declaration is of constants

logical, intent( in) :: is_const

! **********************************************************************

!  entry: int_stmt is blank_compress_lower_case integer declaration past the integer keyword
!         "::..." | ",parameter::..."

!  exit: int_stmt is processed or error exit

! **********************************************************************

!  process_sf_integer_declaration() constants

! ----------------------------------------------------------------------

!  mark the end of a definition

character( len= *), parameter :: end_of_def = comma // blank

! **********************************************************************

!  process_sf_integer_declaration() local

! ----------------------------------------------------------------------

!  string containing a single symbol declaration symbol

   character( len= buffer_len) :: symbol_str

!  name of symbol

   character( len= symbol_name_len) :: symbol_name

!  results of decoding statement

   integer :: symbol_len

! ----------------------------------------------------------------------

!  point to next character to be decoded

   integer :: next_char

   integer :: def_len

! **********************************************************************

!  process_sf_integer_declaration() text

continue

! ----------------------------------------------------------------------

   next_char = 1

!  extract all symbols on directive

   all_symbols: do

!  one symbol at a time to the symbol string

      def_len = scan( integer_stmt( next_char: ), end_of_def) + next_char - 1

      symbol_str = integer_stmt( next_char: def_len - 1)

!  extract symbol name

      call get_symbol_name( symbol_str, symbol_name, symbol_len)

!  check for valid new set file name

      call valid_new_sf_name( symbol_name( 1: symbol_len))

!  store symbol in symbol list

      call add_sf_integer( symbol_str( symbol_len + 1: def_len - 1), symbol_name, is_const)

!  comma separates symbols, blank is end of statement

      all_done: if( integer_stmt( def_len: def_len) == blank )then

         exit all_symbols

      end if all_done

!  move to next symbol

      next_char = def_len + 1

!  extract all symbols on directive

   end do all_symbols

! ----------------------------------------------------------------------

!  process_sf_integer_declaration() exit

return

! **********************************************************************

!  process_sf_integer_declaration()

end subroutine process_sf_integer_declaration

! **********************************************************************
! **********************************************************************

!  add_sf_integer() store integer declaration in symbol table

subroutine add_sf_integer( int_decl_str, integer_name, is_const)

! **********************************************************************

!  add_sf_integer() interface

! ----------------------------------------------------------------------

!  the statement containing the declaration

character( len= *), intent( in) :: int_decl_str

!  the symbol name

character( len= *), intent( in) :: integer_name

!  true if the symbol is a constant

logical, intent( in) :: is_const

! **********************************************************************

!  entry: int_decl_str is blank_compress_lower_case integer declaration statement past the name
!         "" | "=..."
!         sym_name is the symbol name
!         is_const is true if this is a constant declaration

!  exit: integer declaration is added to the integer symbol list or error exit

! **********************************************************************

!  add_sf_integer() local

! ----------------------------------------------------------------------

!  expression defining integer symbol

   character( len= buffer_len) :: expr_str

!  type pointer to allocate

   type( integer_t), pointer :: integer_ptr

! **********************************************************************

!  add_sf_integer() text

continue

! ----------------------------------------------------------------------

!  allocate new integer

   allocate( integer_ptr, stat= state% status, errmsg= state% message)

   alloc_error: if( is_status_error( state) )then

      call msg_quit( "can't allocate set file integer: " // trim( integer_name))

   end if alloc_error

!  build new integer on list

   subsequent_or_first: if( associated( first_sf_symbol) )then

      last_sf_symbol% next => integer_ptr

      last_sf_symbol => last_sf_symbol% next

   else subsequent_or_first

      first_sf_symbol => integer_ptr

      last_sf_symbol => first_sf_symbol

   end if subsequent_or_first

   nullify( integer_ptr% next)

! ----------------------------------------------------------------------

!  set symbol members

   integer_ptr% name_str = integer_name

   integer_ptr% declared_file = get_class_file_name( current_file)
   integer_ptr% declared_line = current_file% lines_transfered

! ----------------------------------------------------------------------

!  store whether integer is a constant

   integer_ptr% constant = is_const

!  determine if declaration specifies a value

   got_eq: if( len( int_decl_str) > 0 )then

      integer_ptr% defined = int_decl_str( 1: len( equals)) == equals

   else got_eq

      integer_ptr% defined = .false.

   end if got_eq

!  there must be a value in the set file to compute and store

   constant_value: if( .not. integer_ptr% defined )then

      call msg_quit( "a set file integer constant must have a value: " // trim( integer_name) // trim( int_decl_str) )

   end if constant_value

!  decode the value

   process_value: if( integer_ptr% defined )then

      all_constants = .true.

      expr_str = int_decl_str( len( equals) + 1: )

      call eval_int_expr( expr_str, integer_ptr% integer_value)

      non_const: if( integer_ptr% constant .and. .not. all_constants )then

         call msg_quit( "non constant expression used to define set file integer constant: " // trim( integer_name))

      end if non_const

      integer_ptr% defined_file = get_class_file_name( current_file)
      integer_ptr% defined_line = current_file% lines_transfered

   end if process_value

! ----------------------------------------------------------------------

!  add_sf_integer() exit

return

! **********************************************************************

!  add_sf_integer()

end subroutine add_sf_integer

! **********************************************************************
! **********************************************************************

!  process_sf_logical_declaration() process logical declarations

subroutine process_sf_logical_declaration( logical_stmt, is_const)

! **********************************************************************

!  process_sf_logical_declaration() interface

! ----------------------------------------------------------------------

!  the statement containing the logical declaration

character( len= *), intent( in) :: logical_stmt

!  true if declaration is of constants

logical, intent( in) :: is_const

! **********************************************************************

!  entry: logical_stmt is blank_compress_lower_case logical declaration past the logical keyword
!         "::..." | ",parameter::..."

!  exit: logical declaration is processed or error exit

! **********************************************************************

!  process_sf_logical_declaration() local

! ----------------------------------------------------------------------

!  string containing a single symbol declaration symbol

   character( len= buffer_len) :: symbol_str

!  name of symbol

   character( len= symbol_name_len) :: symbol_name

!  results of decoding statement

   integer :: symbol_len

! ----------------------------------------------------------------------

!  point to next character to be decoded

   integer :: next_char

   integer :: decl_len

! **********************************************************************

!  process_sf_logical_declaration() text

continue

   next_char = 1

! ----------------------------------------------------------------------

!  extract all symbols on directive

   all_symbols: do

!  one symbol at a time to the symbol string

      decl_len = scan( logical_stmt( next_char: ), end_of_decl) + next_char - 1

      symbol_str = logical_stmt( next_char: decl_len - 1)

!  extract symbol name

      call get_sf_symbol_name( symbol_str, symbol_name, symbol_len)

!  check for valid new set file name

      call valid_new_sf_name( symbol_name( 1: symbol_len))

!  store symbol in symbol list

      call add_sf_logical( symbol_str( symbol_len + 1: decl_len - 1), symbol_name, is_const)

!  comma separates symbols, blank is end of statement

      all_done: if( logical_stmt( decl_len: decl_len) == blank )then

         exit all_symbols

      end if all_done

!  reset for next symbol

      next_char = decl_len + 1

!  extract all symbols on directive

   end do all_symbols

! ----------------------------------------------------------------------

!  process_sf_logical_declaration() exit

return

! **********************************************************************

!  process_sf_logical_declaration()

end subroutine process_sf_logical_declaration

! **********************************************************************
! **********************************************************************

!  add_sf_logical() store logical declaration in symbol table

subroutine add_sf_logical( log_decl_str, logical_name, is_const)

! **********************************************************************

!  add_sf_logical() interface

! ----------------------------------------------------------------------

!  the statement containing the declaration

character( len= *), intent( in) :: log_decl_str

!  the valid logical name

character( len= *), intent( in) :: logical_name

!  true if the symbol is a constant

logical, intent( in) :: is_const

! **********************************************************************

!  entry: int_decl_str is blank_compress_lower_case logical declaration statement past the name
!         "" | "=..."
!         sym_name is the symbol name
!         is_const is true if this is a constant declaration

!  exit: logical declaration is added to the logical symbol list or error exit

! **********************************************************************

!  add_sf_logical() local

! ----------------------------------------------------------------------

!  expression defining logical symbol

   character( len= buffer_len) :: expr_str

!  type pointer to allocate

   type( logical_t), pointer :: logical_ptr

! **********************************************************************

!  add_sf_logical() text

continue

! ----------------------------------------------------------------------

!  allocate new logical

   allocate( logical_ptr, stat= state% status, errmsg= state% message)

   alloc_error: if( is_status_error( state) )then

      call msg_quit( "can't allocate set file logical: " // trim( logical_name))

   end if alloc_error

!  build new logical on list

   subsequent_or_first: if( associated( first_sf_symbol) )then

      last_sf_symbol% next => logical_ptr

      last_sf_symbol => last_sf_symbol% next

   else subsequent_or_first

      first_sf_symbol => logical_ptr

      last_sf_symbol => first_sf_symbol

   end if subsequent_or_first

   nullify( logical_ptr% next)

! ----------------------------------------------------------------------

!  set symbol members

   logical_ptr% name_str = logical_name

   logical_ptr% declared_file = get_class_file_name( current_file)
   logical_ptr% declared_line = current_file% lines_transfered

! ----------------------------------------------------------------------

!  store whether logical is a constant

   logical_ptr% constant = is_const

!  store whether symbol is declared in the set file

   logical_ptr% sf_defined = .false.

!  determine if declaration specifies a value

   got_eq: if( len( log_decl_str) > 0 )then

      logical_ptr% defined = log_decl_str( 1: len( equals)) == equals

   else got_eq

      logical_ptr% defined = .false.

   end if got_eq

!  there must be a value in the set file to compute and store

   constant_value: if( .not. logical_ptr% defined )then

      call msg_quit( "a set file logical constant must have a value: " &
                      // trim( logical_name) // trim( log_decl_str) )

   end if constant_value

!  decode the value

   process_value: if( logical_ptr% defined )then

      all_constants = .true.

      expr_str = log_decl_str( len( equals) + 1: )

      call eval_log_expr( expr_str, logical_ptr% logical_value)

      non_const: if( logical_ptr% constant .and. .not. all_constants )then

         call msg_quit( "non constant expression used to define logical constant: " // trim( logical_name))

      end if non_const

      logical_ptr% defined_file = get_class_file_name( current_file)
      logical_ptr% defined_line = current_file% lines_transfered

   end if process_value

! ----------------------------------------------------------------------

!  add_sf_logical() exit

return

! **********************************************************************

!  add_sf_logical()

end subroutine add_sf_logical

! **********************************************************************
! **********************************************************************

!  %%% read and write files, and process coco lines and statements

! **********************************************************************
! **********************************************************************

!  process_input_file() reads a coco source file, recurse upon include files

recursive subroutine process_input_file( file)

! **********************************************************************

!  process_input_file() interface

! ----------------------------------------------------------------------

!  the file to be processed

type( input_file_t), target, intent( in out) :: file

! **********************************************************************

!  entry: source file to be processed

!  exit: source file has been processed or error

! **********************************************************************

!  process_input_file() local

! ----------------------------------------------------------------------

!  statement buffer

   character( len= buffer_len) :: statement

!  signal complete statement

   logical :: complete

!  get if block nesting correct at file open and close

   type( if_t), pointer :: file_if_level

!  store to line macro

   character( len= conversion_len) :: line_str

! **********************************************************************

!  process_input_file() text

continue

!  the if level at the start of this file

   file_if_level => if_construct

!  open source file

   call file% open()

!  count files

   total% input_files = total% input_files + 1

!  set file predefined macro

   predefined_macros( file_ss)% macro_value = file% name_str

!  as if finished a complete statement at beginning of file

   complete = .true.

! ----------------------------------------------------------------------

!  main read lines loop

   read_lines: do

!  read from input file

      call file% read()

! ----------------------------------------------------------------------

!  read until end of file or complete statement

      read_eof: if( state% status < 0 )then

         call blank_compress_lower_case( statement, null_string)

         continuation_eof: if( .not. complete )then

            call msg_quit( "end of file encountered in a continued coco statement")

         end if continuation_eof

         exit read_lines

      end if read_eof

! ----------------------------------------------------------------------

!  update line predefined macro

      write( unit= line_str, fmt= conversion_fmt) file% lines_transfered

      predefined_macros( line_ss)% macro_value = adjustl( line_str)

!  process coco lines or source lines

      process_coco: if( line( 1: len( coco_key)) == coco_key )then

!  count lines

         total% coco_lines = total% coco_lines + 1

!  write coco line to the output

         call write_coco_line( output_file)

!  if line is not a coco comment

         coco_statement: if( is_coco_statement( line( len( coco_key) + 1: )) )then

!  read a complete statement

            call gather_coco_statement( line, statement, complete)

! ----------------------------------------------------------------------

!  process coco directives

! ----------------------------------------------------------------------

!  directive is a coco include directive

            process_directive: if( statement( 1: len( include_str)) == include_str )then

!  cannot continue an include line

               continue_include: if( .not. complete )then

                  call msg_quit( "include line cannot be continued")

               end if continue_include

!  process (possibly recursive) include directives (include 'a' --> include 'b' and so on)

               call process_include_directive( statement( len( include_str) + 1: ))

!  reset current file

               current_file => file

!  reset file predefined macro

               predefined_macros( file_ss)% macro_value = file% name_str

!  directive is a coco endfile directive

            else if( statement( 1: len( endfile_str)) == endfile_str )then process_directive

!  cannot continue an endfile line

               continue_endfile: if( .not. complete )then

                  call msg_quit( "endfile line cannot be continued")

               end if continue_endfile

               active_endfile: if( if_construct% now_selected )then

                  call process_endfile_directive( statement, file_if_level)

                  exit read_lines

               end if active_endfile

!  process any other directive

            else process_directive

!  if not yet a complete statement go get the rest of it

               get_statement: if( ( .not. complete) )then

                  cycle read_lines

               end if get_statement

!  process other (not possibly recursive) coco directives

               call process_coco_statement( statement)

            end if process_directive

!  if line is not a coco comment

         end if coco_statement

! ----------------------------------------------------------------------

!  process source lines

      else process_coco

!  error if a source line is mixed into a continued coco statement

         continuation_error: if( .not. complete )then

            call msg_quit( "source line encountered in a continued coco statement")

         end if continuation_error

!  if within the active block of a coco if construct

         active_source: if( if_construct% now_selected )then

!  if left key characters present, edit source line

            edit_line: if( index( string= line, substring= arg_key) > 0 )then

               call edit_source_line( line)

            end if edit_line

         end if active_source

!  copy source lines

         call write_source_line( output_file)

!  end processing coco lines

      end if process_coco

!  end main read lines loop

   end do read_lines

   total% input_lines = total% input_lines + file% lines_transfered

! ----------------------------------------------------------------------

!  no open if blocks

   no_blocks: if( .not. associated( if_construct, file_if_level) )then

      call msg_quit( "badly nested if-block at end of file: " // trim( file% name_str))

   end if no_blocks

   nullify( file_if_level)

!  end of file

   call file% close()

! ----------------------------------------------------------------------

!  process_input_file() exit

return

! **********************************************************************

!  process_input_file()

end subroutine process_input_file

! **********************************************************************
! **********************************************************************

!  gather_coco_statement() examine lines and signal a complete statement

subroutine gather_coco_statement( line, statement, complete)

! **********************************************************************

!  gather_coco_statement() interface

! ----------------------------------------------------------------------

!  the current input file

character( len= *), intent( in) :: line

!  the statement as it is built

character( len= *), intent( in out) :: statement

!  true when a complete statement has been seen

logical, intent( out) :: complete

! **********************************************************************

!  entry: statement is a line
!         "..."

!  exit: statement is accumulated, complete is true when whole

! **********************************************************************

!  gather_coco_statement() local

! ----------------------------------------------------------------------

!  count continuation lines

   integer, save :: continuation_lines = 0

! ----------------------------------------------------------------------

!  number of characters processed so far

   integer, save :: statement_len = 0

! **********************************************************************

!  gather_coco_statement() text

continue

! ----------------------------------------------------------------------

!  blank compress lower case

   call blank_compress_lower_case( statement, line( len( coco_key) + 1: ) )

!  if statement length hasn't changed and statement is not a comment

   null_stmt: if( statement_len == len_trim( statement) )then

      call msg_quit( "null statement encountered in continuation sequence")

   end if null_stmt

!  if not a complete statement yet

   statement_len = len_trim( statement)

!  last character is continuation means more to read to get a complete statement

   incomplete: if( statement( statement_len: statement_len) == continuation )then

!  if too many continuation lines

      too_many: if( continuation_lines > max_continuations )then

         call msg_quit( "too many continuations")

      end if too_many

!  count continuation lines

      continuation_lines = continuation_lines + 1

!  go get the rest of the statement

      complete = .false.

      return

   end if incomplete

!  coco statement is complete

   continuation_lines = 0

   statement_len = 0

   complete = .true.

! ----------------------------------------------------------------------

!  gather_coco_statement() exit

return

! **********************************************************************

!  gather_coco_statement()

end subroutine gather_coco_statement

! **********************************************************************
! **********************************************************************

!  is_coco_statement() true if coco statement is a coco construct and not a coco comment

logical function is_coco_statement( coco_stmt)

! **********************************************************************

!  is_coco_statement() interface

! ----------------------------------------------------------------------

!  the coco statement to be categorized

character( len= *), intent( in) :: coco_stmt

! **********************************************************************

!  entry: coco_stmt is coco statement past the coco key
!         "..."

!  exit: true if statement is a coco construct,
!        false if statement is a coco comment

! **********************************************************************

!  is_coco_statement() local

! ----------------------------------------------------------------------

!  locations of specific characters

   integer :: char_idx

! **********************************************************************

!  is_coco_statement() text

continue

! ----------------------------------------------------------------------

!  scan from first character (past coco key) for non-whitespace

   char_idx = verify( coco_stmt, white_space)

!  if found other than whitespace

   white_space_or_comment: if( char_idx > 0 )then

!  is construct if first character past the white space is not comment

      is_coco_statement = coco_stmt( char_idx: char_idx) /= comment

!  all whitespace

   else white_space_or_comment

!  is not a construct

      is_coco_statement = .false.

   end if white_space_or_comment

! ----------------------------------------------------------------------

!  is_coco_statement() exit

return

! **********************************************************************

!  is_coco_statement()

end function is_coco_statement

! **********************************************************************
! **********************************************************************

!  write_coco_line() write a coco line of output

subroutine write_coco_line( file)

! **********************************************************************

!  write_coco_line() interface

! ----------------------------------------------------------------------

!  the file to receive the output

type( output_file_t), intent( in out) :: file

! **********************************************************************

!  entry: out_unit is the logical unit connected to the output file
!         coco_line is a line to be written as per the current alter state

!  exit: line has been written or error exit

! **********************************************************************

!  write_coco_line() constants

! ----------------------------------------------------------------------

!  line prefix when alter state is shift 3

!  changed for NONMEM!

character( len= *), parameter :: shift3_prefix = ';?>'

! **********************************************************************

!  write_coco_line() local

! ----------------------------------------------------------------------

!  maximum line length

   character( len= buffer_len) :: line_buffer

! **********************************************************************

!  write_coco_line() text

continue

! ----------------------------------------------------------------------

!  write output as per alter state

   alter_print: select case( options% alter_state)

!  delete the line

   case( alter_delete) alter_print

      return

!  blank line

   case( alter_blank) alter_print

      write( unit= file% io_unit, fmt= string_fmt, iostat= state% status, iomsg= state% message) null_string

!  comment the line

   case( alter_shift_0) alter_print

      line_buffer = comment // file% line( len( comment) + 1: )

!  check whether to number the source line

      want_numbers_shift_0: if( options% number_source )then

!  if so, go number the atring

         call add_number_to_line( line_buffer)

      end if want_numbers_shift_0

      write( unit= file% io_unit, fmt= string_fmt, iostat= state% status, iomsg= state% message) trim( line_buffer)

!  shift one or comment the line

   case( alter_shift_1) alter_print

      line_buffer = comment // file% line( 1: len( file% line) - len( comment))

!  check whether to number the source line

      want_numbers_shift_1: if( options% number_source )then

!  if so, go number the atring

         call add_number_to_line( line_buffer)

      end if want_numbers_shift_1

      write( unit= file% io_unit, fmt= string_fmt, iostat= state% status, iomsg= state% message) trim( line_buffer)

!  shift three or comment the line

   case( alter_shift_3) alter_print

      line_buffer = shift3_prefix // file% line( 1: len( file% line) - len( shift3_prefix))

!  check whether to number the source line

      want_numbers_shift_3: if( options% number_source )then

!  if so, go number the atring

         call add_number_to_line( line_buffer)

      end if want_numbers_shift_3

      write( unit= file% io_unit, fmt= string_fmt, iostat= state% status, iomsg= state% message) trim( line_buffer)

   end select alter_print

! ----------------------------------------------------------------------

!  check write iostat

   write_this_file: if( state% status > 0 )then

      call msg_quit( "error writing source output: " // trim( file% name_str) )

   end if write_this_file

! ----------------------------------------------------------------------

!  write_coco_line() exit

return

! **********************************************************************

!  write_coco_line()

end subroutine write_coco_line

! **********************************************************************
! **********************************************************************

!  write_source_line() write a line of output

subroutine write_source_line( file)

! **********************************************************************

!  write_source_line() interface

! ----------------------------------------------------------------------

!  the file to receive the output

type( output_file_t), target, intent( in out) :: file

! **********************************************************************

!  entry: out_unit is the logical unit connected to the output file
!         source_line is the line of Fortran source to be written

!  exit: the line is written or error exit

! **********************************************************************

!  write_source_line() local

   type( output_file_t), pointer :: effective_file

! **********************************************************************

!  write_source_line() text

continue

! ----------------------------------------------------------------------

!  if currently printing output

   process_line: if( if_construct% now_selected )then

!  write to output or alternate

      got_alternate: if( associated( alternate_output) )then

         effective_file => alternate_output% file

         call write_coco_line( file)

      else got_alternate

         effective_file => file

      end if got_alternate

!  check whether to number the source line

      want_numbers: if( options% number_source )then

!  if so, go number the atring

         call add_number_to_line( effective_file% line)

      end if want_numbers

!  write source output

      write( unit= effective_file% io_unit, fmt= string_fmt, iostat= state% status, iomsg= state% message) &
             trim( effective_file% line)

!  check for write error

      write_error: if( is_status_error( state) )then

         call msg_quit( "error writing source output: " // trim( file% name_str) )

      end if write_error

!  count lines written

      effective_file% lines_transfered = effective_file% lines_transfered + 1

      total% selected_lines = total% selected_lines + 1

   else process_line

!  otherwise print as per the alter state

      call write_coco_line( file)

      total% elided_lines = total% elided_lines + 1

   end if process_line

! ----------------------------------------------------------------------

!  write_source_line() exit

return

! **********************************************************************

!  write_source_line()

end subroutine write_source_line

! **********************************************************************
! **********************************************************************

!  add_number_to_line() write a line of output

subroutine add_number_to_line( output_line)

! **********************************************************************

!  add_number_to_line() interface

! ----------------------------------------------------------------------

!  the file to receive the output

character( len= buffer_len), intent( in out) :: output_line

! **********************************************************************

!  entry: output_line is a line of output

!  exit: the line has numbers added or error exit

! **********************************************************************

!  add_number_to_line() constants

!  changed for NONMEM!

character( len= *), parameter :: name_prefix = '; '

character( len= *), parameter :: number_prefix = ': '

integer, parameter :: number_pad = 1

! **********************************************************************

!  add_number_to_line() local

! ----------------------------------------------------------------------

!  character line number

   character( len= conversion_len) :: number_str

!  where to put the line numbering

   integer, save :: number_len = card_image_len

!  format a label ! file-name:line-number

   character( len= file_name_len) :: label

! **********************************************************************

!  add_number_to_line() text

continue

! ----------------------------------------------------------------------

!  avoid touching current_file for lines not associated with an input file

   from_input: if( associated( current_file) )then

!  get the line number as a string

      write( unit= number_str, fmt= conversion_fmt) current_file% lines_transfered

!  prepare label

      label = name_prefix // trim( get_class_file_name( current_file)) // number_prefix // adjustl( number_str)

!  pick a length

      number_len = max( number_len, len_trim( output_line) + number_pad)

!  add ! file: number

      output_line = output_line( 1: number_len) // trim( label)

   end if from_input

! ----------------------------------------------------------------------

!  add_number_to_line() exit

return

! **********************************************************************

!  add_number_to_line()

end subroutine add_number_to_line

! **********************************************************************
! **********************************************************************

!  process_endfile_directive() process a coco endfile directive

subroutine process_endfile_directive( endfile_dir, if_ptr)

! **********************************************************************

!  process_endfile_directive() interface

! ----------------------------------------------------------------------

!  a statement containing an endfile directive

character( len= *), intent( in) :: endfile_dir

!  a pointer to the if block containing the first if block in this source

type( if_t), pointer :: if_ptr

! **********************************************************************

!  entry: endfile_dir is blank_compress_lower_case if directive

!  exit: the directive is processed or error exit

! **********************************************************************

!  process_endfile_directive() text

continue

! ----------------------------------------------------------------------

!  check for a well formed if directive

   endfile_extra_chars: if( endfile_dir( len( endfile_str) + 1: ) /= blank )then

      call msg_quit( "extra characters after endfile directive: " // trim( endfile_dir))

   end if endfile_extra_chars

! ----------------------------------------------------------------------

!  if this endfile is enclosed within selected lines

   active_lines: if( if_construct% enclosing% now_selected )then

!  reset the if-block state so the next input starts ok

      pop_ifs: do

         if( associated( if_construct, if_ptr) ) exit pop_ifs

          call delete_block()

      end do pop_ifs

   end if active_lines

! ----------------------------------------------------------------------

!  process_endfile_directive() exit

return

! **********************************************************************

!  process_endfile_directive()

end subroutine process_endfile_directive

! **********************************************************************
! **********************************************************************

!  process_include_directive() process an include directive

recursive subroutine process_include_directive( include_dir)

! **********************************************************************

!  process_include_directive() interface

! ----------------------------------------------------------------------

!  the include directive

character( len= *), intent( in) :: include_dir

! **********************************************************************

!  entry: inc_name is inlcude file name

!  exit: inc_name is inlcude file name with directory prepended

! **********************************************************************

!  process_include_directive() constants

! ----------------------------------------------------------------------

!  mark the beginning and end of include files as per the standard

character( len= *), parameter :: begin_inc = '??! include '

character( len= *), parameter :: end_inc = '??! end include '

! **********************************************************************

!  process_include_directive() local

! ----------------------------------------------------------------------

!  file variable of file named on the include directive

   type( input_file_t), target :: include_file

! ----------------------------------------------------------------------

!  length of quoted include file name

   integer :: construct_len

!  length of unquoted include file name

   integer :: name_len

!  whether named file is found

   logical :: found_file

! **********************************************************************

!  process_include_directive() text

continue

! ----------------------------------------------------------------------

!  check the include syntax: unquote the include file name

   call unquote_string( include_dir, include_file% name_str, construct_len, name_len )

   badly_quoted: if( name_len == 0 )then

      call msg_quit( "null include file name: " // trim( include_dir))

   else if( construct_len > len_trim( include_dir) )then badly_quoted

      call msg_quit( "badly quoted include file name: " // trim( include_dir))

   end if badly_quoted

! ----------------------------------------------------------------------

!  if active block, process include directive

   active_inc: if( if_construct% now_selected )then

!  see if the include file exists

      inquire( file= include_file% name_str, &
               exist= found_file, iostat= state% status, iomsg= state% message)

      inquire_error: if( is_status_error( state) )then

         call msg_quit( "can't inquire include file: " // trim( include_file% name_str) )

      end if inquire_error

!  if not found, check directories

      seek_inc: if( found_file )then

         count_include_in_dot = count_include_in_dot + 1

      else seek_inc

         call seek_include_file( include_file, found_file)

      end if seek_inc

!  if still not found, complain and quit

      no_name: if( .not. found_file )then

         call msg_quit( "can't find include file: " // trim( include_file% name_str) )

      end if no_name

! ----------------------------------------------------------------------

!  build include_file to pass to process_input_file()

! ----------------------------------------------------------------------

!  mark include file in output

      line = begin_inc // include_file% name_str

      call write_coco_line( output_file)

!  push dummy block on the if block list to catch badly nested if blocks

      call new_block( phase= include_block)

!  prepare to process include file

      total% include_files = total% include_files + 1

!  process include file

      call process_input_file( include_file)

!  push dummy block on the if block list to catch badly nested if blocks

      call delete_block( phase= include_block)

!  mark include file in output

      line = end_inc // include_file% name_str

      call write_coco_line( output_file)

! ----------------------------------------------------------------------

!  if active block, process include directive

   end if active_inc

!  end processing include statement

! ----------------------------------------------------------------------

!  process_include_directive() exit

return

! **********************************************************************

!  process_include_directive()

end subroutine process_include_directive

! **********************************************************************
! **********************************************************************

!  seek_include_file() seek inlcude file in directories

subroutine seek_include_file( include_file, found_file)

! **********************************************************************

!  seek_include_file() interface

! ----------------------------------------------------------------------

!  the include file name to be sought

type( input_file_t), intent( in out) :: include_file

logical, intent( out) :: found_file

! **********************************************************************

!  entry: include_file is inlcude file name

!  exit: include_file is inlcude file name with directory prepended

! **********************************************************************

!  seek_include_file() local

! ----------------------------------------------------------------------

!  pointer to directories on path

   type( path_t), pointer :: directory

!  construct path/names to check for existance

   character( len= file_name_len) :: trial_name

! **********************************************************************

!  seek_include_file() text

continue

! ----------------------------------------------------------------------

!  search list for directory/file

   nullify( directory)

   directory => first_directory

!  last directory in path is not associated

   search_path: do

      if( .not. associated( directory) ) exit search_path

!  construct full name <directory-name><file-name>

      trial_name = trim( directory% name_str) // include_file% name_str

!  check file existence

      inquire( file= trial_name, exist= found_file, iostat= state% status, iomsg= state% message)

      inquire_error: if( is_status_error( state) )then

         call msg_quit( "can't inquire include file: " // trim( trial_name))

      end if inquire_error

!  found file name

      name_match: if( found_file )then

!  found a file in this directory

         directory% times_accessed = directory% times_accessed + 1

!  rewrite include file name to include directory

         include_file% name_str = trial_name

         exit search_path

      end if name_match

!  file name not yet found so try next directory

      directory => directory% next

   end do search_path

! ----------------------------------------------------------------------

!  seek_include_file() exit

return

! **********************************************************************

!  seek_include_file()

end subroutine seek_include_file

! **********************************************************************
! **********************************************************************

!  %%% edit Fortran source lines

! **********************************************************************
! **********************************************************************

!  edit_source_line() edit source lines

subroutine edit_source_line( source_line)

! **********************************************************************

!  edit_source_line() interface

! ----------------------------------------------------------------------

!  source line to be edited

character( len= *), intent( in out) :: source_line

! **********************************************************************

!  entry: line is a line of Fortran source
!         with (possibly) ?file?, ?line?, ?date?, ?time?, ?integer?, ?logical?, ?macro?

!  exit: line has any ?macro? etc. strings replaced with their values

! **********************************************************************

!  edit_source_line() local

! ----------------------------------------------------------------------

!  copy of line since editing may expand the line beyond its length

   character( len= buffer_len) :: edit_line

!  make lower case copy of line

   character( len= buffer_len) :: lower_case_line

! **********************************************************************

!  edit_source_line() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  the line may be expanded by editing, so edit a long buffer

   edit_line = source_line

!  make lower case copy of line

   lower_case_line = to_lower( source_line)

! ----------------------------------------------------------------------

!  replace ?name? with the current string value of name

   call edit_macro_strings( edit_line, lower_case_line)

!  in case macros substituted upper case strings

   lower_case_line = to_lower( lower_case_line)

! ----------------------------------------------------------------------

!  process the predefined macros

   call edit_coco_strings( edit_line, lower_case_line)

! ----------------------------------------------------------------------

!  replace ?name? with the current integer or logical value of name

   call edit_integer_strings( edit_line, lower_case_line)

   call edit_logical_strings( edit_line, lower_case_line)

! ----------------------------------------------------------------------

!  process the file symbols

   call edit_file_symbol_strings( edit_line, lower_case_line)

! ----------------------------------------------------------------------

!  remove any line length overflow

   if( options% wrapping_lines ) call wrap_source_line( edit_line)

   source_line = edit_line

! ----------------------------------------------------------------------

!  edit_source_line() exit

return

! **********************************************************************

!  edit_source_line()

end subroutine edit_source_line

! **********************************************************************
! **********************************************************************

!  edit_coco_strings() process ?coco? strings

subroutine edit_coco_strings( edit_line, lower_case_line)

! **********************************************************************

!  edit_coco_strings() interface

! ----------------------------------------------------------------------

!  the source line to be edited

character( len= *), intent( in out) :: edit_line

!  the source line in lower case to enable searches

character( len= *), intent( in out) :: lower_case_line

! **********************************************************************

!  entry: line is a line of Fortran source with (possibly) ?integer

!  exit: line has any ?name strings replaced with their values

! **********************************************************************

!  edit_coco_strings() local

! ----------------------------------------------------------------------

!  make substring

   character( len= symbol_name_len) :: search_str

   integer :: search_len

!  find substring index

   integer :: search_idx

   integer :: close_paren_idx

   integer :: open_paren_idx

   integer :: eval_int

   character( len= buffer_len) :: eval_buf

!  loop index

   integer :: i

! **********************************************************************

!  edit_coco_strings() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  replace remaining predefined macros with their values

   edit_predefined: do i = 1, size( predefined_macros)

      search_str = arg_key // predefined_macros( i)% name_str // arg_key
      search_len = len_trim( search_str)

      search_idx = index( lower_case_line, search_str( 1: search_len))

      go_edit: do while( search_idx > 0 )

         if( i == eval_ss) then
            open_paren_idx = search_idx + get_target_len( predefined_macros( i)% name_str)
            call seek_close_paren( lower_case_line, open_paren_idx, close_paren_idx)

            call eval_int_expr(lower_case_line(open_paren_idx:close_paren_idx), eval_int)
            write( unit= eval_buf, fmt='(i0)') eval_int
            edit_line = edit_line( 1: search_idx-1) &
                        // trim(eval_buf) &
                        // edit_line(close_paren_idx+1: )
            lower_case_line = lower_case_line( 1: search_idx-1) &
                        // trim(eval_buf) &
                        // lower_case_line(close_paren_idx+1: )
         else
            call replace_substring( edit_line, lower_case_line, search_str( 1: search_len), &
                                    trim_value( predefined_macros( i)% macro_value), search_idx)
         end if

         predefined_macros( i)% referenced = .true.
         predefined_macros( i)% referenced_file = get_class_file_name( current_file)
         predefined_macros( i)% referenced_line = current_file% lines_transfered

         search_idx = index( lower_case_line, search_str( 1: search_len))

      end do go_edit

   end do edit_predefined

! ----------------------------------------------------------------------

!  edit_coco_strings() exit

return

! **********************************************************************

!  edit_coco_strings()

end subroutine edit_coco_strings

! **********************************************************************
! **********************************************************************

!  edit_integer_strings() process ?integer? strings

subroutine edit_integer_strings( edit_line, lower_case_line)

! **********************************************************************

!  edit_integer_strings() interface

! ----------------------------------------------------------------------

!  the source line to be edited

character( len= *), intent( in out) :: edit_line

!  the source line in lower case to enable searches

character( len= *), intent( in out) :: lower_case_line

! **********************************************************************

!  entry: line is a line of Fortran source with (possibly) ?integer?

!  exit: line has any ?name? strings replaced with their values

! **********************************************************************

!  edit_integer_strings() local

! ----------------------------------------------------------------------

!  string containing integer value

   character( len= conversion_len) :: value_str

!  point to integers on symbol list

   type( integer_t), pointer :: integer_ptr

!  point to search_str location in line

   integer :: search_idx

! **********************************************************************

!  edit_integer_strings() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  replace ?integer? with the current string value of integer

   nullify( integer_ptr)

!  test the occurance of each integer on symbol list

   each_integer: do

      call get_next_integer( integer_ptr)

      if( .not. associated( integer_ptr) ) exit each_integer

!  does ?integer? appear on line

      search_idx = index( lower_case_line, integer_ptr% name_str)

!  if found the target, try to replace it with its value

      go_integer: if( search_idx > 0 )then

!  if integer has a value

         no_value: if( .not. integer_ptr% defined )then

            call msg_quit( "edit integer symbol not defined: " // integer_ptr% name_str)

         end if no_value

         write( unit= value_str, fmt= conversion_fmt) integer_ptr% integer_value

         value_str = adjustl( value_str)

!  go replace the string with its value

         call replace_substring( edit_line, lower_case_line, trim_target( integer_ptr% name_str), trim( value_str), search_idx)

      end if go_integer

   end do each_integer

! ----------------------------------------------------------------------

!  edit_integer_strings() exit

return

! **********************************************************************

!  edit_integer_strings()

end subroutine edit_integer_strings

! **********************************************************************
! **********************************************************************

!  edit_logical_strings() process ?logical? strings

subroutine edit_logical_strings( edit_line, lower_case_line)

! **********************************************************************

!  edit_logical_strings() interface

! ----------------------------------------------------------------------

!  the source line to be edited

character( len= *), intent( in out) :: edit_line

!  the source line in lower case to enable searches

character( len= *), intent( in out) :: lower_case_line

! **********************************************************************

!  entry: line is a line of Fortran source with (possibly) ?logical

!  exit: line has any ?name strings replaced with their values

! **********************************************************************

!  edit_logical_strings() constants

character( len= *), parameter :: true_str = '.true.'
character( len= *), parameter :: false_str = '.false.'

! ----------------------------------------------------------------------

!  string containing logical value

! **********************************************************************

!  edit_logical_strings() local

! ----------------------------------------------------------------------

!  string containing logical value

   character( len= conversion_len) :: value_str

!  point to logicals on symbol list

   type( logical_t), pointer :: logical_ptr

!  point to search_str location in line

   integer :: search_idx

! **********************************************************************

!  edit_logical_strings() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  replace ?logical? with the current string value of logical

   nullify( logical_ptr)

   each_logical: do

      call get_next_logical( logical_ptr)

      if( .not. associated( logical_ptr) ) exit each_logical

!  does ?logical? appear on line

      search_idx = index( lower_case_line, logical_ptr% name_str)

      go_logical: if( search_idx > 0 )then

!  if logical has a value

         no_value: if( .not. logical_ptr% defined )then

            call msg_quit( "edit logical symbol not defined: " // logical_ptr% name_str)

         end if no_value

         decode: if( logical_ptr% logical_value )then

            value_str = true_str

         else decode

            value_str = false_str

         end if decode

         call replace_substring( edit_line, lower_case_line, trim_target( logical_ptr% name_str), trim( value_str), search_idx)

      end if go_logical

   end do each_logical

! ----------------------------------------------------------------------

!  edit_logical_strings() exit

return

! **********************************************************************

!  edit_logical_strings()

end subroutine edit_logical_strings

! **********************************************************************
! **********************************************************************

!  edit_file_symbol_strings() process ?file_symbol? strings

subroutine edit_file_symbol_strings( edit_line, lower_case_line)

! **********************************************************************

!  edit_file_symbol_strings() interface

! ----------------------------------------------------------------------

!  the source line to be edited

character( len= *), intent( in out) :: edit_line

!  the source line in lower case to enable searches

character( len= *), intent( in out) :: lower_case_line

! **********************************************************************

!  entry: line is a line of Fortran source with (possibly) ?file_symbol

!  exit: line has any ?name strings replaced with their values

! **********************************************************************

! **********************************************************************

!  edit_file_symbol_strings() local

! ----------------------------------------------------------------------

!  string containing file_symbol value

   character( len= buffer_len) :: value_str

!  point to file_symbols on symbol list

   type( file_symbol_t), pointer :: file_symbol_ptr

!  point to search_str location in line

   integer :: search_idx

! **********************************************************************

!  edit_file_symbol_strings() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  replace ?file_symbol? with the current string value of file_symbol

   nullify( file_symbol_ptr)

   each_file_symbol: do

      call get_next_file( file_symbol_ptr)

      if( .not. associated( file_symbol_ptr) ) exit each_file_symbol

!  does ?file? appear on line

      search_idx = index( lower_case_line, file_symbol_ptr% name_str)

      go_file_symbol: if( search_idx > 0 )then

!  if file_symbol has a value

         value_str = file_symbol_ptr% file% name_str

         call replace_substring( edit_line, lower_case_line, get_name_str( file_symbol_ptr% name_str), &
                                 trim( value_str), search_idx)

      end if go_file_symbol

   end do each_file_symbol

! ----------------------------------------------------------------------

!  edit_file_symbol_strings() exit

return

! **********************************************************************

!  edit_file_symbol_strings()

end subroutine edit_file_symbol_strings

! **********************************************************************
! **********************************************************************

!  edit_macro_strings() process ?macro? strings

subroutine edit_macro_strings( edit_line, lower_case_line)

! **********************************************************************

!  edit_macro_strings() interface

! ----------------------------------------------------------------------

!  the source line to be edited

character( len= *), intent( in out) :: edit_line

!  the source line in lower case to enable searches

character( len= *), intent( in out) :: lower_case_line

! **********************************************************************

!  entry: line is a line of Fortran source with (possibly) ?macro

!  exit: line has any ?macro strings replaced with their values

! **********************************************************************

!  edit_macro_strings() constants

! ----------------------------------------------------------------------

!  maximum number of passes before declaring a circular pattern exists

integer, parameter :: max_passes = 50

! **********************************************************************

!  edit_macro_strings() local

! ----------------------------------------------------------------------

!  point to ?macro?

   integer :: search_idx

!  argument strings

   character( len= buffer_len) :: value_str

!  scan for macros

   type( macro_t), pointer :: macro_ptr

!  end of substrings

   integer :: close_paren_idx

   integer :: open_paren_idx

   integer :: value_len

!  repeat editing until all macro symbols are substituted

   logical :: editing

   integer :: count_passes

! **********************************************************************

!  edit_macro_strings() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  make passes through the macro list until all occurrances are substituted

   count_passes = 0

   edit_all: do

      editing = .false.

! ----------------------------------------------------------------------

!  replace ?macro? with the current string value of macro

      nullify( macro_ptr)

      each_macro: do

         call get_next_macro( macro_ptr)

         end_of_list: if( .not. associated( macro_ptr) )then

            exit each_macro

         end if end_of_list

! ----------------------------------------------------------------------

!  does ?macro? appear on line

         search_idx = index( lower_case_line, macro_ptr% name_str)

! ----------------------------------------------------------------------

!  if macro appears on line

         found_macro: if( search_idx > 0 )then

            editing = .true.

            macro_ptr% referenced = .true.
            macro_ptr% referenced_file = get_class_file_name( current_file)
            macro_ptr% referenced_line = current_file% lines_transfered

!  macro definition has a dummy arg list

            have_arg_list: if( allocated( macro_ptr% dummy_args) )then

!  must rebuild the macro value with each new set of actual args

               next_dummy_args: do

                  if( search_idx == 0 ) exit next_dummy_args

!  check for actual arg list

                  open_paren_idx = search_idx + get_target_len( macro_ptr% name_str)

                  no_actual_args: if( edit_line( open_paren_idx: open_paren_idx) /= open_paren )then

                     call msg_quit( "macro args missing: " // macro_ptr% name_str)

                  end if no_actual_args

!  have an actual arg list, find the close parenthesis

                  call seek_close_paren( edit_line, open_paren_idx, close_paren_idx)

                  no_close_paren: if( close_paren_idx > len_trim( edit_line) )then

                     call msg_quit( "can't find close parenthesis on line to be edited: " // trim( edit_line))

                  end if no_close_paren

                  call make_actual_array( edit_line( open_paren_idx + 1: close_paren_idx - 1) // comma, &
                                          macro_ptr% actual_args, macro_ptr% args_in_parens)

!  build the new macro value

                  call process_actual_arglist( macro_ptr% actual_args, &
                                               value_str, macro_ptr% macro_value, macro_ptr% dummy_args)

!  substitute it

                  value_len = len_trim( value_str)

!  replace whole "?macro?(args)" with computed macro value

                  edit_line = edit_line( 1: search_idx - 1) // value_str( 1: value_len) // edit_line( close_paren_idx + 1: )

                  lower_case_line = lower_case_line( 1: search_idx - 1) // value_str( 1: value_len) &
                                 // lower_case_line( close_paren_idx + 1: )

!  find the next occurance of ?macro?

                  search_idx = index( lower_case_line, macro_ptr% name_str)

               end do next_dummy_args

! ----------------------------------------------------------------------

!  no arg list so macro value doesn't change

            else have_arg_list

!  insert macro into the line

               value_len = len_trim( macro_ptr% macro_value)

               call replace_substring( edit_line, lower_case_line, trim_target( macro_ptr% name_str), &
                                       macro_ptr% macro_value( 1: value_len), search_idx)

            end if have_arg_list

! ----------------------------------------------------------------------

!  done with this macro

         end if found_macro

! ----------------------------------------------------------------------

!  go try the next macro

      end do each_macro

!  when no macro is found

      if( .not. editing ) exit edit_all

      count_passes = count_passes + 1

      circular_editing: if( count_passes > max_passes )then

         call msg_quit( 'circular macro definitions found')

      end if circular_editing

   end do edit_all

! ----------------------------------------------------------------------

!  edit_macro_strings() exit

return

! **********************************************************************

!  edit_macro_strings()

end subroutine edit_macro_strings

! **********************************************************************
! **********************************************************************

!  make_actual_array() process actual arglist strings to actual arg array

subroutine make_actual_array( actual_args, actual_array, must_parens)

! **********************************************************************

!  make_actual_array() interface

! ----------------------------------------------------------------------

!  the actual args from the source line

character( len= *), intent( in) :: actual_args

!  the actual args one per element

character( len= *), dimension( :), intent( out) :: actual_array

!  true if must enclose actual arguments in parenthesis

logical, intent( in) :: must_parens

! **********************************************************************

!  entry: actual_args is a single string with the actual args

!  exit: actual array is the actual args one per array element

! **********************************************************************

!  make_actual_array() local

! ----------------------------------------------------------------------

!  index the actual args array

   integer :: i_arg

!  index characters

   integer :: i_char

!  mark the previous character

   integer :: prev_char

!  mark the close parenthesis

   integer :: close_paren_idx

!  mark the close braket

   integer :: close_braket_idx

! **********************************************************************

!  make_actual_array() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  scan through the actual arguments string to find a comma outside parenthesis

   i_char = 1
   prev_char = 1

   i_arg = 1

   each_char: do

!  find a comma outside parenthesis

      find_actual: select case( actual_args( i_char: i_char))

!  at open paren, skip to matching close paren

      case( open_paren) find_actual

         call seek_close_paren( actual_args, i_char, close_paren_idx)

         no_close_paren: if( close_paren_idx > len_trim( actual_args) )then

            call msg_quit( "can't find close parenthesis in actual argument list: " // trim( actual_args))

         end if no_close_paren

         i_char = close_paren_idx + 1

!  at open braket, skip to matching close braket

      case( open_braket) find_actual

         call seek_close_braket( actual_args, i_char, close_braket_idx)

         no_close_braket: if( close_braket_idx > len_trim( actual_args) )then

            call msg_quit( "can't find close braket in actual argument list: " // trim( actual_args))

         end if no_close_braket

         i_char = close_braket_idx + 1

!  actual argument is isolated before comma outside parenthesis

      case( comma) find_actual

         ensure_parens: if( must_parens )then

            call seek_close_paren( actual_args, prev_char, close_paren_idx)

            needs_parens: if( close_paren_idx == i_char - 1 )then

               actual_array( i_arg) = adjustl( actual_args( prev_char: i_char - 1))

            else needs_parens

               actual_array( i_arg) = open_paren // trim( adjustl( actual_args( prev_char: i_char - 1))) // close_paren

            end if needs_parens

         else ensure_parens

            actual_array( i_arg) = adjustl( actual_args( prev_char: i_char - 1))

         end if ensure_parens

         if( i_arg == size( actual_array) ) exit each_char

         i_arg = i_arg + 1

         i_char = i_char + 1
         prev_char = i_char

!  otherwise, keep checking characters

      case default find_actual

         i_char = i_char + 1

      end select find_actual

   end do each_char

! ----------------------------------------------------------------------

!  make_actual_array() exit

return

! **********************************************************************

!  make_actual_array()

end subroutine make_actual_array

! **********************************************************************
! **********************************************************************

!  process_actual_arglist() process macro actual arglist strings

subroutine process_actual_arglist( actual_args, value_str, macro_value_str, dummy_args)

! **********************************************************************

!  process_actual_arglist() interface

! ----------------------------------------------------------------------

!  the comma separated actual args from the macro instance

character( len= *), dimension( :), intent( in) :: actual_args

!  the value of the macro after editing

character( len= *), intent( out) :: value_str

!  the macro value

character( len= *), intent( in) :: macro_value_str

!  the macro dummy args

type( target_t), dimension( :), intent( in) :: dummy_args

! **********************************************************************

!  entry: arg_list is an actual argument list
!         macro is a macro variable

!  exit: value_buf has the macro's value with all dummy args replaced by actuals

! **********************************************************************

!  process_actual_arglist() local

! ----------------------------------------------------------------------

!  search lower case copy macro text with actual args

   character( len= len( value_str)) :: lc_value_str

! ----------------------------------------------------------------------

!  index argument loop

   integer :: i

!  character pointers

   integer :: dummy_idx

! **********************************************************************

!  process_actual_arglist() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  the value will be edited with the actual args

   value_str = macro_value_str
   lc_value_str = to_lower( value_str)

!  loop thru each dummy arg

   each_arg: do i = 1, size( dummy_args)

!  prepare the ?dummy? string

      dummy_idx = index( lc_value_str, dummy_args( i))

      substitute: if( dummy_idx > 0 )then

         call replace_substring( mixed_case_str= value_str, &
                                 lower_case_str= lc_value_str, search_str= trim_target( dummy_args( i)), &
                                 replace_str= trim( actual_args( i)), first_idx= dummy_idx)

      end if substitute

   end do each_arg

! ----------------------------------------------------------------------

!  process_actual_arglist() exit

return

! **********************************************************************

!  process_actual_arglist()

end subroutine process_actual_arglist

! **********************************************************************
! **********************************************************************

!  wrap_source_line() ensure lines are not too long

subroutine wrap_source_line( wrap_line)

! **********************************************************************

!  wrap_source_line() interface

! ----------------------------------------------------------------------

!  the line to be wrapped

character( len= *), target, intent( in out) :: wrap_line

! **********************************************************************

!  entry: line is a line of Fortran source with (possibly) more than 132 characters

!  exit: line has continuations written and fewer than 132 source lines

! **********************************************************************

!  wrap_source_line() constants

! ----------------------------------------------------------------------

!  start in the right column for fixed format

integer, parameter :: start_col = 5

!  fixed format start of line

character( len= *), parameter :: blank_str = repeat( blank, start_col)

!  fixed format start of line

integer, parameter :: no_comment = huge( 0)

! **********************************************************************

!  wrap_source_line() local

! ----------------------------------------------------------------------

!  length of source line

   integer :: output_len

!  index of comment

   integer :: comment_idx

! **********************************************************************

!  wrap_source_line() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  initialize

   output_len = len_trim( wrap_line)

   comment_idx = no_comment

! ----------------------------------------------------------------------

!  while line is too long

   wrap_lines: do

      if( output_len <= options% wrap_len ) exit wrap_lines

!  seek a comment prior to the wrap length

      call seek_comment_idx( wrap_line( 1: output_len), comment_idx)

      if( comment_idx <= options% wrap_len ) exit wrap_lines

!  process fixed format differently

      fix_length: if( options% free_form )then

!  free format line up to the wrap length, then the continuation character

         line = wrap_line( 1: options% wrap_len - len( continuation)) // continuation

!  the continuation character, then the rest of the line

         wrap_line = continuation // wrap_line( options% wrap_len: )

      else fix_length

         if( line( 1: 1) == 'c' .or. line( 1: 1) == 'C' .or. line( 1: 1) == '*' ) exit wrap_lines

!  fixed format line up to the breakpoint, then blanks to column 72, then continuation in column 73

         line = wrap_line( 1: options% wrap_len)

!  blanks up to column 6, then continuation in column 6, then the rest of the line

         wrap_line = blank_str // continuation // wrap_line( options% wrap_len + 1: )

      end if fix_length

!  write lines as they are made

      call write_source_line( output_file)

!  reset length to check for another wrap

      output_len = len_trim( wrap_line)

! ----------------------------------------------------------------------

!  while line is too long

   end do wrap_lines

! ----------------------------------------------------------------------

!  wrap_source_line() exit

return

! **********************************************************************

!  wrap_source_line()

end subroutine wrap_source_line

! **********************************************************************
! **********************************************************************

!  seek_comment_idx() ensure lines are not too long

subroutine seek_comment_idx( wrap_line, comment_idx)

! **********************************************************************

!  seek_comment_idx() interface

! ----------------------------------------------------------------------

!  the line to be wrapped

character( len= *), target, intent( in out) :: wrap_line

!  the index of a comment or unchanged

integer, intent( in out) :: comment_idx

! **********************************************************************

!  entry: line is a line of Fortran source with (possibly) more than 132 characters

!  exit: comment_idx indicates the start of an in-line comment or is unchanged

! **********************************************************************

!  seek_comment_idx() local

! ----------------------------------------------------------------------

!  length of source line

   integer :: line_len

!  loop through line

   integer :: char_idx

!  the quote that starts a character context

   character( len= 1), save :: quote

!  whether in character context or not

   logical, save :: char_context = .false.

! **********************************************************************

!  seek_comment_idx() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  initialize

   line_len = len( wrap_line)

! ----------------------------------------------------------------------

!  loop through line

   each_char: do char_idx = 1, line_len

      in_char_context: if( char_context )then

         found_the_quote: if( wrap_line( char_idx: char_idx) == quote )then

            char_context = .false.
            quote = blank

         end if found_the_quote

      else in_char_context

         found_comment: if( ( wrap_line( char_idx: char_idx) == comment) )then

            comment_idx = char_idx
            exit each_char

         end if found_comment

         found_a_quote: if( ( wrap_line( char_idx: char_idx) == single_quote) &
                       .or. ( wrap_line( char_idx: char_idx) == double_quote) )then

            quote = wrap_line( char_idx: char_idx)
            char_context = .true.

         end if found_a_quote

      end if in_char_context

   end do each_char

! ----------------------------------------------------------------------

!  seek_comment_idx() exit

return

! **********************************************************************

!  seek_comment_idx()

end subroutine seek_comment_idx

! **********************************************************************
! **********************************************************************

!  blank_compress_lower_case() blank compress or convert to lower case

subroutine blank_compress_lower_case( out_str, in_str)

! **********************************************************************

!  blank_compress_lower_case() interface

! ----------------------------------------------------------------------

!  a coco line with blanks, quoted strings and comments

character( len= *), intent( in) :: in_str

!  a blank compressed lower case coco statement

character( len= *), intent( out) :: out_str

! **********************************************************************

!  entry: in_str is a coco line

!  exit: out_str is a coco statement, possibly complete only up to hwm

! **********************************************************************

!  blank_compress_lower_case() local

! ----------------------------------------------------------------------

!  quote used for the current quoted string

   character( len= 1), save :: quote

!  length of in_str

   integer :: in_str_len

!  input pointer reset for each line

   integer :: in_idx

!  output pointer reset for each statement

   integer, save :: out_idx = 0

!  asis pointer follows output pointer

   integer, save :: asis_idx

!  character pointer reset by each intrinsic use

   integer :: char_idx

! **********************************************************************

!  blank_compress_lower_case() text

continue

! ----------------------------------------------------------------------

!  null input signals end-of-file reached on input file

   cleanup: if( in_str == null_string )then

      out_idx = 0

      return

   end if cleanup

! ----------------------------------------------------------------------

!  initialize line length

   in_str_len = len_trim( in_str)

!  setup pointers

   initialize: if( out_idx == 0 )then

      in_idx = 1

      out_idx = 1

      asis_idx = 1

      out_str = null_string

      asis_stmt = null_string

      quote = null_string

   else initialize

      char_idx = verify( in_str, white_space)

!  check whether first character is a continuation

      skip_contin: if( in_str( char_idx: char_idx) == continuation )then

         in_idx = char_idx + 1

      else skip_contin

         in_idx = char_idx

      end if skip_contin

   end if initialize

! **********************************************************************

!  scan each character until end of input string

   scan_line: do

      if( in_idx > in_str_len) exit scan_line

! ----------------------------------------------------------------------

!  if in quoted string

      char_literal: select case( quote)

! ----------------------------------------------------------------------

      case( single_quote, double_quote) char_literal

!  if found matching quote

         end_of_string: if( in_str( in_idx: in_idx) == quote )then

!  out of string so set quote to null

            quote = null_string

         end if end_of_string

!  copy the character

         out_str( out_idx: out_idx) = in_str( in_idx: in_idx)
         asis_stmt( asis_idx: asis_idx) = in_str( in_idx: in_idx)

!  update the pointers

         in_idx = in_idx + 1

         out_idx = out_idx + 1
         asis_idx = asis_idx + 1

! ----------------------------------------------------------------------

!  not in quoted string

      case default char_literal

!  white space is not copied

         skip_ws: select case( in_str( in_idx: in_idx) )

!  blanks or tabs

         case( blank, tab) skip_ws

            in_idx = in_idx + 1
            asis_idx = asis_idx + 1

!  all others

         case default skip_ws

!  check for special characters

            spec_char: select case( in_str( in_idx: in_idx) )

!  found quoted string

            case( single_quote, double_quote) spec_char

               quote = in_str( in_idx: in_idx)

!  found coco comment

            case( comment) spec_char

               exit scan_line

            end select spec_char

! ----------------------------------------------------------------------

!  copy non-blank characters to lower case

            out_str( out_idx: out_idx) = to_lower( in_str( in_idx: in_idx))
            asis_stmt( asis_idx: asis_idx) = in_str( in_idx: in_idx)

!  update pointers

            in_idx = in_idx + 1

            out_idx = out_idx + 1
            asis_idx = asis_idx + 1

         end select skip_ws

! ----------------------------------------------------------------------

      end select char_literal

!  process next character

   end do scan_line

! ----------------------------------------------------------------------

!  check whether last character is continuation

   line_complete: if( out_str( out_idx - 1: out_idx - 1) == continuation )then

!  next line is a continuation line

      out_idx = out_idx - 1
      asis_idx = asis_idx - 1

   else line_complete

!  next line is an initial line

      asis_len = asis_idx
      out_idx = 0

   end if line_complete

! ----------------------------------------------------------------------

!  blank_compress_lower_case() exit

return

! **********************************************************************

!  blank_compress_lower_case()

end subroutine blank_compress_lower_case

! **********************************************************************
! **********************************************************************

!  process_coco_statement() process a coco directive

subroutine process_coco_statement( coco_stmt)

! **********************************************************************

!  process_coco_statement() interface

! ----------------------------------------------------------------------

!  the coco statement to be processed

character( len= *), intent( in) :: coco_stmt

! **********************************************************************

!  entry: coco_stmt is a blank_compress_lower_case coco directive past the coco key
!         "stop..." | "message..." | "if..." | "else if..." | "else..." |
!         "end if..." | "integer..." | "logical..." | "assert..." | "<name>=..." |
!         "text..." | "copy..." | "symbols" | "options" | "report" |

!  exit: the directive is processed or error exit

!  If a directive might have something after the keyword, the keyword
!  match is checked by "keyword( : len( keyword) ) == string", otheriwse,
!  if the directive must not have anything after the keyword, the
!  keyword match is checked by "keyword == string".  Thus, a directive
!  with unknown nonblank characters after the keyword is an unknown directive.

! **********************************************************************

!  process_coco_statement() local

! ----------------------------------------------------------------------

!  point to location of symbol on symbol list

   class( symbol_t), pointer :: symbol_ptr

   type( integer_t), pointer :: integer_ptr
   type( logical_t), pointer :: logical_ptr

!  possible index of equals

   integer :: eq_idx

!  expression string is after the equals

   integer :: expr_idx

! **********************************************************************

!  process_coco_statement() text

continue

! ----------------------------------------------------------------------

!  detect assignment statements assigning to named variables

   nullify( symbol_ptr)

   eq_idx = scan( coco_stmt( 1: symbol_name_len + len( equals)), equals)

   got_equals: if( eq_idx > 0 )then

      call seek_symbol_name( coco_stmt( 1: eq_idx - 1), symbol_ptr)

   end if got_equals

! ----------------------------------------------------------------------

!  which directive?

! ----------------------------------------------------------------------

!  assignment directive

   which_directive: if( associated( symbol_ptr) )then

!  up to the equals must be a declared name

      expr_idx = eq_idx + len( equals)

!  must be an integer or logical variable

      integer_or_logical_or_error: select type( symbol_ptr)

      type is( integer_t) integer_or_logical_or_error

         integer_ptr => symbol_ptr

         call process_integer_assignment( coco_stmt( expr_idx: ), integer_ptr)

      type is( logical_t) integer_or_logical_or_error

         logical_ptr => symbol_ptr

         call process_logical_assignment( coco_stmt( expr_idx: ), logical_ptr)

      class default integer_or_logical_or_error

         call msg_quit( "target of assignment must be type integer or logical: " // symbol_ptr% name_str)

      end select integer_or_logical_or_error

! ----------------------------------------------------------------------

!  stop directive

   else if( coco_stmt == stop_str )then which_directive

      call process_stop_directive( coco_stmt( len( stop_str) + 1: ) )

! ----------------------------------------------------------------------

!  message directive

   else if( coco_stmt( 1: len( message_str)) == message_str )then which_directive

      call process_message_directive( coco_stmt( len( message_str) + 1: ) )

! ----------------------------------------------------------------------

!  if directive

   else if( coco_stmt( 1: len( if_str)) == if_str )then which_directive

      call process_if_directive( coco_stmt( len( if_str) + 1: ) )

! ----------------------------------------------------------------------

!  else if directive

   else if( coco_stmt( 1: len( elseif_str)) == elseif_str )then which_directive

      call process_elseif_directive( coco_stmt( len( elseif_str) + 1: ) )

! ----------------------------------------------------------------------

!  else directive

   else if( coco_stmt( 1: len( else_str)) == else_str )then which_directive

      call process_else_directive( coco_stmt( len( else_str) + 1: ) )

! ----------------------------------------------------------------------

!  endif directive

   else if( coco_stmt( 1: len( endif_str)) == endif_str )then which_directive

      call process_endif_directive( coco_stmt( len( endif_str) + 1: ) )

! ----------------------------------------------------------------------

!  integer declaration

   else if( coco_stmt( 1: len( integer_str)) == integer_str )then which_directive

      call process_integer_declaration( coco_stmt( len( integer_str) + 1: ), .false.)

! ----------------------------------------------------------------------

!  integer constant declaration

   else if( coco_stmt( 1: len( integer_constant_str)) == integer_constant_str )then which_directive

      call process_integer_declaration( coco_stmt( len( integer_constant_str) + 1: ), .true.)

! ----------------------------------------------------------------------

!  logical declaration

   else if( coco_stmt( 1: len( logical_str)) == logical_str )then which_directive

      call process_logical_declaration( coco_stmt( len( logical_str) + 1: ), .false.)

! ----------------------------------------------------------------------

!  logical constant declaration

   else if( coco_stmt( 1: len( logical_constant_str)) == logical_constant_str )then which_directive

      call process_logical_declaration( coco_stmt( len( logical_constant_str) + 1: ), .true.)

! ----------------------------------------------------------------------

!  macro declaration

   else if( coco_stmt( 1: len( macro_str)) == macro_str )then which_directive

      call process_macro_declaration( coco_stmt( len( macro_str) + 1: ), .false.)

! ----------------------------------------------------------------------

!  macro, parens declaration

   else if( coco_stmt( 1: len( macro_parens_str)) == macro_parens_str )then which_directive

      call process_macro_declaration( coco_stmt( len( macro_parens_str) + 1: ), .true.)

! ----------------------------------------------------------------------

!  getenv declaration

   else if( coco_stmt( 1: len( getenv_str)) == getenv_str )then which_directive

      call process_getenv_declaration( coco_stmt( len( getenv_str) + 1: ))

! ----------------------------------------------------------------------

!  assert directive

   else if( coco_stmt( 1: len( assert_str)) == assert_str )then which_directive

      call process_assert_directive( coco_stmt( len( assert_str) + 1: ))

! ----------------------------------------------------------------------

!  cmdline directive

   else if( coco_stmt( 1: len( cmdline_str)) == cmdline_str )then which_directive

      call process_cmdline_directive( coco_stmt( len( cmdline_str) + 1: ))

! ----------------------------------------------------------------------

!  document directive

   else if( coco_stmt( 1: len( document_str)) == document_str )then which_directive

      call process_document_directive( coco_stmt( len( document_str) + 1: ))

! ----------------------------------------------------------------------

!  freeze directive

   else if( coco_stmt( 1: len( freeze_str)) == freeze_str )then which_directive

      call process_freeze_directive( coco_stmt( len( freeze_str) + 1: ))

! ----------------------------------------------------------------------

!  options directive

   else if( coco_stmt( 1: len( options_str)) == options_str )then which_directive

      call process_options_directive( coco_stmt( len( options_str) + 1: ))

! ----------------------------------------------------------------------

!  report directive

   else if( coco_stmt( 1: len( report_str)) == report_str )then which_directive

      call process_report_directive( coco_stmt( len( report_str) + 1: ))

! ----------------------------------------------------------------------

!  symbols directive

   else if( coco_stmt( 1: len( symbols_str)) == symbols_str )then which_directive

      call process_symbols_directive( coco_stmt( len( symbols_str) + 1: ))

! ----------------------------------------------------------------------

!  text directive

   else if( coco_stmt( 1: len( text_str)) == text_str )then which_directive

      call process_text_directive( coco_stmt( len( text_str) + 1: ), .false.)

! ----------------------------------------------------------------------

!  text, parens directive

   else if( coco_stmt( 1: len( text_parens_str)) == text_parens_str )then which_directive

      call process_text_directive( coco_stmt( len( text_parens_str) + 1: ), .true.)

! ----------------------------------------------------------------------

!  copy directive

   else if( coco_stmt( 1: len( copy_str)) == copy_str )then which_directive

      call process_copy_directive( coco_stmt( len( copy_str) + 1: ))

! ----------------------------------------------------------------------

!  open directive

   else if( coco_stmt( 1: len( open_str)) == open_str )then which_directive

      call process_open_directive( coco_stmt( len( open_str) + 1: ))

! ----------------------------------------------------------------------

!  output directive

   else if( coco_stmt( 1: len( output_str)) == output_str )then which_directive

      call process_output_directive( coco_stmt( len( output_str) + 1: ))

! ----------------------------------------------------------------------

!  cannot process this directive

   else which_directive

      call msg_quit( "unknown coco directive: " // trim( coco_stmt))

! ----------------------------------------------------------------------

!  which directive?

   end if which_directive

! ----------------------------------------------------------------------

!  process_coco_statement() exit

return

! **********************************************************************

!  process_coco_statement()

end subroutine process_coco_statement

! **********************************************************************
! **********************************************************************

!  process_integer_assignment() process a coco stop directive

subroutine process_integer_assignment( assign_dir, integer_ptr)

! **********************************************************************

!  process_integer_assignment() interface

! ----------------------------------------------------------------------

!  the integer assignment directive

character( len= *), intent( in) :: assign_dir

!  a pointer to the integer symbol

type( integer_t), pointer :: integer_ptr

! **********************************************************************

!  entry: stop_dir is blank_compress_lower_case coco stop directive, past the coco key word

!  exit: coco processing stops

! **********************************************************************

!  process_integer_assignment() local

! **********************************************************************

!  process_integer_assignment() text

continue

! ----------------------------------------------------------------------

!  process assignment directive if on an active line

   active_line: if( if_construct% now_selected ) then

!  do not allow redefinition of constants

      redefine_constant: if( integer_ptr% constant )then

         call msg_quit( "attempt to redefine a constant: " // integer_ptr% name_str)

      end if redefine_constant

!  assign the value

      call eval_int_expr( assign_dir, integer_ptr% integer_value)

      integer_ptr% defined = .true.

      integer_ptr% defined_file = get_class_file_name( current_file)
      integer_ptr% defined_line = current_file% lines_transfered

   end if active_line

! ----------------------------------------------------------------------

!  process_integer_assignment() exit

return

! **********************************************************************

!  process_integer_assignment()

end subroutine process_integer_assignment

! **********************************************************************
! **********************************************************************

!  process_logical_assignment() process a coco stop directive

subroutine process_logical_assignment( assign_dir, logical_ptr)

! **********************************************************************

!  process_logical_assignment() interface

! ----------------------------------------------------------------------

!  the logical assignment directive

character( len= *), intent( in) :: assign_dir

!  a pointer to the logical symbol

type( logical_t), pointer :: logical_ptr

! **********************************************************************

!  entry: stop_dir is blank_compress_lower_case coco stop directive, past the coco key word

!  exit: coco processing stops

! **********************************************************************

!  process_logical_assignment() local

! **********************************************************************

!  process_logical_assignment() text

continue

! ----------------------------------------------------------------------

!  process stop directive if on an active line

   active_line: if( if_construct% now_selected )then

!  do not allow redefinition of constants

      redefine_constant: if( logical_ptr% constant )then

         call msg_quit( "attempt to redefine a constant: " // logical_ptr% name_str)

      end if redefine_constant

!  assign the value

      call eval_log_expr( assign_dir, logical_ptr% logical_value)

      logical_ptr% defined = .true.

      logical_ptr% defined_file = get_class_file_name( current_file)
      logical_ptr% defined_line = current_file% lines_transfered

   end if active_line

! ----------------------------------------------------------------------

!  process_logical_assignment() exit

return

! **********************************************************************

!  process_logical_assignment()

end subroutine process_logical_assignment

! **********************************************************************
! **********************************************************************

!  process_stop_directive() process a coco stop directive

subroutine process_stop_directive( stop_dir)

! **********************************************************************

!  process_stop_directive() interface

! ----------------------------------------------------------------------

!  the stop directive

character( len= *), intent( in) :: stop_dir

! **********************************************************************

!  entry: stop_dir is blank_compress_lower_case coco stop directive, past the coco key word

!  exit: coco processing stops

! **********************************************************************

!  process_stop_directive() text

continue

! ----------------------------------------------------------------------

!  process stop directive if on an active line

   active_line: if( if_construct% now_selected )then

      verbose_output: if( options% verbose_mode )then

         call msg_continue( "coco stop directive encountered: " // trim( stop_dir))

      end if verbose_output

      output_file% line = stop_dir

      call write_coco_line( output_file)

      stop 'coco stop directive encountered'

   end if active_line

! ----------------------------------------------------------------------

!  process_stop_directive() exit

return

! **********************************************************************

!  process_stop_directive()

end subroutine process_stop_directive

! **********************************************************************
! **********************************************************************

!  process_message_directive() process a coco message directive

subroutine process_message_directive( message_dir)

! **********************************************************************

!  process_message_directive() interface

! ----------------------------------------------------------------------

!  the message directive

character( len= *), intent( in) :: message_dir

! **********************************************************************

!  entry: message_dir is blank_compress_lower_case coco message directive, past the message key word

!  exit: message is written to error unit

! **********************************************************************

!  process_message_directive() constants

character( len= *), parameter :: plus = '+'
character( len= *), parameter :: minus = '-'

! **********************************************************************

!  process_message_directive() local

! ----------------------------------------------------------------------

   character( len= buffer_len) :: msg_buffer

   integer :: in_idx

   integer :: out_idx

   integer :: comma_idx

   integer :: quoted_len

   integer :: unquoted_len

   integer :: integer_value

   logical :: logical_value

   logical :: is_integer

   character( len= buffer_len) :: expr_str

   character( len= conversion_len) :: conversion_str

! **********************************************************************

!  process_message_directive() text

continue

! ----------------------------------------------------------------------

!  process if on active line

   active_line: if( if_construct% now_selected )then

! ----------------------------------------------------------------------

!  initialize

      in_idx = 1
      out_idx = 1

      msg_buffer = blank

!  loop thru message list items

      list_items: do

         if( in_idx > len_trim( message_dir) ) exit list_items

! ----------------------------------------------------------------------

!  a list item can be a quoted string or an expression

         string_expr: select case( message_dir( in_idx: in_idx) )

! ----------------------------------------------------------------------

!  process quoted strings

         case( single_quote, double_quote) string_expr

!  try to unquote the string

            call unquote_string( message_dir( in_idx: ), msg_buffer( out_idx: ), quoted_len, unquoted_len)

!  if the matching quote is found within the string

            got_string: if( quoted_len <= len( message_dir( in_idx: )) )then

!  found quote, update the character pointers

               in_idx = in_idx + quoted_len + index( message_dir( in_idx + quoted_len - 1: ), comma) - 1

               out_idx = out_idx + unquoted_len - 1

!  found not quote, complain and quit

            else got_string

               call msg_quit( "bad message string: " // trim( message_dir) )

            end if got_string

! ----------------------------------------------------------------------

!  process expressions

         case( 'a': 'z', '0': '9', dot, plus, minus, open_paren) string_expr

!  expression ends at a comma

            comma_idx = scan( message_dir( in_idx: ), comma)

!  find the comma or the end of the expression

            end_of_string: if( comma_idx == 0 )then

               comma_idx = len_trim( message_dir) + 1

            else end_of_string

               comma_idx = in_idx + comma_idx - 1

            end if end_of_string

!  encode integer or logical

            call integer_or_logical( message_dir( in_idx: comma_idx - 1), is_integer)

!  an integer expression

            int_log: if( is_integer )then

               expr_str = message_dir( in_idx: comma_idx - 1)

               call eval_int_expr( expr_str, integer_value)

               write( unit= conversion_str, fmt= conversion_fmt, iostat= state% status, iomsg= state% message) integer_value

!  trap internal write errors

               encode: if( is_status_error( state) )then

                  call msg_quit( "can't encode: " // message_dir( in_idx: comma_idx - 1) )

               end if encode

               msg_buffer( out_idx: ) = adjustl( conversion_str)

!  a logical expression

            else int_log

               expr_str = message_dir( in_idx: comma_idx - 1)

               call eval_log_expr( expr_str, logical_value)

               t_or_f: if( logical_value )then

                  msg_buffer( out_idx: ) = '.true.'

               else t_or_f

                  msg_buffer( out_idx: ) = '.false.'

               end if t_or_f

            end if int_log

!  update pointers and add to output buffer

            adjust: if( msg_buffer( out_idx: out_idx) == blank )then

               msg_buffer( out_idx + 1: ) = adjustl( msg_buffer( out_idx + 1: ) )

            end if adjust

            in_idx = comma_idx + 1

            out_idx = len_trim( msg_buffer) + 2

! ----------------------------------------------------------------------

!  list item isn't a string, a symbol or a literal

         case default string_expr

            call msg_quit( "bad message list item: " // message_dir( in_idx: ) )

! ----------------------------------------------------------------------

         end select string_expr

!  loop thru message list items

      end do list_items

! ----------------------------------------------------------------------

!  make the message available

      verbose_output: if( options% verbose_mode )then

         call msg_continue( msg_buffer( 1: out_idx) )

      end if verbose_output

!  copy for the log file

      write( unit= log_file% io_unit, fmt= string_fmt) 'coco message: ' // msg_buffer( 1: out_idx)

!  copy for the output file

      if( options% wrapping_lines ) call wrap_source_line( msg_buffer( 1: out_idx))

!  add a leading blank to avoid shift 0 from deleting a character

      line = blank // msg_buffer( 1: len( line) - 1)

      call write_coco_line( output_file)

   end if active_line

! ----------------------------------------------------------------------

!  process_message_directive() exit

return

! **********************************************************************

!  process_message_directive()

end subroutine process_message_directive

! **********************************************************************
! **********************************************************************

!  process_freeze_directive() process a coco freeze directive

subroutine process_freeze_directive( freeze_dir)

! **********************************************************************

!  process_freeze_directive() interface

! ----------------------------------------------------------------------

!  the stop directive

character( len= *), intent( in) :: freeze_dir

! **********************************************************************

!  entry: freeze_dir is blank_compress_lower_case coco freeze directive, past the coco key word

!  exit: freeze file is written

! **********************************************************************

!  process_freeze_directive() local

   character( len= file_name_len) :: file_name

   integer :: unquoted_len, quoted_len

! **********************************************************************

!  process_freeze_directive() text

continue

! ----------------------------------------------------------------------

!  process freeze directive if on an active line

   active_line: if( if_construct% now_selected )then

      verbose_output: if( options% verbose_mode )then

         call msg_continue( "coco freeze directive encountered: " // trim( freeze_dir))

      end if verbose_output

      have_filename: if( freeze_dir( 1: 1) == single_quote .or. freeze_dir( 1: 1) == double_quote )then

         call unquote_string( freeze_dir, file_name, unquoted_len, quoted_len)

         chars_after_fn: if( freeze_dir( unquoted_len + 1: ) /= blank )then

            call msg_quit( "extra characters following freeze file name: " // freeze_dir)

         end if chars_after_fn

!  allocate freeze file

         allocate( freeze_file, source= output_freeze_file, stat= state% status, errmsg= state% message)

         alloc_error: if( is_status_error( state) )then

            call msg_quit( "can't allocate freeze file for writing: " // trim( file_name))

         end if alloc_error

         freeze_file = file_name

         call write_freeze_file()

      else have_filename

         chars_after_dir: if( freeze_dir /= blank )then

            call msg_quit( "extra characters following freeze directive: " // freeze_dir)

         end if chars_after_dir

      end if have_filename

      options% freeze_declarations = .true.

   end if active_line

! ----------------------------------------------------------------------

!  process_freeze_directive() exit

return

! **********************************************************************

!  process_freeze_directive()

end subroutine process_freeze_directive

! **********************************************************************
! **********************************************************************

!  write_freeze_file() write symbols to the freeze file as declarations

subroutine write_freeze_file()

! **********************************************************************

!  write_freeze_file() constants

character( len= *), parameter :: integer_declaration_str = 'integer :: '
character( len= *), parameter :: integer_constant_declaration_str = 'integer, parameter :: '
character( len= *), parameter :: logical_declaration_str = 'logical :: '
character( len= *), parameter :: logical_constant_declaration_str = 'logical, parameter :: '
character( len= *), parameter :: macro_declaration_str = 'macro :: '
character( len= *), parameter :: macro_parens_declaration_str = 'macro, parens :: '
character( len= *), parameter :: text_declaration_str = 'text :: '
character( len= *), parameter :: text_parens_declaration_str = 'text, parens :: '

character( len= *), parameter :: end_text_str = 'end text '

character( len= *), parameter :: open_declaration_str = 'open :: '

character( len= *), parameter :: eq_space = ' = '
character( len= *), parameter :: eq_true = ' = .true.'
character( len= *), parameter :: eq_false = ' = .false.'

! **********************************************************************

!  write_freeze_file() local

! ----------------------------------------------------------------------

!  symbol to be written to the log file

   class( symbol_t), pointer :: symbol_ptr

!  construct a line to output

   character( len= buffer_len) :: freeze_line

   character( len= conversion_len) :: value_str

   character( len= buffer_len) :: arg_str

!  predefined macros index and block line index

   integer :: i

! **********************************************************************

!  write_freeze_file() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  open the freeze file

   call freeze_file% open()

! ----------------------------------------------------------------------

   symbol_ptr => first_symbol

   all_symbols: do

      if( .not. associated( symbol_ptr) ) exit all_symbols

! ----------------------------------------------------------------------

!  type-specific data

      print_type: select type( symbol_ptr)

! ----------------------------------------------------------------------

!  logical data

      type is( logical_t)

         is_log_const: if( symbol_ptr% constant )then

            log_const_value: if( symbol_ptr% logical_value )then

               value_str = eq_true

            else log_const_value

               value_str = eq_false

            end if log_const_value

            freeze_line = coco_key // blank // logical_constant_declaration_str // symbol_ptr% name_str // value_str

         else is_log_const

            log_defined: if( symbol_ptr% defined )then

               log_value: if( symbol_ptr% logical_value )then

                  value_str = eq_true

               else log_value

                  value_str = eq_false

               end if log_value

            else log_defined

               value_str = null_string

            end if log_defined

            freeze_line = coco_key // blank // logical_declaration_str // symbol_ptr% name_str // value_str

         end if is_log_const

!  write the line to the freeze file

         call write_freeze_line( freeze_line)

! ----------------------------------------------------------------------

!  integer data

      type is( integer_t)

         is_int_const: if( symbol_ptr% constant )then

            write( unit= value_str, fmt= conversion_fmt) symbol_ptr% integer_value

            freeze_line = coco_key // blank // integer_constant_declaration_str // symbol_ptr% name_str &
                       // eq_space // adjustl( value_str)

         else is_int_const

            int_defined: if( symbol_ptr% defined )then

               write( unit= value_str, fmt= conversion_fmt) symbol_ptr% integer_value
               value_str = eq_space // adjustl( value_str)

            else int_defined

               value_str = null_string

            end if int_defined

            freeze_line = coco_key // blank // integer_declaration_str // symbol_ptr% name_str // value_str

         end if is_int_const

!  write the line to the freeze file

         call write_freeze_line( freeze_line)

! ----------------------------------------------------------------------

!  macro data

      type is( macro_t)

         mac_has_args: if( allocated( symbol_ptr% dummy_args) )then

            arg_str = open_paren // blank // symbol_ptr% dummy_args( 1)

            build_mac_args: do i = 2, size( symbol_ptr% dummy_args)

               arg_str = trim( arg_str) // comma // blank // symbol_ptr% dummy_args( i)

            end do build_mac_args

            arg_str = trim( arg_str) // close_paren

         else mac_has_args

            arg_str = null_string

         end if mac_has_args

         mac_parens: if( symbol_ptr% args_in_parens )then

            freeze_line = coco_key // blank // macro_parens_declaration_str // symbol_ptr% name_str // trim( arg_str) &
                        // eq_space // symbol_ptr% macro_value

         else mac_parens

            freeze_line = coco_key // blank // macro_declaration_str // symbol_ptr% name_str // trim( arg_str) &
                       // eq_space // symbol_ptr% macro_value

         end if mac_parens

!  write the line to the freeze file

         call write_freeze_line( freeze_line)

! ----------------------------------------------------------------------

!  text data

      type is( text_t)

         txt_has_args: if( allocated( symbol_ptr% dummy_args) )then

            arg_str = open_paren // blank // symbol_ptr% dummy_args( 1)

            build_txt_args: do i = 2, size( symbol_ptr% dummy_args)

               arg_str = trim( arg_str) // comma // blank // symbol_ptr% dummy_args( i)

            end do build_txt_args

            arg_str = trim( arg_str) // close_paren

         else txt_has_args

            arg_str = null_string

         end if txt_has_args

         txt_parens: if( symbol_ptr% args_in_parens )then

            freeze_line = coco_key // blank // text_parens_declaration_str // symbol_ptr% name_str // arg_str

         else txt_parens

            freeze_line = coco_key // blank // text_declaration_str // symbol_ptr% name_str // arg_str

         end if txt_parens

!  write the line to the freeze file

         call write_freeze_line( freeze_line)

         write_text_body: do i = 1, size( symbol_ptr% text_lines)

            call write_freeze_line( symbol_ptr% text_lines( i))

         end do write_text_body

         freeze_line = end_text_str // symbol_ptr% name_str

         call write_freeze_line( freeze_line)

! ----------------------------------------------------------------------

!  file data

      type is( file_symbol_t)

         freeze_line = coco_key // blank // open_declaration_str // symbol_ptr% name_str // blank &
                     // equals // blank // single_quote // trim( symbol_ptr% file% name_str) // single_quote

!  write the line to the freeze file

         call write_freeze_line( freeze_line)

! ----------------------------------------------------------------------

      class default

         call msg_quit( 'error: unknown type to be written to freeze file: ' // symbol_ptr% name_str)

      end select print_type

!  next symbol

      symbol_ptr => symbol_ptr% next

   end do all_symbols

! ----------------------------------------------------------------------

!  close the freeze file

   call freeze_file% close()

! ----------------------------------------------------------------------

!  write_freeze_file() exit

return

! **********************************************************************

!  write_freeze_file()

end subroutine write_freeze_file

! **********************************************************************
! **********************************************************************

!  write_freeze_line() ensure lines are not too long

subroutine write_freeze_line( freeze_line)

! **********************************************************************

!  write_freeze_line() interface

! ----------------------------------------------------------------------

!  the line to be wrapped

character( len= *), target, intent( in out) :: freeze_line

! **********************************************************************

!  entry: line is a line of coco declaration with (possibly) more than 132 characters

!  exit: line has continuations written and fewer than 132 source lines

! **********************************************************************

!  write_freeze_line() local

! ----------------------------------------------------------------------

!  length of source line

   integer :: output_len

! **********************************************************************

!  write_freeze_line() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  initialize

   output_len = len_trim( freeze_line)

! ----------------------------------------------------------------------

!  while line is too long

   wrap_lines: do

      if( output_len <= free_form_len ) exit wrap_lines

!  the continuation character, then the rest of the line

      write( unit= freeze_file% io_unit, fmt= string_fmt, iostat= state% status, iomsg= state% message) &
             freeze_line( 1: free_form_len - len( continuation)) // continuation

      split_error: if( is_status_error( state) )then

         call msg_quit( "error: write freeze file: " // get_class_file_name( freeze_file))

      end if split_error

!  reset line to check for another wrap

      freeze_line = continuation // freeze_line( free_form_len: )

      output_len = len_trim( freeze_line)

! ----------------------------------------------------------------------

!  while line is too long

   end do wrap_lines

! ----------------------------------------------------------------------

!  now write the line

   write( unit= freeze_file% io_unit, fmt= string_fmt, iostat= state% status, iomsg= state% message) &
          trim( freeze_line)

   write_error: if( is_status_error( state) )then

      call msg_quit( "error: write freeze file: " // get_class_file_name( freeze_file))

   end if write_error

! ----------------------------------------------------------------------

!  write_freeze_line() exit

return

! **********************************************************************

!  write_freeze_line()

end subroutine write_freeze_line

! **********************************************************************
! **********************************************************************

!  %%% process declarations of integers and logicals

! **********************************************************************
! **********************************************************************

!  get_symbol_name() extract symbol name and determine its length

subroutine get_symbol_name( decl_stmt, symbol_name, name_len)

! **********************************************************************

!  get_symbol_name() interface

! ----------------------------------------------------------------------

!  a declaration statement with a symbol name

character( len= *), intent( in) :: decl_stmt

!  the name of the symbol

character( len= *), intent( out) :: symbol_name

!  the length of the symbol name

integer, intent( out) :: name_len

! **********************************************************************

!  entry: decl_stmt is blank_compress_lower_case declaration statement past the double colon
!         "name" | "name=..."

!  exit: a valid symbol name and its length or error exit

! **********************************************************************

!  get_symbol_name() constants

! ----------------------------------------------------------------------

!  characters which must end a symbol name

character( len= *), parameter :: end_of_name = equals // blank

! **********************************************************************

!  get_symbol_name() local

! ----------------------------------------------------------------------

!  pointers to characters in decl_stmt

   integer :: char_idx

! **********************************************************************

!  get_symbol_name() text

continue

! ----------------------------------------------------------------------

!  look for equals following separator

   char_idx = scan( decl_stmt, end_of_name)

   name_error: if( char_idx == 0 )then

      call msg_quit( "can't find name in declaration: " // trim( decl_stmt))

   end if name_error

!  length of name is one less than first character past name

   name_len = char_idx - 1

! ----------------------------------------------------------------------

!  check that name is a valid new name

   call valid_new_name( decl_stmt( 1: name_len))

!  return name

   symbol_name = decl_stmt( 1: name_len)

! ----------------------------------------------------------------------

!  get_symbol_name() exit

return

! **********************************************************************

!  get_symbol_name()

end subroutine get_symbol_name

! **********************************************************************
! **********************************************************************

!  process_integer_declaration() process integer declarations

subroutine process_integer_declaration( integer_stmt, is_const)

! **********************************************************************

!  process_integer_declaration() interface

! ----------------------------------------------------------------------

!  the statement containing the declaration

character( len= *), intent( in) :: integer_stmt

!  true if declaration is of constants

logical, intent( in) :: is_const

! **********************************************************************

!  entry: int_stmt is blank_compress_lower_case integer declaration past the integer keyword
!         "::..." | ",parameter::..."

!  exit: int_stmt is processed or error exit

! **********************************************************************

!  process_integer_declaration() constants

! ----------------------------------------------------------------------

!  mark the end of a definition

character( len= *), parameter :: end_of_def = comma // blank

! **********************************************************************

!  process_integer_declaration() local

! ----------------------------------------------------------------------

!  string containing a single symbol declaration symbol

   character( len= buffer_len) :: symbol_str

!  name of symbol

   character( len= symbol_name_len) :: symbol_name

!  results of decoding statement

   integer :: symbol_len

! ----------------------------------------------------------------------

!  point to next character to be decoded

   integer :: next_char

   integer :: def_len

! **********************************************************************

!  process_integer_declaration() text

continue

! ----------------------------------------------------------------------

!  if active line, process the declaration

   active_line: if( if_construct% now_selected )then

!  ensure no freeze is in effect

      if( options% freeze_declarations ) call msg_quit( "attempt to declare integer after freeze")

      next_char = 1

!  extract all symbols on directive

      all_symbols: do

!  one symbol at a time to the symbol string

         def_len = scan( integer_stmt( next_char: ), end_of_def) + next_char - 1

         symbol_str = integer_stmt( next_char: def_len - 1)

!  extract symbol name

         call get_symbol_name( symbol_str, symbol_name, symbol_len)

!  store symbol in symbol list

         call add_integer( symbol_str( symbol_len + 1: def_len - 1), symbol_name, is_const)

!  comma separates symbols, blank is end of statement

         all_done: if( integer_stmt( def_len: def_len) == blank )then

            exit all_symbols

         end if all_done

!  move to next symbol

         next_char = def_len + 1

!  extract all symbols on directive

      end do all_symbols

!  if active line, process the declaration

   end if active_line

! ----------------------------------------------------------------------

!  process_integer_declaration() exit

return

! **********************************************************************

!  process_integer_declaration()

end subroutine process_integer_declaration

! **********************************************************************
! **********************************************************************

!  add_integer() store integer declaration in symbol table

subroutine add_integer( int_decl_str, integer_name, is_const)

! **********************************************************************

!  add_integer() interface

! ----------------------------------------------------------------------

!  the statement containing the declaration

character( len= *), intent( in) :: int_decl_str

!  the symbol name

character( len= *), intent( in) :: integer_name

!  true if the symbol is a constant

logical, intent( in) :: is_const

! **********************************************************************

!  entry: int_decl_str is blank_compress_lower_case integer declaration statement past the name
!         "" | "=..."
!         sym_name is the symbol name
!         is_const is true if this is a constant declaration

!  exit: integer declaration is added to the integer symbol list or error exit

! **********************************************************************

!  add_integer() local

! ----------------------------------------------------------------------

!  expression defining integer symbol

   character( len= buffer_len) :: expr_str

!  type pointer to allocate

   type( integer_t), pointer :: integer_ptr

! **********************************************************************

!  add_integer() text

continue

! ----------------------------------------------------------------------

!  allocate new integer

   allocate( integer_ptr, stat= state% status, errmsg= state% message)

   alloc_error: if( is_status_error( state) )then

      call msg_quit( "can't allocate integer: " // trim( integer_name))

   end if alloc_error

!  build new integer on list

   subsequent_or_first: if( associated( first_symbol) )then

      last_symbol% next => integer_ptr

      last_symbol => last_symbol% next

   else subsequent_or_first

      first_symbol => integer_ptr

      last_symbol => first_symbol

   end if subsequent_or_first

   nullify( integer_ptr% next)

! ----------------------------------------------------------------------

!  set symbol members

   integer_ptr% name_str = integer_name

   integer_ptr% declared_file = get_class_file_name( current_file)
   integer_ptr% declared_line = current_file% lines_transfered

   integer_ptr% referenced = .false.
   integer_ptr% referenced_file = null_string
   integer_ptr% referenced_line = 0

! ----------------------------------------------------------------------

!  store whether integer is a constant

   integer_ptr% constant = is_const

!  store whether symbol is declared in the set file

   integer_ptr% sf_defined = .false.
   integer_ptr% cl_defined = .false.

!  determine if declaration specifies a value

   got_eq: if( len( int_decl_str) > 0 )then

      integer_ptr% defined = int_decl_str( 1: len( equals)) == equals

   else got_eq

      integer_ptr% defined = .false.

   end if got_eq

!  compute and store the value if there is one

   constant_value: if( integer_ptr% constant .and. .not. integer_ptr% defined )then

      call msg_quit( "an integer constant must have a value: " // trim( integer_name) // trim( int_decl_str) )

   end if constant_value

!  decode the value

   process_value: if( integer_ptr% defined )then

      all_constants = .true.

      expr_str = int_decl_str( len( equals) + 1: )

      call eval_int_expr( expr_str, integer_ptr% integer_value)

      non_const: if( integer_ptr% constant .and. .not. all_constants )then

         call msg_quit( "non constant expression used to define integer constant: " // trim( integer_name))

      end if non_const

      integer_ptr% defined_file = get_class_file_name( current_file)
      integer_ptr% defined_line = current_file% lines_transfered

   end if process_value

!  allow a value from the set file and/or the command line to overwrite the source file value

   call get_int_value_from_set_file( integer_ptr)
   call get_int_value_from_cmdline( integer_ptr)

! ----------------------------------------------------------------------

!  add_integer() exit

return

! **********************************************************************

!  add_integer()

end subroutine add_integer

! **********************************************************************
! **********************************************************************

!  process_logical_declaration() process logical declarations

subroutine process_logical_declaration( logical_stmt, is_const)

! **********************************************************************

!  process_logical_declaration() interface

! ----------------------------------------------------------------------

!  the statement containing the logical declaration

character( len= *), intent( in) :: logical_stmt

!  true if declaration is of constants

logical, intent( in) :: is_const

! **********************************************************************

!  entry: logical_stmt is blank_compress_lower_case logical declaration past the logical keyword
!         "::..." | ",parameter::..."

!  exit: logical declaration is processed or error exit

! **********************************************************************

!  process_logical_declaration() local

! ----------------------------------------------------------------------

!  string containing a single symbol declaration symbol

   character( len= buffer_len) :: symbol_str

!  name of symbol

   character( len= symbol_name_len) :: symbol_name

!  results of decoding statement

   integer :: symbol_len

! ----------------------------------------------------------------------

!  point to next character to be decoded

   integer :: next_char

   integer :: decl_len

! **********************************************************************

!  process_logical_declaration() text

continue

! ----------------------------------------------------------------------

!  if active line, process the declaration

   active_line: if( if_construct% now_selected )then

!  ensure no freeze is in effect

      if( options% freeze_declarations ) call msg_quit( "attempt to declare logical after freeze")

      next_char = 1

!  extract all symbols on directive

      all_symbols: do

!  one symbol at a time to the symbol string

         decl_len = scan( logical_stmt( next_char: ), end_of_decl) + next_char - 1

         symbol_str = logical_stmt( next_char: decl_len - 1)

!  extract symbol name

         call get_symbol_name( symbol_str, symbol_name, symbol_len)

!  store symbol in symbol list

         call add_logical( symbol_str( symbol_len + 1: decl_len - 1), symbol_name, is_const)

!  comma separates symbols, blank is end of statement

         all_done: if( logical_stmt( decl_len: decl_len) == blank )then

            exit all_symbols

         end if all_done

!  reset for next symbol

         next_char = decl_len + 1

!  extract all symbols on directive

      end do all_symbols

!  if active line, process the declaration

   end if active_line

! ----------------------------------------------------------------------

!  process_logical_declaration() exit

return

! **********************************************************************

!  process_logical_declaration()

end subroutine process_logical_declaration

! **********************************************************************
! **********************************************************************

!  add_logical() store logical declaration in symbol table

subroutine add_logical( log_decl_str, logical_name, is_const)

! **********************************************************************

!  add_logical() interface

! ----------------------------------------------------------------------

!  the statement containing the declaration

character( len= *), intent( in) :: log_decl_str

!  the valid logical name

character( len= *), intent( in) :: logical_name

!  true if the symbol is a constant

logical, intent( in) :: is_const

! **********************************************************************

!  entry: int_decl_str is blank_compress_lower_case logical declaration statement past the name
!         "" | "=..."
!         sym_name is the symbol name
!         is_const is true if this is a constant declaration

!  exit: logical declaration is added to the logical symbol list or error exit

! **********************************************************************

!  add_logical() local

! ----------------------------------------------------------------------

!  expression defining logical symbol

   character( len= buffer_len) :: expr_str

!  type pointer to allocate

   type( logical_t), pointer :: logical_ptr

! **********************************************************************

!  add_logical() text

continue

! ----------------------------------------------------------------------

!  allocate new logical

   allocate( logical_ptr, stat= state% status, errmsg= state% message)

   alloc_error: if( is_status_error( state) )then

      call msg_quit( "can't allocate logical: " // trim( logical_name))

   end if alloc_error

!  build new logical on list

   subsequent_or_first: if( associated( first_symbol) )then

      last_symbol% next => logical_ptr

      last_symbol => last_symbol% next

   else subsequent_or_first

      first_symbol => logical_ptr

      last_symbol => first_symbol

   end if subsequent_or_first

   nullify( logical_ptr% next)

! ----------------------------------------------------------------------

!  set symbol members

   logical_ptr% name_str = logical_name

   logical_ptr% declared_file = get_class_file_name( current_file)
   logical_ptr% declared_line = current_file% lines_transfered

   logical_ptr% referenced = .false.
   logical_ptr% referenced_file = null_string
   logical_ptr% referenced_line = 0

! ----------------------------------------------------------------------

!  store whether logical is a constant

   logical_ptr% constant = is_const

!  store whether symbol is declared in the set file

   logical_ptr% sf_defined = .false.
   logical_ptr% cl_defined = .false.

!  determine if declaration specifies a value

   got_eq: if( len( log_decl_str) > 0 )then

      logical_ptr% defined = log_decl_str( 1: len( equals)) == equals

   else got_eq

      logical_ptr% defined = .false.

   end if got_eq

!  compute and store the value if there is one

   constant_value: if( logical_ptr% constant .and. .not. logical_ptr% defined )then

      call msg_quit( "an logical constant must have a value: " &
                      // trim( logical_name) // trim( log_decl_str) )

   end if constant_value

!  decode the value

   process_value: if( logical_ptr% defined )then

      all_constants = .true.

      expr_str = log_decl_str( len( equals) + 1: )

      call eval_log_expr( expr_str, logical_ptr% logical_value)

      non_const: if( logical_ptr% constant .and. .not. all_constants )then

         call msg_quit( "non constant expression used to define logical constant: " // trim( logical_name))

      end if non_const

      logical_ptr% defined_file = get_class_file_name( current_file)
      logical_ptr% defined_line = current_file% lines_transfered

   end if process_value

!  allow a value from the set file and/or the command line to overwrite the source file value

   call get_log_value_from_set_file( logical_ptr)
   call get_log_value_from_cmdline( logical_ptr)

! ----------------------------------------------------------------------

!  add_logical() exit

return

! **********************************************************************

!  add_logical()

end subroutine add_logical

! **********************************************************************
! **********************************************************************

!  %%% process if constructs: if, else if, else, end if

! **********************************************************************
! **********************************************************************

!  new_block() pushes a new block on the block list

subroutine new_block( phase)

! **********************************************************************

!  new_block() interface

! ----------------------------------------------------------------------

!  the phase of the new block, or if_block if not present

integer, intent( in), optional :: phase

! **********************************************************************

!  entry: a phase and the block list

!  exit: the block list has a new entry with the required phase

! **********************************************************************

!  new_block() text

continue

! ----------------------------------------------------------------------

!  append new if construct at end of list

   allocate( if_construct% nested, stat= state% status, errmsg= state% message)

   alloc_error: if( is_status_error( state) )then

      call msg_quit( "allocate block failed")

   end if alloc_error

!  establish pointers

   if_construct% nested% enclosing => if_construct

!  make new if construct the active if construct

   if_construct => if_construct% nested

   nullify( if_construct% nested)

!  if the phase is specified

   use_phase: if( present( phase) )then

!  this phase is as requested

      if_construct% phase = phase

!  if this is processed, it's an active statement

      if_construct% now_selected = .true.

   else use_phase

!  otherwise, this phase is an if block

      if_construct% phase = if_block

   end if use_phase

! ----------------------------------------------------------------------

!  new_block() exit

return

! **********************************************************************

!  new_block()

end subroutine new_block

! **********************************************************************
! **********************************************************************

!  delete_block() pops a block on the block list

subroutine delete_block( phase)

! **********************************************************************

!  delete_block() interface

! ----------------------------------------------------------------------

!  the phase of the popped block, no required phase if not present

integer, intent( in), optional :: phase

! **********************************************************************

!  entry: a phase and the block list

!  exit: the block list has an entry removed or error

! **********************************************************************

!  delete_block() local

! ----------------------------------------------------------------------

!  status of deallocating a block

   integer :: astat

! **********************************************************************

!  delete_block() text

continue

! ----------------------------------------------------------------------

!  if requires phase, enforce it

   have_phase: if( present( phase) )then

      must_be: if( if_construct% phase /= phase )then

         call msg_quit( "blocks bested badly")

      end if must_be

   end if have_phase

! ----------------------------------------------------------------------

!  decrement if level

   if_construct => if_construct% enclosing

   deallocate( if_construct% nested, stat= astat)

   next_error: if( astat > 0 )then

      call msg_quit( "deallocate block failed")

   end if next_error

   nullify( if_construct% nested)

! ----------------------------------------------------------------------

!  delete_block() exit

return

! **********************************************************************

!  delete_block()

end subroutine delete_block

! **********************************************************************
! **********************************************************************

!  process_if_directive() process a coco if( )then directive

subroutine process_if_directive( if_dir)

! **********************************************************************

!  process_if_directive() interface

! ----------------------------------------------------------------------

!  a statement containing an if directive

character( len= *), intent( in) :: if_dir

! **********************************************************************

!  entry: if_dir is blank_compress_lower_case if directive, past the coco key and the "if("
!         "<logical>)then"

!  exit: the directive is processed or error exit

! **********************************************************************

!  process_if_directive() local

! ----------------------------------------------------------------------

!  pointer to ')then'

   integer :: then_idx

!  value of logical expression

   logical :: expression_value

!  copy expression string for evaluation

   character( len= buffer_len) :: expr_str

! **********************************************************************

!  process_if_directive() text

continue

! ----------------------------------------------------------------------

!  check for a well formed if directive

   then_idx = index( if_dir, then_str)

   syntax_check: if( then_idx == 0 )then

      call msg_quit( "no 'then' in if directive: " // trim( if_dir) )

   end if syntax_check

   extra_chars_check: if( if_dir( then_idx + len( then_str): ) /= blank )then

      call msg_quit( "extra characters after if directive: " // trim( if_dir) )

   end if extra_chars_check

! ----------------------------------------------------------------------

!  append new if construct at end of list

   call new_block()

! ----------------------------------------------------------------------

!  if this if block is enclosed within selected lines

   active_lines: if( if_construct% enclosing% now_selected )then

! ----------------------------------------------------------------------

!  evaluate logical expression only when enclosing if block is selected

      expr_str = if_dir( 1: then_idx - 1)

      call eval_log_expr( expr_str, expression_value)

!  set if value accordingly

      if_construct% now_selected = expression_value

      if_construct% ever_selected = expression_value

! ----------------------------------------------------------------------

!  the enclosing if block is not selected

   else active_lines

      if_construct% now_selected = .false.

      if_construct% ever_selected = .true.

   end if active_lines

! ----------------------------------------------------------------------

!  process_if_directive() exit

return

! **********************************************************************

!  process_if_directive()

end subroutine process_if_directive

! **********************************************************************
! **********************************************************************

!  process_elseif_directive() process a coco elseif( )then directive

subroutine process_elseif_directive( elseif_dir)

! **********************************************************************

!  process_elseif_directive() interface

! ----------------------------------------------------------------------

!  a statement containing an elseif directive

character( len= *), intent( in) :: elseif_dir

! **********************************************************************

!  entry: elseif_dir is blank_compress_lower_case elseif directive, past the coco key

!  exit: the directive is processed or error exit

! **********************************************************************

!  process_elseif_directive() local

! ----------------------------------------------------------------------

!  location of closing )then

   integer :: then_idx

!  value of logical expression

   logical :: expression_value

   character( len= buffer_len) :: expr_str

! **********************************************************************

!  process_elseif_directive() text

continue

! ----------------------------------------------------------------------

!  if not in if-block, elseif is misplaced

   if_sequence: select case( if_construct% phase)

   case( outside_block ) if_sequence

      call msg_quit( "else if outside if construct: " // trim( elseif_dir) )

   case( else_block ) if_sequence

      call msg_quit( "else if after else: " // trim( elseif_dir) )

   case( text_block ) if_sequence

      call msg_quit( "else if badly nested in text block: " // trim( elseif_dir) )

   case( include_block ) if_sequence

      call msg_quit( "else if badly nested in include file: " // trim( elseif_dir) )

   end select if_sequence

! ----------------------------------------------------------------------

!  logical expression must be between 'if(' and ')then'

   then_idx = index( elseif_dir, then_str)

   syntax_check: if( then_idx == 0 )then

      call msg_quit( "no 'then' in else if directive: " // trim( elseif_dir) )

   end if syntax_check

   extra_chars_check: if( elseif_dir( then_idx + len( then_str): ) /= blank )then

      call msg_quit( "extra characters after else if directive: " // trim( elseif_dir) )

   end if extra_chars_check

!  this phase is an elseif block

   if_construct% phase = elseif_block

! ----------------------------------------------------------------------

!  if this if block is enclosed within selected lines

   active_lines: if( if_construct% enclosing% now_selected )then

      previous_true: if( if_construct% ever_selected )then

         if_construct% now_selected = .false.

      else previous_true

!  evaluate logical expression

         expr_str = elseif_dir( 1: then_idx - 1)

         call eval_log_expr( expr_str, expression_value)

!  set if value accordingly

         if_construct% now_selected = expression_value

         if_construct% ever_selected = expression_value

      end if previous_true

   end if active_lines

! ----------------------------------------------------------------------

!  process_elseif_directive() exit

return

! **********************************************************************

!  process_elseif_directive()

end subroutine process_elseif_directive

! **********************************************************************
! **********************************************************************

!  process_else_directive() process a coco else directive

subroutine process_else_directive( else_dir)

! **********************************************************************

!  process_else_directive() interface

! ----------------------------------------------------------------------

!  a statement containing an else directive

character( len= *), intent( in) :: else_dir

! **********************************************************************

!  entry: else_dir is blank_compress_lower_case else directive, past the coco key

!  exit: the directive is processed or error exit

! **********************************************************************

!  process_else_directive() text

continue

! ----------------------------------------------------------------------

!  if not in if-block, else is misplaced

   if_sequence: select case( if_construct% phase)

   case( outside_block ) if_sequence

      call msg_quit( "else outside if construct: " // trim( else_dir) )

   case( else_block ) if_sequence

      call msg_quit( "else after else: " // trim( else_dir) )

   case( text_block ) if_sequence

      call msg_quit( "else badly nested in text block: " // trim( else_dir) )

   case( include_block ) if_sequence

      call msg_quit( "else badly nested in include file: " // trim( else_dir) )

   end select if_sequence

!  must have nothing after 'else'

   syntax_error: if( else_dir /= blank )then

      call msg_quit( "extra characters after else directive: " // trim( else_dir) )

   end if syntax_error

! ----------------------------------------------------------------------

!  this phase is an else block

   if_construct% phase = else_block

! ----------------------------------------------------------------------

!  select else block if this if ithe enclosing block is active and no previous block has been selected

   if_construct% now_selected = if_construct% enclosing% now_selected &
                                .and. .not. if_construct% ever_selected

! ----------------------------------------------------------------------

!  process_else_directive() exit

return

! **********************************************************************

!  process_else_directive()

end subroutine process_else_directive

! **********************************************************************
! **********************************************************************

!  process_endif_directive() process a coco endif directive

subroutine process_endif_directive( endif_dir)

! **********************************************************************

!  process_endif_directive() interface

! ----------------------------------------------------------------------

!  a statement containing an endif directive

character( len= *), intent( in) :: endif_dir

! **********************************************************************

!  entry: endif_dir is blank_compress_lower_case endif directive, past the coco key

!  exit: the directive is processed or error exit

! **********************************************************************

!  process_endif_directive() local

! **********************************************************************

!  process_endif_directive() text

continue

! ----------------------------------------------------------------------

!  if not in if-block, endif is misplaced

   if_sequence: select case( if_construct% phase)

   case( outside_block ) if_sequence

      call msg_quit( "end if outside any if construct: " // trim( endif_dir) )

   case( text_block ) if_sequence

      call msg_quit( "end if badly nested in text block: " // trim( endif_dir) )

   case( include_block ) if_sequence

      call msg_quit( "end if badly nested in include file: " // trim( endif_dir) )

   end select if_sequence

!  must have nothing after 'endif'

   syntax_error: if( endif_dir /= blank )then

      call msg_quit( "extra characters after end if directive: " // trim( endif_dir) )

   end if syntax_error

! ----------------------------------------------------------------------

!  decrement if level

   call delete_block()

! ----------------------------------------------------------------------

!  process_endif_directive() exit

return

! **********************************************************************

!  process_endif_directive()

end subroutine process_endif_directive

! **********************************************************************
! **********************************************************************

!  process extensions assert, macro, text, copy

! **********************************************************************
! **********************************************************************

!  process_assert_directive() process an assert directive

subroutine process_assert_directive( assert_dir)

! **********************************************************************

!  process_assert_directive() interface

! ----------------------------------------------------------------------

!  a statement containing an assert directive

character( len= *), intent( in) :: assert_dir

! **********************************************************************

!  entry: assert_dir is quoted assert condition

!  exit: assertion code is written to the output

! **********************************************************************

!  process_assert_directive() constants

! ----------------------------------------------------------------------

!  pieces of the assert output

character( len= *), parameter :: if_prt = '" if( .not. '

character( len= *), parameter :: then_prt = ')then'

character( len= *), parameter :: write_prt = '" write( unit= *, fmt= *) "assertion failed: '

character( len= *), parameter :: stop_prt = '" stop "assertion failed"'

character( len= *), parameter :: endif_prt = '" end if'

! ----------------------------------------------------------------------

!  starting column of output

integer, parameter :: free_start_col = 1

integer, parameter :: fixed_start_col = 7

! **********************************************************************

!  process_assert_directive() local

! ----------------------------------------------------------------------

!  the column to start writing output source

   integer, save :: start_col = free_start_col

!  the length of the assert condition

   integer :: cond_len

!  start of the condition in the as-is buffer

   integer :: open_idx

!  assemble the output line

   character( len= buffer_len) :: edit_line

! ----------------------------------------------------------------------

!  convert the current input line number to characters

   character( len= conversion_len) :: conversion_str

! **********************************************************************

!  process_assert_directive() text

continue

! ----------------------------------------------------------------------

!  check syntax- condition must be within parentheses

   find_open_praen: if( assert_dir( 1: 1) /= open_paren )then

      call msg_quit( "can't find open parenthesis: " // trim( assert_dir))

   end if find_open_praen

   call seek_close_paren( assert_dir, 1, cond_len)

   find_close_praen: if( assert_dir( cond_len: cond_len) /= close_paren )then

      call msg_quit( "can't find close parenthesis in assert directive: " // trim( assert_dir))

   else if( len_trim( assert_dir) <= len( open_paren // close_paren) )then find_close_praen

      call msg_quit( "can't find condition in assert directive: " // trim( assert_dir))

   end if find_close_praen

!  check syntax- directive must be blank after condition

   extra_chars: if( assert_dir( cond_len + 1: ) /= blank )then

      call msg_quit( "extra characters after assert condition: " // trim( assert_dir))

   end if extra_chars

! ----------------------------------------------------------------------

!  if active block, process assert directive

   active_line: if( if_construct% now_selected )then

!  use the condition from the as-is buffer

      open_idx = index( asis_stmt, open_paren)

!  set start column per the source form

      if( .not. options% free_form ) start_col = fixed_start_col

!  write the if statement

      if( start_col > 1 ) edit_line( 1: start_col - 1) = blank

      edit_line( start_col: ) = if_prt // asis_stmt( open_idx: asis_len) // then_prt

!  remove any line length overflow

      if( options% wrapping_lines ) call wrap_source_line( edit_line)

! ----------------------------------------------------------------------

!  write assembled if-then statement

      line = edit_line( 1: len( line))

      call write_source_line( output_file)

! ----------------------------------------------------------------------

!  write the write statement

      if( start_col > 1 ) edit_line( 1: start_col - 1) = blank

!  get the current line number

      write( unit= conversion_str, fmt= conversion_fmt) current_file% lines_transfered

!  construct the assertion complaint

      edit_line( start_col: ) = write_prt // trim( get_class_file_name( current_file)) &
                                          // ": " // trim( adjustl( conversion_str)) &
                                          // ': " // ' // '"' // trim( asis_stmt( open_idx: asis_len)) // '"'

!  remove any line length overflow

      if( options% wrapping_lines ) call wrap_source_line( edit_line)

! ----------------------------------------------------------------------

!  write assembled write statement

      line = edit_line( 1: len( line))

      call write_source_line( output_file)

! ----------------------------------------------------------------------

!  blank until the start column for the stop and end if

      if( start_col > 1 ) line( 1: start_col - 1) = blank

!  write the stop statement

      line( start_col: ) = stop_prt

      call write_source_line( output_file)

! ----------------------------------------------------------------------

!  blank until the start column for the stop and end if

      if( start_col > 1 ) line( 1: start_col - 1) = blank

!  write the end if statement

      line( start_col: ) = endif_prt

      call write_source_line( output_file)

! ----------------------------------------------------------------------

   end if active_line

! ----------------------------------------------------------------------

!  process_assert_directive() exit

return

! **********************************************************************

!  process_assert_directive()

end subroutine process_assert_directive

! **********************************************************************
! **********************************************************************

!  %%% process coco macros

! **********************************************************************
! **********************************************************************

!  get_macro_name() verify macro name and determine its length

subroutine get_macro_name( decl_stmt, macro_name, name_len)

! **********************************************************************

!  get_macro_name() interface

! ----------------------------------------------------------------------

!  the directive containing the macro

character( len= *), intent( in) :: decl_stmt

!  the name of the macro

character( len= *), intent( out) :: macro_name

!  the length of the macro name

integer, intent( out) :: name_len

! **********************************************************************

!  entry: decl_stmt is blank_compress_lower_case declaration statement past the double colon
!         "name=..." | "name(..."

!  exit: name is valid and its length is known or error exit

! **********************************************************************

!  get_macro_name() constants

! ----------------------------------------------------------------------

!  equals or open parenthesis may end a macro name

character( len= *), parameter :: end_of_name = equals // open_paren

! **********************************************************************

!  get_macro_name() local

! ----------------------------------------------------------------------

!  pointers to characters in decl_stmt

   integer :: char_idx

! **********************************************************************

!  get_macro_name() text

continue

! ----------------------------------------------------------------------

!  look for equals or open parenthesis following separator

   char_idx = scan( decl_stmt, end_of_name)

!  if no equals or open paren found

   no_eq_op: if( char_idx == 0 )then

     call msg_quit( "can't find name in macro declaration: " // trim( decl_stmt))

   end if no_eq_op

   name_len = char_idx - 1

! ----------------------------------------------------------------------

!  check that name is not in use and has a valid form

   call valid_new_name( decl_stmt( 1: name_len))

   macro_name = decl_stmt( 1: name_len)

! ----------------------------------------------------------------------

!  get_macro_name() exit

return

! **********************************************************************

!  get_macro_name()

end subroutine get_macro_name

! **********************************************************************
! **********************************************************************

!  process_macro_declaration() process macro declarations

subroutine process_macro_declaration( mac_stmt, must_parens)

! **********************************************************************

!  process_macro_declaration() interface

! ----------------------------------------------------------------------

!  the statement containing the macro declaration

character( len= *), intent( in) :: mac_stmt

!  this macro must have actual args in parens

logical, intent( in) :: must_parens

! **********************************************************************

!  entry: mac_stmt is blank_compress_lower_case logical declaration
!         past the macro keyword "::..."

!  exit: macro declaration is processed or error exit

! **********************************************************************

!  process_macro_declaration() local

! ----------------------------------------------------------------------

!  name of symbol

   character( len= symbol_name_len) :: macro_name

!  results of decoding statement

   integer :: name_len

! **********************************************************************

!  process_macro_declaration() text

continue

! ----------------------------------------------------------------------

!  extract symbol name

   call get_macro_name( mac_stmt, macro_name, name_len)

!  if active line, process the declaration

   active_line: if( if_construct% now_selected )then

!  ensure no freeze is in effect

      if( options% freeze_declarations ) call msg_quit( "attempt to declare macro after freeze")

!  store symbol in symbol list

      call add_macro( mac_stmt( name_len + 1: ), macro_name, must_parens)

   end if active_line

! ----------------------------------------------------------------------

!  process_macro_declaration() exit

return

! **********************************************************************

!  process_macro_declaration()

end subroutine process_macro_declaration

! **********************************************************************
! **********************************************************************

!  add_macro() store macro declaration in symbol table

subroutine add_macro( mac_decl_str, macro_name, must_parens)

! **********************************************************************

!  add_macro() interface

! ----------------------------------------------------------------------

!  the statement containing the declaration

character( len= *), intent( in) :: mac_decl_str

!  the symbol name

character( len= *), intent( in) :: macro_name

!  wrap actual args in parens

logical, intent( in) :: must_parens

! **********************************************************************

!  entry: int_decl_str is blank_compress_lower_case macro declaration statement past the name
!         "" | "=..."
!         sym_name is the symbol name
!         is_const is true if this is a constant declaration

!  exit: macro declaration is added to the macro symbol list or error exit

! **********************************************************************

!  add_macro() local

! ----------------------------------------------------------------------

!  type pointer to allocate

   type( macro_t), pointer :: macro_ptr

!  index of the =

   integer :: eq_idx

!  length of arglist if there is one

   integer :: arglist_len

! **********************************************************************

!  add_macro() text

continue

! ----------------------------------------------------------------------

!  allocate new macro

   allocate( macro_ptr, stat= state% status, errmsg= state% message)

   alloc_error: if( is_status_error( state) )then

      call msg_quit( "can't allocate macro: " // trim( macro_name))

   end if alloc_error

!  build new macro on list

   subsequent_or_first: if( associated( first_symbol) )then

      last_symbol% next => macro_ptr

      last_symbol => last_symbol% next

   else subsequent_or_first

      first_symbol => macro_ptr

      last_symbol => first_symbol

   end if subsequent_or_first

   nullify( macro_ptr% next)

! ----------------------------------------------------------------------

!  set symbol members

   macro_ptr% name_str = macro_name

   macro_ptr% declared_file = get_class_file_name( current_file)
   macro_ptr% declared_line = current_file% lines_transfered

   macro_ptr% referenced = .false.
   macro_ptr% referenced_file = null_string
   macro_ptr% referenced_line = 0

! ----------------------------------------------------------------------

!  check for dummy argument list

   got_paren: if( mac_decl_str( 1: len( open_paren)) == open_paren )then

!  arglist_len is zero if no close paren

      arglist_len = index( mac_decl_str, close_paren)

      bad_arg_list_len: if( arglist_len <= len( open_paren // close_paren) )then

         call msg_quit( "bad macro dummy argument list: " // trim( mac_decl_str))

      end if bad_arg_list_len

!  must have something between parens

      call process_dummy_arglist( mac_decl_str( len( open_paren) + 1: arglist_len - 1), macro_ptr% dummy_args)

      allocate( macro_ptr% actual_args( size( macro_ptr% dummy_args)), stat= state% status, errmsg= state% message)

      actual_error: if( is_status_error( state) )then

         call msg_quit( "can't aloocate macro actual args array: " // macro_ptr% name_str)

      end if actual_error

   else got_paren

!  declared must have parenthesis on actual arguments but no dummy arguments

      parens_no_args: if( must_parens )then

         call msg_quit( "macro actual args declared to need parens but no dummy args: " // macro_ptr% name_str)

      end if parens_no_args

   end if got_paren

! ----------------------------------------------------------------------

!  must have equals

   eq_idx = index( mac_decl_str, equals)

   no_eq: if( eq_idx == 0 )then

      call msg_quit( "a macro definition must have an equals sign: " // trim( mac_decl_str))

   end if no_eq

!  process the macro value

   call verify_macro_value( macro_ptr% dummy_args, mac_decl_str( eq_idx + 1: ))

!  assign the macro value from the as-is buffer

   eq_idx = index( asis_stmt, equals)

   macro_ptr% macro_value = adjustl( asis_stmt( eq_idx + 1: asis_len))

!  finish the macro definition

   macro_ptr% args_in_parens = must_parens

! ----------------------------------------------------------------------

!  add_macro() exit

return

! **********************************************************************

!  add_macro()

end subroutine add_macro

! **********************************************************************
! **********************************************************************

!  process_dummy_arglist() process macro or text dummy argument list

subroutine process_dummy_arglist( arglist, arg_array)

! **********************************************************************

!  process_dummy_arglist() interface

! ----------------------------------------------------------------------

!  the non-null comma separated dummy argument list

character( len= *), intent( in) :: arglist

!  an allocatable array one dummy arg per word

type( target_t), dimension( :), allocatable :: arg_array

! **********************************************************************

!  entry: arglist is a character with the arglist

!  exit: symbol_ptr has its arglist array allocated or defined

! **********************************************************************

!  process_dummy_arglist() constants

character( len= *), parameter :: end_of_arg = blank // comma

! **********************************************************************

!  process_dummy_arglist() local

! ----------------------------------------------------------------------

!  number of dummy arguments found

   integer :: number_of_args

!  an element in arg_array or character in arg list

   integer :: i

!  beginning and end of an arg

   integer :: b_arg, e_arg

! **********************************************************************

!  process_dummy_arglist() text

continue

! ----------------------------------------------------------------------

!  count macro dummy arguments

   number_of_args = 1

   count_args: do i = 1, len( arglist)

      found_another: select case( arglist( i: i))

      case( comma) found_another

         number_of_args = number_of_args + 1

      end select found_another

   end do count_args

! ----------------------------------------------------------------------

!  allocate array to hold dummy args

   allocate( arg_array( 1: number_of_args), stat= state% status, errmsg= state% message)

   arg_error: if( is_status_error( state) )then

      call msg_quit( "allocate dummy argument list failed: " // trim( arglist))

   end if arg_error

! ----------------------------------------------------------------------

!  put each dummy arg into its own array element

   b_arg = 1

   get_args: do i = 1, number_of_args

!  find end of each arg

      e_arg = scan( arglist( b_arg: ) // blank, end_of_arg) - 2 + b_arg

!  store arg

      arg_array( i) = arglist( b_arg: e_arg)

!  shift off that arg

      b_arg = e_arg + 2

   end do get_args

!  check the dummy arg names

   call verify_dummy_args( arg_array)

! ----------------------------------------------------------------------

!  process_dummy_arglist() exit

return

! **********************************************************************

!  process_dummy_arglist()

end subroutine process_dummy_arglist

! **********************************************************************
! **********************************************************************

!  verify_dummy_args() process macro or text dummy argument list

subroutine verify_dummy_args( arg_array)

! **********************************************************************

!  verify_dummy_args() interface

! ----------------------------------------------------------------------

!  an array of dummy args

type( target_t), dimension( :), intent( in) :: arg_array

! **********************************************************************

!  entry: macro or text dummy arg list

!  exit: macro's dummy arguments are valid and may be substituted in value

! **********************************************************************

!  verify_dummy_args() local

! ----------------------------------------------------------------------

!  index args array

   integer :: i

   integer :: j

! **********************************************************************

!  verify_dummy_args() text

continue

! ----------------------------------------------------------------------

!  macro or text dummy arguments must be valid names

   check_names: do i = 1, size( arg_array)

      call valid_new_name( trim_name( arg_array( i)))

      dup_name: do j = 1, i - 1

         got_dup: if( arg_array( i) == arg_array( j) )then

            call msg_quit( "found duplicated dummy argument name: " // arg_array( i))

         end if got_dup

      end do dup_name

   end do check_names

! ----------------------------------------------------------------------

!  verify_dummy_args() exit

return

! **********************************************************************

!  verify_dummy_args()

end subroutine verify_dummy_args

! **********************************************************************
! **********************************************************************

!  verify_macro_value() process macro dummy argument list

subroutine verify_macro_value( arglist, value_str)

! **********************************************************************

!  verify_macro_value() interface

! ----------------------------------------------------------------------

!  a pointer to the macro definition so far

type( target_t), dimension( :), allocatable :: arglist

!  the macro value string is the declaration past the =

character( len=*), intent( in) :: value_str

! **********************************************************************

!  entry: macro value

!  exit: macro's value is valid & may be assigned to macro

! **********************************************************************

!  verify_macro_value() local

! ----------------------------------------------------------------------

!  check all symbol names

   type( macro_t), pointer :: symbol_ptr

!  point to characters

   integer :: arg_idx

!  point to dummy args

   integer :: i

! **********************************************************************

!  verify_macro_value() text

continue

! ----------------------------------------------------------------------

!  check for null valued macros

   null_macro: if( len_trim( value_str) == 0 )then

      call msg_quit( "macro has null value")

   end if null_macro

! ----------------------------------------------------------------------

!  if warning, check that all the dummy args all appear in the macro value

   have_dummy_args: if( options% warning .and. allocated( arglist) )then

      scan_dummy_args: do i = 1, size( arglist)

         arg_idx = index( value_str, arglist( i))

         arg_not_used: if( arg_idx == 0 )then

            call msg_continue( "macro argument unused: " // arglist( i))

         end if arg_not_used

      end do scan_dummy_args

   end if have_dummy_args

! ----------------------------------------------------------------------

!  check that no existing macro ?name? does not appear in macro value- no recursion

   nullify( symbol_ptr)

   call get_next_macro( symbol_ptr)

   check_all_macros: do

      if( .not. associated( symbol_ptr) ) exit check_all_macros

      arg_idx = index( value_str, symbol_ptr% name_str)

      name_defined: if( arg_idx > 0 )then

         call msg_quit( "name in macro value has already been declared: " // symbol_ptr% name_str)

      end if name_defined

      call get_next_macro( symbol_ptr)

   end do check_all_macros

! ----------------------------------------------------------------------

!  verify_macro_value() exit

return

! **********************************************************************

!  verify_macro_value()

end subroutine verify_macro_value

! **********************************************************************
! **********************************************************************

!  process_getenv_declaration() process getenv declarations

subroutine process_getenv_declaration( ge_stmt)

! **********************************************************************

!  process_getenv_declaration() interface

! ----------------------------------------------------------------------

!  the statement containing the macro declaration

character( len= *), intent( in) :: ge_stmt

! **********************************************************************

!  entry: ge_stmt is blank_compress_lower_case logical declaration
!         past the getenv keyword "::..."

!  exit: macro declaration is processed or error exit

! **********************************************************************

!  process_macro_declaration() local

! ----------------------------------------------------------------------

!  name of symbol

   character( len= symbol_name_len) :: macro_name

!  results of decoding statement

   integer :: name_len

!  construct macro statement

   integer :: eq_idx

   character( len= buffer_len) :: ev_value

! **********************************************************************

!  process_macro_declaration() text

continue

! ----------------------------------------------------------------------

!  extract symbol name

   call get_macro_name( ge_stmt, macro_name, name_len)

!  if active line, process the declaration

   active_line: if( if_construct% now_selected )then

!  ensure no freeze is in effect

      if( options% freeze_declarations ) call msg_quit( "attempt to declare macro from environment after freeze")

!  get environment variable name from asis_statement

      eq_idx = index( asis_stmt, equals)

      no_eq: if( eq_idx == 0 )then

         call msg_quit( "no = found on getenv declaration: " // asis_stmt)

      end if no_eq

      call get_environment_value( adjustl( asis_stmt( eq_idx + 1: )), ev_value)

!  construct as-is macro statement

      asis_stmt( eq_idx + 1: ) = ev_value( 1: len( asis_stmt) - eq_idx)
      asis_len = len_trim( asis_stmt)

!  store symbol in symbol list

      call add_macro( ge_stmt, macro_name, .false.)

   end if active_line

! ----------------------------------------------------------------------

!  process_getenv_declaration() exit

return

! **********************************************************************

!  process_getenv_declaration()

end subroutine process_getenv_declaration

! **********************************************************************
! **********************************************************************

!  get_environment_value() get environment varaible value string

subroutine get_environment_value( ev_name, ev_value)

! **********************************************************************

!  get_pwd_environment_value() interface

! ----------------------------------------------------------------------

!  the environment variable name

character( len= *), intent( in) :: ev_name

! ----------------------------------------------------------------------

!  the environment variable value

character( len= buffer_len), intent( out) :: ev_value

! **********************************************************************

!  get_pwd_environment_value() local

! ----------------------------------------------------------------------

!  test command line status and length

   integer :: env_stat

! **********************************************************************

!  get_pwd_environment_value() text

! ----------------------------------------------------------------------

continue

! ----------------------------------------------------------------------

!  try to get environment variable value

   call get_environment_variable( name= ev_name, status= env_stat)

!  check for errors

   env_status_error: if( env_stat > 0 )then

      call msg_continue( "can't access environment variable: " // ev_name)

   end if env_status_error

!  check too long

   env_var_too_long: if( env_stat < 0 )then

      call msg_quit( "environment variable value too long for macro value: " // ev_name)

   end if env_var_too_long

!  ok to go

   call get_environment_variable( name= ev_name, value= ev_value)

! ----------------------------------------------------------------------

!  get_environment_value() exit

return

! **********************************************************************

!  get_environment_value()

end subroutine get_environment_value

! **********************************************************************
! **********************************************************************

!  %%% process coco text and copy

! **********************************************************************
! **********************************************************************

!  get_text_name() verify text name and determine its length

subroutine get_text_name( decl_stmt, text_name, name_len)

! **********************************************************************

!  get_text_name() interface

! ----------------------------------------------------------------------

!  the statement containing the text name

character( len= *), intent( in) :: decl_stmt

!  the text name

character( len= *), intent( out) :: text_name

!  the length of the text name

integer, intent( out) :: name_len

! **********************************************************************

!  entry: decl_stmt is blank_compress_lower_case declaration statement past the double colon
!         "name" | "name(..."

!  exit: name is valid and its length is known or error exit

! **********************************************************************

!  get_text_name() constants

! ----------------------------------------------------------------------

!  blank or open parenthesis may end a name

character( len= *), parameter :: end_of_name = blank // open_paren

! **********************************************************************

!  get_text_name() local

! ----------------------------------------------------------------------

!  pointers to characters in decl_stmt

   integer :: char_idx

! **********************************************************************

!  get_text_name() text

continue

! ----------------------------------------------------------------------

!  look for equals following separator

   char_idx = scan( decl_stmt, end_of_name)

!  if no equals found

   no_eq_op: if( char_idx == 0 )then

     call msg_quit( "can't find name in text statement: " // trim( decl_stmt))

   end if no_eq_op

   name_len = char_idx - 1

! ----------------------------------------------------------------------

!  check that name is not in use and has a valid form

   call valid_new_name( decl_stmt( 1: name_len))

   text_name = decl_stmt( 1: name_len)

! ----------------------------------------------------------------------

!  get_text_name() exit

return

! **********************************************************************

!  get_text_name()

end subroutine get_text_name

! **********************************************************************
! **********************************************************************

!  process_text_directive() process an text declaration

subroutine process_text_directive( text_dir, must_parens)

! **********************************************************************

!  process_text_directive() interface

! ----------------------------------------------------------------------

!  a statement containing a text directive

character( len= *), intent( in) :: text_dir

!  true if actual arguments must be in parenthesis

logical, intent( in) :: must_parens

! **********************************************************************

!  entry: text_dir is quoted text condition

!  exit: text code is stored in the text variable on the symbol list

! **********************************************************************

!  process_text_directive() local

! ----------------------------------------------------------------------

!  name of symbol

   character( len= symbol_name_len) :: text_name

!  results of decoding statement

   integer :: name_len

! **********************************************************************

!  process_text_directive() text

continue

! ----------------------------------------------------------------------

!  extract text name

   call get_text_name( text_dir, text_name, name_len)

!  if active block, process text declaration

   active_line: if( if_construct% now_selected )then

!  ensure no freeze is in effect

      if( options% freeze_declarations ) call msg_quit( "attempt to declare text after freeze")

!  add a block to the if-block list to ensure correct nesting

      call new_block( text_block)

!  store text in symbol list

      call add_text( text_dir( name_len + 1: ), text_name, must_parens)

!  end of text so remove text block from if block list

      call delete_block( text_block)

!  count text blocks

      total% text_blocks = total% text_blocks + 1

   end if active_line

! ----------------------------------------------------------------------

!  process_text_directive() exit

return

! **********************************************************************

!  process_text_directive()

end subroutine process_text_directive

! **********************************************************************
! **********************************************************************

!  add_text() copy text block to symbol table

subroutine add_text( text_decl_str, text_name, must_parens)

! **********************************************************************

!  add_text() interface

! ----------------------------------------------------------------------

!  a statement containing a text declaration

character( len= *), intent( in) :: text_decl_str

!  the name of the text

character( len= *), intent( in) :: text_name

!  true if actual args are wrapped in parentheses

logical, intent( in) :: must_parens

! **********************************************************************

!  entry: text_decl_str is blank_compress_lower_case logical declaration statement past the double colon
!         "" | "(...)"
!         text_name is the symbol name
!         must_parens is true if this is a constant declaration

!  exit: logical declaration is added to the logical symbol list or error exit

! **********************************************************************

!  add_text() local

! ----------------------------------------------------------------------

!  length of the dummy arglist

   integer :: arglist_len

!  type pointer to allocate

   type( text_t), pointer :: text_ptr

! **********************************************************************

!  add_text() text

continue

! ----------------------------------------------------------------------

!  allocate new text

   allocate( text_ptr, stat= state% status, errmsg= state% message)

   alloc_error: if( is_status_error( state) )then

      call msg_quit( "can't allocate text: " // trim( text_name))

   end if alloc_error

!  build new text on list

   subsequent_or_first: if( associated( first_symbol) )then

      last_symbol% next => text_ptr

      last_symbol => last_symbol% next

   else subsequent_or_first

      first_symbol => text_ptr

      last_symbol => first_symbol

   end if subsequent_or_first

   nullify( text_ptr% next)

! ----------------------------------------------------------------------

!  set symbol members

   text_ptr% name_str = text_name

   text_ptr% declared_file = get_class_file_name( current_file)
   text_ptr% declared_line = current_file% lines_transfered

   text_ptr% referenced = .false.
   text_ptr% referenced_file = null_string
   text_ptr% referenced_line = 0

! ----------------------------------------------------------------------

!  check for dummy argument list

   got_paren: if( text_decl_str( 1: len( open_paren)) == open_paren )then

!  arglist_len is zero if no close paren

      arglist_len = index( text_decl_str, close_paren)

      bad_arg_list_len: if( arglist_len <= len( open_paren // close_paren) )then

         call msg_quit( "bad text dummy argument list: " // trim( text_decl_str))

      end if bad_arg_list_len

!  must have something between parens

      call process_dummy_arglist( text_decl_str( len( open_paren) + 1: arglist_len - 1), text_ptr% dummy_args)

      allocate( text_ptr% actual_args( size( text_ptr% dummy_args)), stat= state% status, errmsg= state% message)

      actual_error: if( is_status_error( state) )then

         call msg_quit( "can't aloocate text actual args array: " // text_ptr% name_str)

      end if actual_error

   else got_paren

!  declared must have parenthesis on actual arguments but no dummy arguments

      parens_no_args: if( must_parens )then

         call msg_quit( "text actual args declared to need parens but no dummy args: " // text_ptr% name_str)

      end if parens_no_args

      arglist_len = 0

   end if got_paren

! ----------------------------------------------------------------------

!  must have nothing after name or close paren

   extra_chars: if( text_decl_str( arglist_len + 1: ) /= blank )then

      call msg_quit( "extra characters at end of text declaration: " // trim( text_decl_str))

   end if extra_chars

! ----------------------------------------------------------------------

!  assign the text value

   call assign_text_value( text_name, text_ptr% dummy_args, text_ptr% text_lines)

!  finish the text definition

   text_ptr% args_in_parens = must_parens

! ----------------------------------------------------------------------

!  add_text() exit

return

! **********************************************************************

!  add_text()

end subroutine add_text

! **********************************************************************
! **********************************************************************

!  assign_text_value() copy text block to symbol table

subroutine assign_text_value( text_name, args_array, lines_array)

! **********************************************************************

!  assign_text_value() interface

! ----------------------------------------------------------------------

!  the name of the text block

character( len= symbol_name_len), intent( in) :: text_name

!  the array to contain the text dummy args

type( target_t), dimension( :), allocatable, intent( in) :: args_array

!  the array to contain the text block

character( len= buffer_len), dimension( :), allocatable, intent( out) :: lines_array

! **********************************************************************

!  entry: the array dummy args

!  exit: the lines array is complete

! **********************************************************************

!  assign_text_value() constants

! ----------------------------------------------------------------------

!  end of a text block

character( len= *), parameter :: endtext_str = 'endtext'

! **********************************************************************

!  assign_text_value() local

! ----------------------------------------------------------------------

!  the text scratch file

   type( scratch_file_t) :: text_file

! ----------------------------------------------------------------------

!  copy buffer

   character( len= buffer_len) :: statement

!  lines of text in the text block

   integer :: i

   integer :: j

!  seek dummy argments in text block

   integer :: arg_idx

!  true when complete statement has been read

   logical :: complete

! ----------------------------------------------------------------------

!  line number string

   character( len= conversion_len) :: line_str

! **********************************************************************

!  assign_text_value() text

continue

! ----------------------------------------------------------------------

!  store text value (read into scratch file, count lines, allocate storage, copy to storage)

! ----------------------------------------------------------------------

!  open the set text file

   call text_file% open()

!  start as if with a complete statement

   complete = .true.

! ----------------------------------------------------------------------

!  main read text block lines loop

   read_lines: do

! ----------------------------------------------------------------------

!  read a text line from the current source file

      read_current: select type( current_file)

      type is( input_file_t) read_current

         call current_file% read()

      end select read_current

! ----------------------------------------------------------------------

!  read until end of file or complete statement

      read_eof: if( is_status_info( state) )then

         total% input_lines = total% input_lines + current_file% lines_transfered

         call msg_quit( "end of file encountered within text block")

      end if read_eof

!  update line predefined macro

      write( unit= line_str, fmt= conversion_fmt) current_file% lines_transfered

      predefined_macros( line_ss)% macro_value = line_str

      total% text_lines = total% text_lines + 1

!  write all lines to the output as coco lines

      call write_coco_line( output_file)

!  write the text line

      write( unit= text_file% io_unit, iostat= state% status, iomsg= state% message) text_file% line

      write_text: if( is_status_error( state) )then

         call msg_quit( "write text file failed: " // trim( text_file% line))

      end if write_text

! count text lines

      text_file% lines_transfered = text_file% lines_transfered + 1

! ----------------------------------------------------------------------

!  process coco lines

      coco_line: if( current_file% line( 1: len( coco_key)) == coco_key )then

!  count coco lines

         total% coco_lines = total% coco_lines + 1

!  ignore coco comments

         coco_statement: if( is_coco_statement( current_file% line( len( coco_key) + 1: )) )then

!  gather a complete statement

            call gather_coco_statement( current_file% line, statement, complete)

!  if incomplete, go get rest of statement

            got_statement: if( .not. complete )then

               cycle read_lines

            end if got_statement

! ----------------------------------------------------------------------

!  check for the end text statement

            end_text: if( trim( statement) == endtext_str &
                     .or. trim( statement) == endtext_str // trim( text_name) )then

               exit read_lines

            end if end_text

!  check for certain directives in the text block

            call verify_text_directive( statement)

         end if coco_statement

!  source lines

      else coco_line

         continuation_error: if( .not. complete )then

            call msg_quit( "source line in continued coco statement in text " // trim( text_name))

         end if continuation_error

!  end processing text statements

      end if coco_line

!  end main read set file lines loop

   end do read_lines

! ----------------------------------------------------------------------

!  check for no lines in text block or remove the end text statement

   null_text: if( text_file% lines_transfered < 2 )then

      call text_file% close()

      call msg_quit( "text block has no lines: " // trim( text_name))

   else null_text

      text_file% lines_transfered = text_file% lines_transfered - 1

   end if null_text

! ----------------------------------------------------------------------

!  allocate array for text

   allocate( lines_array( 1: text_file% lines_transfered), stat= state% status, errmsg= state% message)

   alloc_error: if( is_status_error( state) )then

      call msg_quit( "allocate text block failed")

   end if alloc_error

!  count text lines defined

   total% text_lines = total% text_lines + 1

!  rewind text scratch file

   call text_file% rewind()

!  copy text scratch file to array

   copy: do i = 1, size( lines_array)

      call text_file% read( lines_array( i))

   end do copy

!  close text scratch file

   call text_file% close()

!  verify whether each dummy arg appears in the text block somewhere

   has_dummy_args: if( options% warning .and. allocated( args_array) )then

      check_arg: do j = 1, size( args_array)

         arg_idx = 0

         check_line: do i = 1, size( lines_array)

            arg_idx = max( arg_idx, index( to_lower( lines_array( i)), args_array( j)) )

         end do check_line

         not_found: if( arg_idx == 0 )then

            call msg_continue( "dummy arg " // args_array( j) // " not found in text " // trim( text_name))

         end if not_found

      end do check_arg

   end if has_dummy_args

! ----------------------------------------------------------------------

!  assign_text_value() exit

return

! **********************************************************************

!  assign_text_value()

end subroutine assign_text_value

! **********************************************************************
! **********************************************************************

!  verify_text_directive() check that no invalid directives appear in a text block

subroutine verify_text_directive( text_stmt)

! **********************************************************************

!  verify_text_directive() interface

! ----------------------------------------------------------------------

!  a statement from a text block

character( len= *), intent( in) :: text_stmt

! **********************************************************************

!  entry: text_stmt is a blank_compress_lower_case coco directive past the coco key
!         which must contain only:
!         assignment | "stop" | "message" | "if..." | "else if..." |
!         "else" | "end if" | "assert..."

!  exit: if any other directive exists, the directive is found and flagged

! **********************************************************************

!  verify_text_directive() local

!  presence/location of equals

   integer :: eq_idx

!  walk symbol list

   class( symbol_t), pointer :: symbol_ptr

! **********************************************************************

!  verify_text_directive() text

continue

! ----------------------------------------------------------------------

!  detect assignment statements assigning to named variables

   nullify( symbol_ptr)

   eq_idx = index( text_stmt( 1: symbol_name_len + len( equals)), equals)

   got_equals: if( eq_idx > 0 )then

      call seek_symbol_name( text_stmt( 1: eq_idx - 1), symbol_ptr)

   end if got_equals

! ----------------------------------------------------------------------

!  which directive?

   which_directive: if( associated( symbol_ptr) )then

!  assignment is ok

      continue

! ----------------------------------------------------------------------

!  stop directive

   else if( text_stmt( 1: len( stop_str)) == stop_str )then which_directive

!  stop is ok

      continue

! ----------------------------------------------------------------------

!  message directive

   else if( text_stmt( 1: len( message_str)) == message_str )then which_directive

!  message is ok

      continue

! ----------------------------------------------------------------------

!  if directive

   else if( text_stmt( 1: len( if_str)) == if_str )then which_directive

!  process the if statement to check for correct nesting

      call process_if_directive( text_stmt( len( if_str) + 1: ))

!  just store the if in the text block- evaluate it when the block is copied

      if_construct% now_selected = .true.

! ----------------------------------------------------------------------

!  else if directive

   else if( text_stmt( 1: len( elseif_str)) == elseif_str )then which_directive

!  process the else if statement to check for correct nesting

      call process_elseif_directive( text_stmt( len( elseif_str) + 1: ))

!  just store the elseif in the text block- evaluate it when the block is copied

      if_construct% now_selected = .true.

! ----------------------------------------------------------------------

!  else directive

   else if( text_stmt( 1: len( else_str)) == else_str )then which_directive

!  process the else statement to check for correct nesting

      call process_else_directive( text_stmt( len( else_str) + 1: ))

!  just store the else in the text block- evaluate it when the block is copied

      if_construct% now_selected = .true.

! ----------------------------------------------------------------------

!  end if directive

   else if( text_stmt( 1: len( endif_str)) == endif_str )then which_directive

!  process the if statement to check for correct nesting

      call process_endif_directive( text_stmt( len( endif_str) + 1: ))

! ----------------------------------------------------------------------

!  assert statement

   else if( text_stmt( 1: len( assert_str)) == assert_str )then which_directive

!  assert is ok

      continue

! ----------------------------------------------------------------------

!  all else fails

   else which_directive

!  something other than a good one

      call msg_quit( "illegal directive in text block: " // trim( text_stmt))

! ----------------------------------------------------------------------

!  which directive?

   end if which_directive

! ----------------------------------------------------------------------

!  verify_text_directive() exit

return

! **********************************************************************

!  verify_text_directive()

end subroutine verify_text_directive

! **********************************************************************
! **********************************************************************

!  process_copy_directive() process a coco copy directive

subroutine process_copy_directive( copy_dir)

! **********************************************************************

!  process_copy_directive() interface

! ----------------------------------------------------------------------

!  a statement containing a copy directive

character( len= *), intent( in) :: copy_dir

! **********************************************************************

!  entry: copy directive

!  exit: the directive is processed or error exit

! **********************************************************************

!  process_copy_directive() constants

! ----------------------------------------------------------------------

!  mark beginning and end of text

character( len= *), parameter :: begin_txt = '?? ! text '

character( len= *), parameter :: end_txt = '?? ! end text '

! **********************************************************************

!  process_copy_directive() local

! ----------------------------------------------------------------------

!  use name from directive to find text block pointer

   character( len= symbol_name_len) :: text_name

!  length of text block name

   integer :: name_len

!  find beginning of name

   integer :: name_idx

!  find end of name

   integer :: end_name_idx

!  pointer to text block

   type( text_t), pointer :: text_ptr

!  gather a coco statement from the text block

   character( len= buffer_len) :: statement

!  expanded lines

   character( len= buffer_len) :: expand_line

!  loop through the text block lines

   integer :: i

!  find open parenthesis on copy directive

   integer :: open_paren_idx

!  find close parenthesis

   integer :: close_paren_idx

!  communicate with gather_statement()

   logical :: complete

! **********************************************************************

!  process_copy_directive() text

continue

! ----------------------------------------------------------------------

!  check for valid directive

   name_idx = 1

   call get_copy_name( copy_dir, text_name, name_len)

   end_name_idx = name_idx + name_len - 1

   call get_text_ptr( copy_dir( name_idx: end_name_idx), text_ptr)

! ----------------------------------------------------------------------

!  if active block, process text declaration

   active_line: if( if_construct% now_selected )then

      text_ptr% referenced = .true.
      text_ptr% referenced_file = get_class_file_name( current_file)
      text_ptr% referenced_line = current_file% lines_transfered

!  test first character after name

      open_paren_idx = end_name_idx + 1

!  check that if text has dummy args, copy has actual args, and vice versa

      have_args: if( allocated( text_ptr% dummy_args) )then

!  text with args

         no_args: if( copy_dir( open_paren_idx: open_paren_idx) /= open_paren )then

            call msg_quit( "no actual arguments for text: " // text_ptr% name_str)

         end if no_args

         open_paren_idx = index( asis_stmt, open_paren)

         call seek_close_paren( asis_stmt, open_paren_idx, close_paren_idx)

         no_close_paren: if( close_paren_idx > len_trim( asis_stmt) )then

            call msg_quit( "can't find close parenthesis in copy directive: " // trim( asis_stmt))

         end if no_close_paren

         call make_actual_array( asis_stmt( open_paren_idx + 1: close_paren_idx - 1) // comma, text_ptr% actual_args, &
                                 text_ptr% args_in_parens)

      else have_args

!  text without args

         got_args: if( copy_dir( open_paren_idx: open_paren_idx) == open_paren )then

            call msg_quit( "no dummy arguments for text: " // trim( copy_dir))

         end if got_args

!  block has/has not args

      end if have_args

! ----------------------------------------------------------------------

!  mark the beginning of the text

      line = begin_txt // text_ptr% name_str

      call write_coco_line( output_file)

! ----------------------------------------------------------------------

!  loop thru text block lines

      copy_lines: do i = 1, size( text_ptr% text_lines)

         line = text_ptr% text_lines( i)

!  coco lines or source lines

         coco_lines: if( line( 1: len( coco_key)) == coco_key )then

!  write coco line to the output

            call write_coco_line( output_file)

! ----------------------------------------------------------------------

!  process coco lines, ignore coco comments

            coco_construct: if( is_coco_statement( line( len( coco_key) + 1: )) )then

!  gather a complete coco statement

               call gather_coco_statement( line, statement, complete)

!  if not yet a complete statement, get next line

               incomplete: if( .not. complete )then

                  cycle copy_lines

               end if incomplete

!  process (permitted in a block) directives

               call process_block_directive( statement)

               output_file% line => line

            end if coco_construct

! ----------------------------------------------------------------------

!  source lines

         else coco_lines

!  allow for macro expansion to lengthen the line

            expand_line = text_ptr% text_lines( i)

!  if args substitute in text line

            go_args: if( allocated( text_ptr% dummy_args) )then

               call process_actual_arglist( text_ptr% actual_args, &
                                            expand_line, text_ptr% text_lines( i), text_ptr% dummy_args)

               if( options% wrapping_lines ) call wrap_source_line( expand_line)

            end if go_args

!  if ? present, edit source line

            edit_line_args: if( index( expand_line, arg_key) > 0 )then

               call edit_source_line( expand_line)

            end if edit_line_args

!  finally, write out the line

            line = expand_line( 1: len( line))

            call write_source_line( output_file)

         end if coco_lines

      end do copy_lines

      total% copied_lines = total% copied_lines + size( text_ptr% text_lines)

!  mark the end of the text

      line = end_txt // text_ptr% name_str

      call write_coco_line( output_file)

!  process active lines only

   end if active_line

! ----------------------------------------------------------------------

!  process_copy_directive() exit

return

! **********************************************************************

!  process_copy_directive()

end subroutine process_copy_directive

! **********************************************************************
! **********************************************************************

!  get_copy_name() verify text name and determine its length

subroutine get_copy_name( decl_stmt, text_name, name_len)

! **********************************************************************

!  get_copy_name() interface

! ----------------------------------------------------------------------

!  the statement containing the text name

character( len= *), intent( in) :: decl_stmt

!  the text name

character( len= *), intent( out) :: text_name

!  the length of the text name

integer, intent( out) :: name_len

! **********************************************************************

!  entry: decl_stmt is blank_compress_lower_case declaration statement past the double colon
!         "name" | "name(..."

!  exit: name is valid and its length is known or error exit

! **********************************************************************

!  get_copy_name() constants

! ----------------------------------------------------------------------

!  blank or open parenthesis may end a name

character( len= *), parameter :: end_of_name = blank // open_paren

! **********************************************************************

!  get_copy_name() local

! ----------------------------------------------------------------------

!  pointers to characters in decl_stmt

   integer :: char_idx

! **********************************************************************

!  get_copy_name() text

continue

! ----------------------------------------------------------------------

!  look for equals following separator

   char_idx = scan( decl_stmt, end_of_name)

!  if no equals found

   no_eq_op: if( char_idx == 0 )then

     call msg_quit( "can't find name in copy statement: " // trim( decl_stmt))

   end if no_eq_op

   name_len = char_idx - 1

! ----------------------------------------------------------------------

!  return the name

   text_name = decl_stmt( 1: name_len)

! ----------------------------------------------------------------------

!  get_copy_name() exit

return

! **********************************************************************

!  get_copy_name()

end subroutine get_copy_name

! **********************************************************************
! **********************************************************************

!  process_block_directive() process a coco text block directive

subroutine process_block_directive( block_stmt)

! **********************************************************************

!  process_block_directive() interface

! ----------------------------------------------------------------------

!  a statement from a text block

character( len= *), intent( in) :: block_stmt

! **********************************************************************

!  entry: coco_stmt is a blank_compress_lower_case coco directive past the coco key
!         "stop..." | "message..." | "if..." | "else if..." | "else..." |
!         "end if..." | "assert..." | "name=..."

!  exit: the directive is processed or error exit

! **********************************************************************

!  process_block_directive() local

! ----------------------------------------------------------------------

!  point to location of symbol

   class( symbol_t), pointer :: symbol_ptr

!  specific type pointers

   type( integer_t), pointer :: integer_ptr
   type( logical_t), pointer :: logical_ptr

!  pointer to equals

   integer :: eq_idx

!  expression string is after the equals

   integer :: expr_idx

! **********************************************************************

!  process_block_directive() text

continue

! ----------------------------------------------------------------------

!  which directive?

! ----------------------------------------------------------------------

!  detect assignment statements assigning to variables named by keywords

      nullify( symbol_ptr)

      eq_idx = scan( block_stmt( 1: symbol_name_len + 1), equals)

      got_equals: if( eq_idx > 0 )then

         call seek_symbol_name( block_stmt( 1: eq_idx - 1), symbol_ptr)

      end if got_equals

! ----------------------------------------------------------------------

!  which directive?

! ----------------------------------------------------------------------

!  assignment directive

   which_directive: if( associated( symbol_ptr) )then

!  up to the equals must be a declared name

      expr_idx = eq_idx + len( equals)

!  must be an integer or logical variable

      integer_or_logical_or_error: select type( symbol_ptr)

      type is( integer_t) integer_or_logical_or_error

         integer_ptr => symbol_ptr

         call process_integer_assignment( block_stmt( expr_idx: ), integer_ptr)

      type is( logical_t) integer_or_logical_or_error

         logical_ptr => symbol_ptr

         call process_logical_assignment( block_stmt( expr_idx: ), logical_ptr)

      class default integer_or_logical_or_error

         call msg_quit( "assignment variable must be an integer or a logical: " // symbol_ptr% name_str)

      end select integer_or_logical_or_error

      nullify( symbol_ptr)

! ----------------------------------------------------------------------

!  stop directive

   else if( block_stmt( 1: len( stop_str)) == stop_str )then which_directive

      call process_stop_directive( block_stmt( len( stop_str) + 1: ) )

! ----------------------------------------------------------------------

!  message directive

   else if( block_stmt( 1: len( message_str)) == message_str )then which_directive

      call process_message_directive( block_stmt( len( message_str) + 1: ) )

! ----------------------------------------------------------------------

!  if directive

   else if( block_stmt( 1: len( if_str)) == if_str )then which_directive

      call process_if_directive( block_stmt( len( if_str) + 1: ) )

! ----------------------------------------------------------------------

!  else if directive

   else if( block_stmt( 1: len( elseif_str)) == elseif_str )then which_directive

      call process_elseif_directive( block_stmt( len( elseif_str) + 1: ) )

! ----------------------------------------------------------------------

!  else directive

   else if( block_stmt( 1: len( else_str)) == else_str )then which_directive

      call process_else_directive( block_stmt( len( else_str) + 1: ) )

! ----------------------------------------------------------------------

!  endif directive

   else if( block_stmt( 1: len( endif_str)) == endif_str )then which_directive

      call process_endif_directive( block_stmt( len( endif_str) + 1: ) )

! ----------------------------------------------------------------------

!  assert declaration

   else if( block_stmt( 1: len( assert_str)) == assert_str )then which_directive

      call process_assert_directive( block_stmt( len( assert_str) + 1: ))

! ----------------------------------------------------------------------

!  cannot process this directive

   else which_directive

         call msg_quit( "error: unknown block directive: " // trim( block_stmt))

! ----------------------------------------------------------------------

!  which directive?

   end if which_directive

! ----------------------------------------------------------------------

!  process_block_directive() exit

return

! **********************************************************************

!  process_block_directive()

end subroutine process_block_directive

! **********************************************************************
! **********************************************************************

!  get_text_ptr() seek symbol on symbol list

subroutine get_text_ptr( name_str, text_ptr)

! **********************************************************************

!  get_text_ptr() interface

! ----------------------------------------------------------------------

!  the name of the text whose pointer is sought

character( len= *), intent( in) :: name_str

!  the pointer to the text

type( text_t), pointer :: text_ptr

! **********************************************************************

!  entry: symbol_str is blank_compress_lower_case text symbol name

!  exit: symbol found or not in text symbol array

! **********************************************************************

!  get_text_ptr() local

   class( symbol_t), pointer :: symbol_ptr

! **********************************************************************

!  get_text_ptr() text

continue

! ----------------------------------------------------------------------

!  search symbol list

   symbol_ptr => first_symbol

   search_list: do

      if( .not. associated( symbol_ptr) ) exit search_list

      check_text: select type( symbol_ptr)

      type is( text_t) check_text

         name_match: if( name_str == get_name_str( symbol_ptr% name_str) )then

            text_ptr => symbol_ptr

            return

         end if name_match

      end select check_text

      symbol_ptr => symbol_ptr% next

   end do search_list

! ----------------------------------------------------------------------

!  text not found

   call msg_quit( "unknown text: " // trim( name_str))

! ----------------------------------------------------------------------

!  get_text_ptr() exit

return

! **********************************************************************

!  get_text_ptr()

end subroutine get_text_ptr

! **********************************************************************
! **********************************************************************

!  get_open_name() verify open file name and determine its length

subroutine get_open_name( decl_stmt, file_name, name_len)

! **********************************************************************

!  get_open_name() interface

! ----------------------------------------------------------------------

!  the directive containing the macro

character( len= *), intent( in) :: decl_stmt

!  the name of the file

character( len= *), intent( out) :: file_name

!  the length of the file name

integer, intent( out) :: name_len

! **********************************************************************

!  entry: decl_stmt is blank_compress_lower_case declaration statement past the double colon
!         "name=..." | "name(..."

!  exit: name is valid and its length is known or error exit

! **********************************************************************

!  get_open_name() constants

! ----------------------------------------------------------------------

!  equals must end a file name

character( len= *), parameter :: end_of_name = equals

! **********************************************************************

!  get_open_name() local

! ----------------------------------------------------------------------

!  pointers to characters in decl_stmt

   integer :: char_idx

! **********************************************************************

!  get_open_name() text

continue

! ----------------------------------------------------------------------

!  look for equals following separator

   char_idx = scan( decl_stmt, end_of_name)

!  if no equals found

   no_eq_op: if( char_idx == 0 )then

     call msg_quit( "can't find name in open declaration: " // trim( decl_stmt))

   end if no_eq_op

   name_len = char_idx - 1

! ----------------------------------------------------------------------

!  check that name is not in use and has a valid form

   call valid_new_name( decl_stmt( 1: name_len))

   file_name = decl_stmt( 1: name_len)

! ----------------------------------------------------------------------

!  get_open_name() exit

return

! **********************************************************************

!  get_open_name()

end subroutine get_open_name

! **********************************************************************
! **********************************************************************

!  process_open_declaration() process open declarations

subroutine process_open_directive( open_stmt)

! **********************************************************************

!  process_open_directive() interface

! ----------------------------------------------------------------------

!  the statement containing the open declaration

character( len= *), intent( in) :: open_stmt

! **********************************************************************

!  entry: open_stmt is blank_compress_lower_case open declaration
!         past the macro keyword "::..."

!  exit: macro declaration is processed or error exit

! **********************************************************************

!  process_open_directive() local

! ----------------------------------------------------------------------

!  name of symbol

   character( len= symbol_name_len) :: file_name

!  results of decoding statement

   integer :: name_len

! **********************************************************************

!  process_open_directive() text

continue

! ----------------------------------------------------------------------

!  extract symbol name

   call get_open_name( open_stmt, file_name, name_len)

!  if active line, process the declaration

   active_line: if( if_construct% now_selected )then

!  ensure no freeze is in effect

      if( options% freeze_declarations ) call msg_quit( "attempt to open file after freeze")

!  store symbol in symbol list

      call add_file( open_stmt( name_len + 1: ), file_name)

   end if active_line

! ----------------------------------------------------------------------

!  process_open_directive() exit

return

! **********************************************************************

!  process_open_directive()

end subroutine process_open_directive

! **********************************************************************
! **********************************************************************

!  add_file() store file declaration in symbol table

subroutine add_file( open_decl_str, file_var_name)

! **********************************************************************

!  add_file() interface

! ----------------------------------------------------------------------

!  the statement containing the declaration

character( len= *), intent( in) :: open_decl_str

!  the symbol name

character( len= *), intent( in) :: file_var_name

! **********************************************************************

!  entry: int_decl_str is blank_compress_lower_case open declaration statement past the name
!         "=..."
!         sym_name is the symbol name
!         is_const is true if this is a constant declaration

!  exit: macro declaration is added to the macro symbol list or error exit

! **********************************************************************

!  add_file() local

! ----------------------------------------------------------------------

!  type pointer to allocate

   type( file_symbol_t), pointer :: file_ptr

!  index of the =

   integer :: eq_idx

!  index of the =

   character( len= file_name_len) :: unquoted_name

!  length of the file name

   integer :: quoted_len
   integer :: unquoted_len

! **********************************************************************

!  add_file() text

continue

! ----------------------------------------------------------------------

!  allocate new macro

   allocate( file_ptr, stat= state% status, errmsg= state% message)

   alloc_error: if( is_status_error( state) )then

      call msg_quit( "can't allocate file symbol: " // trim( file_var_name))

   end if alloc_error

!  build new macro on list

   subsequent_or_first: if( associated( first_symbol) )then

      last_symbol% next => file_ptr

      last_symbol => last_symbol% next

   else subsequent_or_first

      first_symbol => file_ptr

      last_symbol => first_symbol

   end if subsequent_or_first

   nullify( file_ptr% next)

! ----------------------------------------------------------------------

!  set symbol members

   file_ptr% name_str = file_var_name

   file_ptr% declared_file = get_class_file_name( current_file)
   file_ptr% declared_line = current_file% lines_transfered

   file_ptr% referenced = .false.
   file_ptr% referenced_file = null_string
   file_ptr% referenced_line = 0

! ----------------------------------------------------------------------

!  must have equals

   eq_idx = index( open_decl_str, equals)

   no_eq: if( eq_idx == 0 )then

      call msg_quit( "a open definition must have an equals sign: " // trim( open_decl_str))

   end if no_eq

!  assign the file name value from the declaration

   unquoted_name = open_decl_str( 2: )

   call unquote_string( unquoted_name, file_ptr% file% name_str, unquoted_len, quoted_len)

   chars_after_fn: if( open_decl_str( unquoted_len + 2: ) /= blank )then

      call msg_quit( "extra characters following open file name: " // open_decl_str)

   end if chars_after_fn

!  open the file for output

   call file_ptr% file% open()

! ----------------------------------------------------------------------

!  add_file() exit

return

! **********************************************************************

!  add_file()

end subroutine add_file

! **********************************************************************
! **********************************************************************

!  process_output_directive() process output directive

subroutine process_output_directive( output_dir)

! **********************************************************************

!  process_output_directive() interface

! ----------------------------------------------------------------------

!  the output directive from the set file

character( len= *), intent( in) :: output_dir

! **********************************************************************

!  entry: output_dir is a file variable

!  exit: output_dir is set

! **********************************************************************

!  process_output_directive() local

! ----------------------------------------------------------------------

!  the name of the file to be opened

   character( len= file_name_len) :: output_name

!  a pointer to a file_symbol_t

   type( file_symbol_t), pointer :: file_ptr

! **********************************************************************

!  process_output_directive() text

continue

! ----------------------------------------------------------------------

!  process output directive if on an active line

   active_line: if( if_construct% now_selected ) then

!  if file-var is on the output line

      got_file_var: if( output_dir( 1: 1) == colon )then

!  find file symbol

         output_name = output_dir( 2: )

         call get_file_ptr( output_name, file_ptr)

!  if a current alternate output, unset it

         got_one_now: if( associated( alternate_output) )then

            call delete_block( phase= output_block)

            alternate_output% active = .false.

            nullify( alternate_output)

         end if got_one_now

!  record the reference

         file_ptr% referenced = .true.
         file_ptr% referenced_file = get_class_file_name( current_file)
         file_ptr% referenced_line = current_file% lines_transfered

         call new_block( phase= output_block)

!  make active and point alternate file to its file

         file_ptr% active = .true.

         alternate_output => file_ptr

!  no file-var on the output line

      else got_file_var

         call delete_block( phase= output_block)

         alternate_output% active = .false.

         nullify( alternate_output)

      end if got_file_var

   end if active_line

! ----------------------------------------------------------------------

!  process_output_directive() exit

return

! **********************************************************************

!  process_output_directive()

end subroutine process_output_directive

! **********************************************************************
! **********************************************************************

!  get_file_ptr() seek symbol on symbol list

subroutine get_file_ptr( name_str, file_ptr)

! **********************************************************************

!  get_file_ptr() interface

! ----------------------------------------------------------------------

!  the name of the text whose pointer is sought

character( len= *), intent( in) :: name_str

!  the pointer to the text

type( file_symbol_t), pointer :: file_ptr

! **********************************************************************

!  entry: symbol_str is blank_compress_lower_case text symbol name

!  exit: symbol found or not in text symbol array

! **********************************************************************

!  get_file_ptr() local

   class( symbol_t), pointer :: symbol_ptr

! **********************************************************************

!  get_file_ptr() text

continue

! ----------------------------------------------------------------------

!  search symbol list

   symbol_ptr => first_symbol

   search_list: do

      if( .not. associated( symbol_ptr) ) exit search_list

      check_file: select type( symbol_ptr)

      type is( file_symbol_t) check_file

         name_match: if( name_str == get_name_str( symbol_ptr% name_str) )then

            file_ptr => symbol_ptr

            return

         end if name_match

      end select check_file

      symbol_ptr => symbol_ptr% next

   end do search_list

! ----------------------------------------------------------------------

!  file not found

   call msg_quit( "unknown file: " // trim( name_str))

! ----------------------------------------------------------------------

!  get_file_ptr() exit

return

! **********************************************************************

!  get_file_ptr()

end subroutine get_file_ptr

! **********************************************************************
! **********************************************************************

!  process_document_directive() process a coco stop directive

subroutine process_document_directive( document_dir)

! **********************************************************************

!  process_document_directive() interface

! ----------------------------------------------------------------------

!  document string

character( len= *), intent( in) :: document_dir

! **********************************************************************

!  process_document_directive() constants

! ----------------------------------------------------------------------

!  document text

!  changed for NONMEM!

character( len= *), dimension( 1: 16), parameter :: document_text = [ &
                            ';                                     ', &
                            '; Preprocessor executed: ?date? ?time?', &
                            ';                                     ', &
                            '; Preprocessor command line: ?cmdline?', &
                            '; Preprocessor set file: ?setfile?    ', &
                            '; Preprocessor log file: ?logfile?    ', &
                            '; Preprocessor version: ?coco?        ', &
                            ';                                     ', &
                            '; Source file: ?file? line: ?line?    ', &
                            '; Compile file: ?output?              ', &
                            '; Include path: ?incpath?             ', &
                            '; Freeze file: ?freeze?               ', &
                            ';                                     ', &
                            '; User: ?user?                        ', &
                            '; Current directory: ?cwd?            ', &
                            ';                                     ' ]


! **********************************************************************

!  process_document_directive() local

! ----------------------------------------------------------------------

!  loop through document text

   integer :: i

! **********************************************************************

!  process_document_directive() text

continue

! ----------------------------------------------------------------------

!  check for extra characters

   extra_chars: if( document_dir /= blank )then

      call msg_quit( "extra characters at end of document directive: " // trim( document_dir))

   end if extra_chars

! ----------------------------------------------------------------------

!  process assignment directive if on an active line

   active_line: if( if_construct% now_selected ) then

      text_to_output: do i = 1, size( document_text)

         line = document_text( i)

!  if ? present, edit source line

         edit_line: if( index( document_text( i), arg_key) > 0 )then

            call edit_source_line( line)

         end if edit_line

!  copy source lines

         call write_source_line( output_file)

      end do text_to_output

   end if active_line

! ----------------------------------------------------------------------

!  process_document_directive() exit

return

! **********************************************************************

!  process_document_directive()

end subroutine process_document_directive

! **********************************************************************
! **********************************************************************

!  process_options_directive() process a coco options directive

subroutine process_options_directive( options_dir)

! **********************************************************************

!  process_options_directive() interface

! ----------------------------------------------------------------------

!  the options directive

character( len= *), intent( in) :: options_dir

! **********************************************************************

!  entry: options_dir is blank_compress_lower_case coco options directive, past the coco key word

!  exit: coco processing optionss

! **********************************************************************

!  process_options_directive() local

   character( len= buffer_len) :: location_line

   character( len= conversion_len) :: line_str

! **********************************************************************

!  process_options_directive() text

continue

! ----------------------------------------------------------------------

!  check for extra characters at end of directive

   extra_chars: if( options_dir /= blank )then

      call msg_quit( "extra characters at end of options directive: " // trim( options_dir))

   end if extra_chars

!  process options directive if on an active line

   active_line: if( if_construct% now_selected )then

      write( unit= line_str, fmt= conversion_fmt) current_file% lines_transfered

! typo directuve corrected here and elsewhere (erik)

      location_line = "option directive: " // trim( get_class_file_name( current_file)) // ": " // trim( adjustl( line_str))

      write( unit= log_file% io_unit, fmt= string_fmt) trim( location_line)

      call write_options( log_file)

   end if active_line

! ----------------------------------------------------------------------

!  process_options_directive() exit

return

! **********************************************************************

!  process_options_directive()

end subroutine process_options_directive

! **********************************************************************
! **********************************************************************

!  process_report_directive() process a coco report directive

subroutine process_report_directive( report_dir)

! **********************************************************************

!  process_report_directive() interface

! ----------------------------------------------------------------------

!  the report directive

character( len= *), intent( in) :: report_dir

! **********************************************************************

!  entry: report_dir is blank_compress_lower_case coco report directive, past the coco key word

!  exit: coco processing reports

! **********************************************************************

!  process_report_directive() local

   character( len= buffer_len) :: location_line

   character( len= conversion_len) :: line_str

! **********************************************************************

!  process_report_directive() text

continue

! ----------------------------------------------------------------------

!  check for extra characters at end of directive

   extra_chars: if( report_dir /= blank )then

      call msg_quit( "extra characters at end of report directive: " // trim( report_dir))

   end if extra_chars

!  process report directive if on an active line

   active_line: if( if_construct% now_selected )then

      write( unit= line_str, fmt= conversion_fmt) current_file% lines_transfered

      location_line = "report directive: " // trim( get_class_file_name( current_file)) // ": " // trim( adjustl( line_str))

      write( unit= log_file% io_unit, fmt= string_fmt) trim( location_line)

      call write_report()

   end if active_line

! ----------------------------------------------------------------------

!  process_report_directive() exit

return

! **********************************************************************

!  process_report_directive()

end subroutine process_report_directive

! **********************************************************************
! **********************************************************************

!  process_symbols_directive() process a coco symbols directive

subroutine process_symbols_directive( symbols_dir)

! **********************************************************************

!  process_symbols_directive() interface

! ----------------------------------------------------------------------

!  the symbols directive

character( len= *), intent( in) :: symbols_dir

! **********************************************************************

!  entry: symbols_dir is blank_compress_lower_case coco symbols directive, past the coco key word

!  exit: coco processing symbolss

! **********************************************************************

!  process_symbols_directive() local

   character( len= buffer_len) :: location_line

   character( len= conversion_len) :: line_str

! **********************************************************************

!  process_symbols_directive() text

continue

! ----------------------------------------------------------------------

!  check for extra characters at end of directive

   extra_chars: if( symbols_dir /= blank )then

      call msg_quit( "extra characters at end of symbols directive: " // trim( symbols_dir))

   end if extra_chars

!  process symbols directive if on an active line

   active_line: if( if_construct% now_selected )then

      write( unit= line_str, fmt= conversion_fmt) current_file% lines_transfered

      location_line = "symbol directive: " // trim( get_class_file_name( current_file)) // ": " // trim( adjustl( line_str))

      write( unit= log_file% io_unit, fmt= string_fmt) trim( location_line)

      call write_symbols()

   end if active_line

! ----------------------------------------------------------------------

!  process_symbols_directive() exit

return

! **********************************************************************

!  process_symbols_directive()

end subroutine process_symbols_directive

! **********************************************************************
! **********************************************************************

!  valid_new_name() returns if string is a valid name

subroutine valid_new_name( string)

! **********************************************************************

!  valid_new_name() interface

! ----------------------------------------------------------------------

!  the name to be checked

character( len= *), intent( in) :: string

! ----------------------------------------------------------------------

!  predefined macro index

   integer :: i

!  character index

   integer :: char_idx

!  search symbol list

   class( symbol_t), pointer :: symbol_ptr

! ----------------------------------------------------------------------

!  valid_new_name() text

continue

! ----------------------------------------------------------------------

!  check that initial character is alphabetic

   char_idx = verify( string( 1: 1), alpha_chars)

   initial_ok: if( char_idx > 0 )then

      call msg_quit( "illegal initial character in name: " // string )

   end if initial_ok

!  check that following characters are legal

   char_idx = verify( string( 2: ), alphanum_chars)

   name_ok: if( char_idx > 0 )then

      call msg_quit( "illegal character in name: " // string )

   end if name_ok

! ----------------------------------------------------------------------

!  check that name isn't predefined

   not_predefined: do i = 1, size( predefined_macros)

      not_name: if( string == trim_name( predefined_macros( i)% name_str) )then

         call msg_quit( "attempt to redefine a predefined macro: " // string)

      end if not_name

   end do not_predefined

! ----------------------------------------------------------------------

!  check that name doesn't exist on list yet

   nullify( symbol_ptr)

   call seek_symbol_name( string, symbol_ptr)

   duplicate_name: if( associated( symbol_ptr) )then

      call msg_quit( "attempt to declare duplicate name: " // string )

   end if duplicate_name

! ----------------------------------------------------------------------

!  valid_new_name() exit

return

! **********************************************************************

!  valid_new_name()

end subroutine valid_new_name

! **********************************************************************
! **********************************************************************

!  valid_new_cl_name() returns if string is a valid name

subroutine valid_new_cl_name( string)

! **********************************************************************

!  valid_new_cl_name() interface

! ----------------------------------------------------------------------

!  the name to be checked

character( len= *), intent( in) :: string

! ----------------------------------------------------------------------

!  index predefined macros

   integer :: i

!  character index

   integer :: char_idx

!  search symbol list

   class( symbol_t), pointer :: symbol_ptr

! ----------------------------------------------------------------------

!  valid_new_cl_name() text

continue

! ----------------------------------------------------------------------

!  check that initial character is alphabetic

   char_idx = verify( string( 1: 1), alpha_chars)

   initial_ok: if( char_idx > 0 )then

      call msg_quit( "illegal initial character in name: " // string )

   end if initial_ok

!  check that following characters are legal

   char_idx = verify( string( 2: ), alphanum_chars)

   name_ok: if( char_idx > 0 )then

      call msg_quit( "illegal character in name: " // string )

   end if name_ok

! ----------------------------------------------------------------------

!  check that name isn't predefined

   not_predefined: do i = 1, size( predefined_macros)

      not_name: if( string == trim_name( predefined_macros( i)% name_str) )then

         call msg_quit( "command line attempt to redefine a predefined macro: " // string)

      end if not_name

   end do not_predefined

! ----------------------------------------------------------------------

!  check that name doesn't exist on list yet

   nullify( symbol_ptr)

   call seek_cl_symbol_name( string, symbol_ptr)

   duplicate_name: if( associated( symbol_ptr) )then

      call msg_quit( "attempt to declare duplicate command line name: " // string )

   end if duplicate_name

! ----------------------------------------------------------------------

!  valid_new_cl_name() exit

return

! **********************************************************************

!  valid_new_cl_name()

end subroutine valid_new_cl_name

! **********************************************************************
! **********************************************************************

!  valid_new_sf_name() returns if string is a valid name

subroutine valid_new_sf_name( string)

! **********************************************************************

!  valid_new_sf_name() interface

! ----------------------------------------------------------------------

!  the name to be checked

character( len= *), intent( in) :: string

! ----------------------------------------------------------------------

!  index predefined macros

   integer :: i

!  character index

   integer :: char_idx

!  search symbol list

   class( symbol_t), pointer :: symbol_ptr

! ----------------------------------------------------------------------

!  valid_new_sf_name() text

continue

! ----------------------------------------------------------------------

!  check that initial character is alphabetic

   char_idx = verify( string( 1: 1), alpha_chars)

   initial_ok: if( char_idx > 0 )then

      call msg_quit( "illegal initial character in name: " // string)

   end if initial_ok

!  check that following characters are legal

   char_idx = verify( string( 2: ), alphanum_chars)

   name_ok: if( char_idx > 0 )then

      call msg_quit( "illegal character in name: " // string)

   end if name_ok

! ----------------------------------------------------------------------

!  check that name isn't predefined

   not_predefined: do i = 1, size( predefined_macros)

      not_name: if( string == trim_name( predefined_macros( i)% name_str) )then

         call msg_quit( "set file attempt to redefine a predefined macro: " // string)

      end if not_name

   end do not_predefined

! ----------------------------------------------------------------------

!  check that name doesn't exist on list yet

   nullify( symbol_ptr)

   call seek_sf_symbol_name( string, symbol_ptr)

   duplicate_name: if( associated( symbol_ptr) )then

      call msg_quit( "attempt to declare duplicate set file name: " // string)

   end if duplicate_name

! ----------------------------------------------------------------------

!  valid_new_sf_name() exit

return

! **********************************************************************

!  valid_new_sf_name()

end subroutine valid_new_sf_name

! **********************************************************************
! **********************************************************************

!  get_int_value_from_set_file() overwrites the integer value with one from the set file

subroutine get_int_value_from_set_file( integer_ptr)

! **********************************************************************

!  get_int_value_from_set_file() interface

! ----------------------------------------------------------------------

!  the name to be checked

type( integer_t), pointer :: integer_ptr

! ----------------------------------------------------------------------

!  search symbol list

   class( symbol_t), pointer :: symbol_ptr

! ----------------------------------------------------------------------

!  get_int_value_from_set_file() text

continue

! ----------------------------------------------------------------------

!  check each symbol on the set file symbol list

   symbol_ptr => first_sf_symbol

   check_all_symbols: do

      if( .not. associated( symbol_ptr ) ) exit check_all_symbols

      names_match: if( symbol_ptr% name_str == integer_ptr% name_str )then

         types_match: select type( symbol_ptr)

         type is( integer_t) types_match

            constant_eqv: if( symbol_ptr% constant .eqv. integer_ptr% constant )then

               integer_ptr% defined = .true.
               integer_ptr% defined_file = symbol_ptr% defined_file
               integer_ptr% defined_line = symbol_ptr% defined_line
               integer_ptr% sf_defined = .true.
               integer_ptr% integer_value = symbol_ptr% integer_value

            else constant_eqv

               call msg_quit( "parameter mismatch set file versus source file: " // integer_ptr% name_str)

            end if constant_eqv

         class default types_match

            call msg_quit( "type mismatch set file versus source: " // integer_ptr% name_str)

         end select types_match

      end if names_match

      symbol_ptr => symbol_ptr% next

   end do check_all_symbols

! ----------------------------------------------------------------------

!  get_int_value_from_set_file() exit

return

! **********************************************************************

!  get_int_value_from_set_file()

end subroutine get_int_value_from_set_file

! **********************************************************************
! **********************************************************************

!  get_log_value_from_set_file() overwrites the logical value with one from the set file

subroutine get_log_value_from_set_file( logical_ptr)

! **********************************************************************

!  get_log_value_from_set_file() interface

! ----------------------------------------------------------------------

!  the name to be checked

type( logical_t), pointer :: logical_ptr

! ----------------------------------------------------------------------

!  search symbol list

   class( symbol_t), pointer :: symbol_ptr

! ----------------------------------------------------------------------

!  get_log_value_from_set_file() text

continue

! ----------------------------------------------------------------------

!  check each symbol on the set file symbol list

   symbol_ptr => first_sf_symbol

   check_all_symbols: do

      if( .not. associated( symbol_ptr ) ) exit check_all_symbols

      names_match: if( symbol_ptr% name_str == logical_ptr% name_str )then

         types_match: select type( symbol_ptr)

         type is( logical_t) types_match

            constant_eqv: if( symbol_ptr% constant .eqv. logical_ptr% constant )then

               logical_ptr% defined = .true.
               logical_ptr% defined_file = symbol_ptr% defined_file
               logical_ptr% defined_line = symbol_ptr% defined_line
               logical_ptr% sf_defined = .true.
               logical_ptr% logical_value = symbol_ptr% logical_value

            else constant_eqv

               call msg_quit( "parameter mismatch set file versus source file: " // logical_ptr% name_str)

            end if constant_eqv

         class default types_match

            call msg_quit( "type mismatch set file versus source: " // logical_ptr% name_str)

         end select types_match

      end if names_match

      symbol_ptr => symbol_ptr% next

   end do check_all_symbols

! ----------------------------------------------------------------------

!  get_log_value_from_set_file() exit

return

! **********************************************************************

!  get_log_value_from_set_file()

end subroutine get_log_value_from_set_file

! **********************************************************************
! **********************************************************************

!  get_int_value_from_cmdline() overwrites the integer value with one from the command line

subroutine get_int_value_from_cmdline( integer_ptr)

! **********************************************************************

!  get_int_value_from_cmdline() interface

! ----------------------------------------------------------------------

!  the name to be checked

type( integer_t), pointer :: integer_ptr

! ----------------------------------------------------------------------

!  search symbol list

   class( symbol_t), pointer :: symbol_ptr

! ----------------------------------------------------------------------

!  get_int_value_from_cmdline() text

continue

! ----------------------------------------------------------------------

!  check each symbol on the command line symbol list

   symbol_ptr => first_cl_symbol

   check_all_symbols: do

      if( .not. associated( symbol_ptr ) ) exit check_all_symbols

      names_match: if( symbol_ptr% name_str == integer_ptr% name_str )then

         types_match: select type( symbol_ptr)

         type is( integer_t) types_match

            integer_ptr% defined = .true.
            integer_ptr% defined_file = '<command line>'
            integer_ptr% defined_line = 0
            integer_ptr% cl_defined = .true.
            integer_ptr% integer_value = symbol_ptr% integer_value

         class default types_match

            call msg_quit( "type mismatch command line versus source: " // integer_ptr% name_str)

         end select types_match

      end if names_match

      symbol_ptr => symbol_ptr% next

   end do check_all_symbols

! ----------------------------------------------------------------------

!  get_int_value_from_cmdline() exit

return

! **********************************************************************

!  get_int_value_from_cmdline()

end subroutine get_int_value_from_cmdline

! **********************************************************************
! **********************************************************************

!  get_log_value_from_cmdline() overwrites the logical value with one from the command line

subroutine get_log_value_from_cmdline( logical_ptr)

! **********************************************************************

!  get_log_value_from_cmdline() interface

! ----------------------------------------------------------------------

!  the name to be checked

type( logical_t), pointer :: logical_ptr

! ----------------------------------------------------------------------

!  search symbol list

   class( symbol_t), pointer :: symbol_ptr

! ----------------------------------------------------------------------

!  get_log_value_from_cmdline() text

continue

! ----------------------------------------------------------------------

!  check each symbol on the command line symbol list

   symbol_ptr => first_cl_symbol

   check_all_symbols: do

      if( .not. associated( symbol_ptr ) ) exit check_all_symbols

      names_match: if( symbol_ptr% name_str == logical_ptr% name_str )then

         types_match: select type( symbol_ptr)

         type is( logical_t) types_match

            logical_ptr% defined = .true.
            logical_ptr% defined_file = '<command line>'
            logical_ptr% defined_line = 0
            logical_ptr% cl_defined = .true.
            logical_ptr% logical_value = symbol_ptr% logical_value

         class default types_match

            call msg_quit( "type mismatch command line versus source: " // logical_ptr% name_str)

         end select types_match

      end if names_match

      symbol_ptr => symbol_ptr% next

   end do check_all_symbols

! ----------------------------------------------------------------------

!  get_log_value_from_cmdline() exit

return

! **********************************************************************

!  get_log_value_from_cmdline()

end subroutine get_log_value_from_cmdline

! **********************************************************************
! **********************************************************************

!  coco

! $Id: coco.f90,v 2.11 2013/04/21 16:23:20 dan Exp $
! **********************************************************************

end program coco
