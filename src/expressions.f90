! bof
! **********************************************************************
! Fortran module expressions

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

!  expressions

! **********************************************************************

module expressions

! ----------------------------------------------------------------------

!  expressions uses modules

use :: constants

use :: char_utilities

use :: targets

use :: files

use :: symbols

! ----------------------------------------------------------------------

!  no implicit typing: require explicit declarations

implicit none

! ----------------------------------------------------------------------

!  all names are private: require explicit exports

private

! ----------------------------------------------------------------------

!  expressions RCS strings

! ----------------------------------------------------------------------

!  source file identifier supplied by RCS

character( len= *), public, parameter :: expressions_rcs_id = &
   '$Id: coco.f90,v 2.11 2013/04/21 16:23:20 dan Exp $'

! ----------------------------------------------------------------------

!  expressions constants

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------

!  constants defining coco (integer or logical) operators, constants, and similar

! ----------------------------------------------------------------------

!  minus sign

character( len= *), parameter :: minus = '-'

!  plus sign

character( len= *), parameter :: plus = '+'

!  times sign

character( len= *), parameter :: times = '*'

!  slash and division sign

character( len= *), parameter :: slash = '/'

!  backslash and modulus sign

character( len= *), parameter :: backslash = '\'

! ----------------------------------------------------------------------

!  logical binary operators

character( len= *), parameter :: or_str = '.or.'

character( len= *), parameter :: and_str = '.and.'

character( len= *), parameter :: eqv_str = '.eqv.'

character( len= *), parameter :: neqv_str = '.neqv.'

! ----------------------------------------------------------------------

!  logical uniary operator

character( len= *), parameter :: not_str = '.not.'

! ----------------------------------------------------------------------

!  logical literals

character( len= *), parameter :: true_str = '.true.'

character( len= *), parameter :: false_str = '.false.'

! ----------------------------------------------------------------------

!  the archaic versions of the relational operators

character( len= *), parameter :: dot_eq = '.eq.'

character( len= *), parameter :: dot_ne = '.ne.'

character( len= *), parameter :: dot_gt = '.gt.'

character( len= *), parameter :: dot_ge = '.ge.'

character( len= *), parameter :: dot_le = '.le.'

character( len= *), parameter :: dot_lt = '.lt.'

!  the modern versions of the relational operators

character( len= *), parameter :: ch_eq = '=='

character( len= *), parameter :: ch_ne = '/='

character( len= *), parameter :: ch_gt = '>'

character( len= *), parameter :: ch_ge = '>='

character( len= *), parameter :: ch_le = '<='

character( len= *), parameter :: ch_lt = '<'

! ----------------------------------------------------------------------

!  expressions library

! ----------------------------------------------------------------------

public :: get_integer_value
public :: get_logical_value

public :: eval_int_expr
public :: eval_log_expr

public :: integer_or_logical

! **********************************************************************

!  module procedures

! **********************************************************************

contains

! **********************************************************************

!  get_integer_value() seek symbol on symbol list

subroutine get_integer_value( integer_str, return_value)

! **********************************************************************

!  get_integer_value() interface

! ----------------------------------------------------------------------

!  the name of the integer whose value is sought

character( len= *), intent( in) :: integer_str

!  the value of the integer

integer, intent( out) :: return_value

! **********************************************************************

!  entry: symbol_str is blank_compress_lower_case logical symbol directive
!         "name..."

!  exit: symbol found or not in logical symbol array

! **********************************************************************

!  get_integer_value() local

! ----------------------------------------------------------------------

!  pointer to search the integer sublist of the symbol list

   class( symbol_t), pointer :: symbol_ptr

! **********************************************************************

!  get_integer_value() text

continue

! ----------------------------------------------------------------------

!  search integer list

   symbol_ptr => first_symbol

   search_list: do

      if( .not. associated( symbol_ptr) ) exit search_list

      check_integers: select type( symbol_ptr)

      type is( integer_t) check_integers

         name_match: if( integer_str == get_name_str( symbol_ptr% name_str) )then

            value_defined: if( symbol_ptr% defined )then

               return_value = symbol_ptr% integer_value

               symbol_ptr% referenced = .true.
               symbol_ptr% referenced_file = get_class_file_name( current_file)
               symbol_ptr% referenced_line = current_file% lines_transfered

               return

            else value_defined

               call msg_quit( "integer not defined: " // trim( integer_str) )

            end if value_defined

         end if name_match

      end select check_integers

      symbol_ptr => symbol_ptr% next

   end do search_list

! ----------------------------------------------------------------------

!  integer not found

   call msg_quit( "unknown integer: " // trim( integer_str) )

! ----------------------------------------------------------------------

!  get_integer_value() exit

return

! **********************************************************************

!  get_integer_value()

end subroutine get_integer_value

! **********************************************************************
! **********************************************************************

!  get_logical_value() seek symbol on symbol list

subroutine get_logical_value( logical_str, return_value)

! **********************************************************************

!  get_logical_value() interface

! ----------------------------------------------------------------------

!  the name of the logical whose value is sought

character( len= *), intent( in) :: logical_str

!  the value of the logical

logical, intent( out) :: return_value

! **********************************************************************

!  entry: symbol_str is blank_compress_lower_case logical symbol directive
!         "name..."

!  exit: symbol found or not in logical symbol array

! **********************************************************************

!  get_logical_value() local

! ----------------------------------------------------------------------

!  pointer to search symbol list

   class( symbol_t), pointer :: symbol_ptr

! **********************************************************************

!  get_logical_value() text

continue

! ----------------------------------------------------------------------

!  search symbol list

   symbol_ptr => first_symbol

   search_list: do

      if( .not. associated( symbol_ptr) ) exit search_list

      check_logicals: select type( symbol_ptr)

      type is( logical_t) check_logicals

         name_match: if( logical_str == get_name_str( symbol_ptr% name_str) )then

            value_defined: if( symbol_ptr% defined )then

               return_value = symbol_ptr% logical_value

               symbol_ptr% referenced = .true.
               symbol_ptr% referenced_file = get_class_file_name( current_file)
               symbol_ptr% referenced_line = current_file% lines_transfered

               return

            else value_defined

               call msg_quit( "logical not defined: " // trim( logical_str) )

            end if value_defined

         end if name_match

      end select check_logicals

      symbol_ptr => symbol_ptr% next

   end do search_list

! ----------------------------------------------------------------------

!  logical not found

   call msg_quit( "unknown logical: " // trim( logical_str) )

! ----------------------------------------------------------------------

!  get_logical_value() exit

return

! **********************************************************************

!  get_logical_value()

end subroutine get_logical_value

! **********************************************************************
! **********************************************************************

!  %%% diagnose and evaluate expressions

! **********************************************************************
! **********************************************************************

!  integer_or_logical() determine type of expression

subroutine integer_or_logical( expr_str, flag)

! **********************************************************************

!  integer_or_logical() interface

! ----------------------------------------------------------------------

!  an expression whose type is to be assertained

character( len= *), intent( in) :: expr_str

!  true if the type is integer

logical, intent( out) :: flag

! **********************************************************************

!  entry: symbol_str is string "..."

!  exit: flag is true if string is an integer expression and false otherwise

! **********************************************************************

!  integer_or_logical() constants

! ----------------------------------------------------------------------

!  search for a character which must be part of a logical expression

character( len= *), parameter :: logical_chars = '.<>='

!  search for a character which may be part of an integer expression

character( len= *), parameter :: integer_chars = '+-*/\'

! **********************************************************************

!  integer_or_logical() local

! ----------------------------------------------------------------------

!  search results

   integer :: char_idx

!  search integer or logical lists

   type( integer_t), pointer ::  integer_ptr
   type( logical_t), pointer ::  logical_ptr

 ! **********************************************************************

!  integer_or_logical() text

continue

! ----------------------------------------------------------------------

!  does string contain a character which is only in logical expressions?

   char_idx = scan( expr_str, logical_chars)

   got_dot: if( char_idx > 0 )then

      flag = .false.

      return

   end if got_dot

!  does string contain a character which is only in integer expressions?

   char_idx = scan( expr_str, integer_chars)

   got_op: if( char_idx > 0 )then

      flag = .true.

      return

   end if got_op

! ----------------------------------------------------------------------

!  is string an integer or a logical symbol name?

   char_idx = verify( expr_str, alphanum_chars)

   got_name: if( char_idx == 0 )then

      nullify( integer_ptr)

      search_integers: do

         call get_next_integer( integer_ptr)

         if( .not. associated( integer_ptr) ) exit search_integers

         match_int_name: if( expr_str == trim_name( integer_ptr% name_str) )then

            flag = .true.

            return

         end if match_int_name

      end do search_integers

      nullify( logical_ptr)

      search_logicals: do

         call get_next_logical( logical_ptr)

         if( .not. associated( logical_ptr) ) exit search_logicals

         match_log_name: if( expr_str == trim_name( logical_ptr% name_str) )then

            flag = .false.

            return

         end if match_log_name

      end do search_logicals

   end if got_name

! ----------------------------------------------------------------------

!  is string all digits?

   char_idx = verify( expr_str, digit_chars)

   got_digits: if( char_idx == 0 )then

      flag = .true.

      return

   end if got_digits

! ----------------------------------------------------------------------

!  can't classify the expression so punt

   call msg_quit( "can't classify: " // trim( expr_str) )

! ----------------------------------------------------------------------

!  integer_or_logical() exit

return

! **********************************************************************

!  integer_or_logical()

end subroutine integer_or_logical

! **********************************************************************
! **********************************************************************

!  eval_int_expr() evaluate int_expr as an integer

recursive subroutine eval_int_expr( int_expr, value)

! **********************************************************************

!  eval_int_expr() interface

! ----------------------------------------------------------------------

!  the integer expression to be evaluated

character( len= *), intent( in) :: int_expr

!  the value of the integer expression

integer, intent( out) :: value

! **********************************************************************

!  entry: int_expr is blank_compress_lower_case integer int_expr

!  exit: true if value is int_expr value, false otherwise

! **********************************************************************

!  eval_int_expr() constants

! ----------------------------------------------------------------------

!  addition operators

integer, parameter :: add_op_len = max( len( plus), len( minus) )

!  multiplication operators (times is defined in the main program)

character( len= *), parameter :: divby = slash

character( len= *), parameter :: remby = backslash

integer, parameter :: mul_op_len = max( len( times), len( divby), len( remby) )

!  length of operators

integer, parameter :: op_len = max( len( plus), len( minus), len( times), len( divby), len( remby) )

! **********************************************************************

!  eval_int_expr() local

! ----------------------------------------------------------------------

!  operations to be done

   character( len= add_op_len) :: add_op

   character( len= mul_op_len) :: mul_op

! ----------------------------------------------------------------------

!  next operation

   character( len= op_len) :: next_op

! ----------------------------------------------------------------------

!  partial values of the int_expr

   integer :: l_add, r_add

   integer :: l_mul, r_mul

! ----------------------------------------------------------------------

!  pointers to characters

   integer :: next_char

   integer :: next_op_idx

   integer :: expr_len

   integer :: primary_len

! **********************************************************************

!  eval_int_expr() text

continue

! ----------------------------------------------------------------------

!  limits of scan

   next_char = 1

   expr_len = len_trim( int_expr)

!  initialize adds

   add_op = plus

   l_add = 0

! ----------------------------------------------------------------------

!  scan thru int_expr

   add_ops: do

      if( next_char > expr_len) exit add_ops

!  find a primary

      call eval_int_primary( int_expr( next_char: ), primary_len, r_add)

      next_op_idx = next_char + primary_len

!  find next operator or end of expression

      add_end: if( next_op_idx <= expr_len )then

         next_op = int_expr( next_op_idx: next_op_idx)

         next_char = next_op_idx + 1

      else add_end

         next_op = blank

         next_char = next_op_idx

      end if add_end

! ----------------------------------------------------------------------

!  initialize for a set of mul ops

      mul_op = next_op

      l_mul = r_add

! ----------------------------------------------------------------------

!  process a set of mul ops

      mul_ops: do

         if( .not. ( next_op == times .or. next_op == divby .or. next_op == remby) ) exit mul_ops

!  find a primary

         call eval_int_primary( int_expr( next_char: ), primary_len, r_mul)

         next_op_idx = next_char + primary_len

!  find next operator or end of expression

         mul_end: if( next_op_idx <= expr_len )then

            next_op = int_expr( next_op_idx: next_op_idx)

            next_char = next_op_idx + 1

         else mul_end

            next_op = blank

            next_char = next_op_idx

         end if mul_end

!  do the pending add op

         mul_div: select case( mul_op)

         case( times) mul_div

            l_mul = l_mul * r_mul

         case( divby) mul_div

            l_mul = l_mul / r_mul

         case( remby) mul_div

            l_mul = mod( l_mul, r_mul)

         end select mul_div

         mul_op = next_op

      end do mul_ops

!  product is the right operand

      r_add = l_mul

! ----------------------------------------------------------------------

!  do the pending add op

      add_sub: select case( add_op)

      case( blank, plus) add_sub

         l_add = l_add + r_add

      case( minus) add_sub

         l_add = l_add - r_add

      case default add_sub

         call msg_quit( "unknown arithmetic operator: " // add_op)

      end select add_sub

      add_op = next_op

   end do add_ops

! ----------------------------------------------------------------------

!  value of integer expression

   value = l_add

! ----------------------------------------------------------------------

!  eval_int_expr() exit

return

! **********************************************************************

!  eval_int_expr()

end subroutine eval_int_expr

! **********************************************************************
! **********************************************************************

!  eval_log_expr() expression is evaluated as a logical

recursive subroutine eval_log_expr( log_expr, value)

! **********************************************************************

!  eval_log_expr() interface

! ----------------------------------------------------------------------

!  the logical expression to be evaluated

character( len= *), intent( in) :: log_expr

!  the value of the expression

logical, intent( out) :: value

! **********************************************************************

!  entry: expression is blank_compress_lower_case logical expression

!  exit: value is expression value

! **********************************************************************

!  eval_log_expr() constants

integer, parameter :: eqv_op_len = max( len( eqv_str), len( neqv_str))

!  length of the next operator

integer, parameter :: next_op_len = max( len( or_str), len( and_str), len( eqv_str), len( neqv_str))

! **********************************************************************

!  eval_log_expr() local

! ----------------------------------------------------------------------

!  the current eqv operator

   character( len= eqv_op_len) :: eqv_op

!  the next operator

   character( len= next_op_len) :: next_op

! ----------------------------------------------------------------------

!  point to characters not yet decoded

   integer :: next_char

   integer :: next_op_idx

   integer :: expr_len

   integer :: primary_len

!  false if and but no eqv

   logical :: do_or

!  expression values

   logical :: l_eqv, l_or, l_and

   logical :: r_eqv, r_or, r_and

! **********************************************************************

!  eval_log_expr() text

continue

! ----------------------------------------------------------------------

!  limits of scan

   next_char = 1

   expr_len = len_trim( log_expr)

!  initialize equivalences

   eqv_op = eqv_str

   l_eqv = .true.

! ----------------------------------------------------------------------

!  scan thru log_expr

   eqv_ops: do

      if( next_char > expr_len) exit eqv_ops

!  find a primary and return its length and value

      call eval_log_primary( log_expr( next_char: ), primary_len, r_eqv)

      next_op_idx = next_char + primary_len

!  find next operator or end of expression

      eqv_or_end: if( next_op_idx <= expr_len )then

!  decode which operator

         eqv_next_op: if( log_expr( next_op_idx: next_op_idx + len( eqv_str) - 1) == eqv_str )then

            next_op = log_expr( next_op_idx: next_op_idx + len( eqv_str) - 1)

            next_char = next_op_idx + len( eqv_str)

         else if( log_expr( next_op_idx: next_op_idx + len( neqv_str) - 1) == neqv_str )then eqv_next_op

            next_op = log_expr( next_op_idx: next_op_idx + len( neqv_str) - 1)

            next_char = next_op_idx + len( neqv_str)

         else if( log_expr( next_op_idx: next_op_idx + len( or_str) - 1) == or_str )then eqv_next_op

            next_op = log_expr( next_op_idx: next_op_idx + len( or_str) - 1)

            next_char = next_op_idx + len( or_str)

         else if( log_expr( next_op_idx: next_op_idx + len( and_str) - 1) == and_str )then eqv_next_op

            next_op = log_expr( next_op_idx: next_op_idx + len( and_str) - 1)

            next_char = next_op_idx + len( and_str)

         else eqv_next_op

            call msg_quit( "unknown logical operator: " // trim( log_expr( next_op_idx: ) ))

         end if eqv_next_op

!  past end of expression

      else eqv_or_end

         next_op = blank

         next_char = next_op_idx

      end if eqv_or_end

! ----------------------------------------------------------------------

!  initialize for a set of or ops

      l_or = r_eqv

! ----------------------------------------------------------------------

!  process a set of and ops

      or_ops: do

         if( .not. ( next_op == or_str .or. next_op == and_str) ) exit or_ops

         do_or = next_op == or_str

         or_next: select case( do_or)

         case( .true.) or_next

!  find a primary and return its length and value

            call eval_log_primary( log_expr( next_char: ), primary_len, r_or)

            next_op_idx = next_char + primary_len

!  find next operator or end of expression

            or_end: if( next_op_idx <= expr_len )then

!  decode which operator

               or_next_op: if( log_expr( next_op_idx: next_op_idx + len( eqv_str) - 1) == eqv_str )then

                  next_op = log_expr( next_op_idx: next_op_idx + len( eqv_str) - 1)

                  next_char = next_op_idx + len( eqv_str)

               else if( log_expr( next_op_idx: next_op_idx + len( neqv_str) - 1) == neqv_str )then or_next_op

                  next_op = log_expr( next_op_idx: next_op_idx + len( neqv_str) - 1)

                  next_char = next_op_idx + len( neqv_str)

               else if( log_expr( next_op_idx: next_op_idx + len( or_str) - 1) == or_str )then or_next_op

                  next_op = log_expr( next_op_idx: next_op_idx + len( or_str) - 1)

                  next_char = next_op_idx + len( or_str)

               else if( log_expr( next_op_idx: next_op_idx + len( and_str) - 1) == and_str )then or_next_op

                  next_op = log_expr( next_op_idx: next_op_idx + len( and_str) - 1)

                  next_char = next_op_idx + len( and_str)

               else or_next_op

                  call msg_quit( "unknown logical operator: " // trim( log_expr( next_op_idx: ) ) )

               end if or_next_op

            else or_end

               next_op = blank

               next_char = next_op_idx

            end if or_end

         case default or_next

            r_or = l_or

         end select or_next

! ----------------------------------------------------------------------

!  initialize for a set of and ops

         l_and = r_or

! ----------------------------------------------------------------------

!  process a set of and ops

         and_ops: do

            if( next_op /= and_str ) exit and_ops

!  find a primary

            call eval_log_primary( log_expr( next_char: ), primary_len, r_and)

            next_op_idx = next_char + primary_len

!  find next operator or end of expression

            and_end: if( next_op_idx <= expr_len )then

!  decode which operator

               and_next_op: if( log_expr( next_op_idx: next_op_idx + len( eqv_str) - 1) == eqv_str )then

                  next_op = log_expr( next_op_idx: next_op_idx + len( eqv_str) - 1)

                  next_char = next_op_idx + len( eqv_str)

               else if( log_expr( next_op_idx: next_op_idx + len( neqv_str) - 1) == neqv_str )then and_next_op

                  next_op = log_expr( next_op_idx: next_op_idx + len( neqv_str) - 1)

                  next_char = next_op_idx + len( neqv_str)

               else if( log_expr( next_op_idx: next_op_idx + len( and_str) - 1) == and_str )then and_next_op

                  next_op = log_expr( next_op_idx: next_op_idx + len( and_str) - 1)

                  next_char = next_op_idx + len( and_str)

               else if( log_expr( next_op_idx: next_op_idx + len( or_str) - 1) == or_str )then and_next_op

                  next_op = log_expr( next_op_idx: next_op_idx + len( or_str) - 1)

                  next_char = next_op_idx + len( or_str)

               else and_next_op

                  call msg_quit( "unknown logical operator: " // trim( log_expr( next_op_idx: ) ) )

               end if and_next_op

            else and_end

               next_op = blank

               next_char = next_op_idx

            end if and_end

!  do the pending and op

            l_and = l_and .and. r_and

         end do and_ops

!  product is the right operand

         r_or = l_and

! ----------------------------------------------------------------------

!  do the pending or op

         this_or: select case( do_or)

         case( .true.) this_or

            l_or = l_or .or. r_or

         case default this_or

            l_or = r_or

         end select this_or

      end do or_ops

!  product is the right operand

      r_eqv = l_or

! ----------------------------------------------------------------------

!  do the pending eqv op

      eqv_neqv: select case( eqv_op)

      case( blank, eqv_str) eqv_neqv

         l_eqv = l_eqv .eqv. r_eqv

      case( neqv_str) eqv_neqv

         l_eqv = l_eqv .neqv. r_eqv

      end select eqv_neqv

      eqv_op = next_op

   end do eqv_ops

! ----------------------------------------------------------------------

   value = l_eqv

! ----------------------------------------------------------------------

!  eval_log_expr() exit

return

! **********************************************************************

!  eval_log_expr()

end subroutine eval_log_expr

! **********************************************************************
! **********************************************************************

!  eval_rel_expr() a relational expression is evaluated as a logical

subroutine eval_rel_expr( rel_expr, value)

! **********************************************************************

!  eval_rel_expr() interface

! ----------------------------------------------------------------------

!  the relational expression ot be evaluated

character( len= *), intent( in) :: rel_expr

!  the value of the relational expression

logical, intent( out) :: value

! **********************************************************************

!  entry: expression is blank_compress_lower_case relational expression

!  exit: value is expression value

! **********************************************************************

!  eval_rel_expr() local

! ----------------------------------------------------------------------

!  index of symbol entry

   integer :: dot_idx

   integer :: eq_idx, ne_idx, gt_idx, ge_idx, le_idx, lt_idx

   integer :: l_val, r_val

   character( len= buffer_len) :: expr_str

! **********************************************************************

!  eval_rel_expr() text

continue

! ----------------------------------------------------------------------

   dot_idx = index( rel_expr, dot)

! ----------------------------------------------------------------------

!  find a dot?

   got_dot: if( dot_idx > 0 )then

!  seek all operators with dot

      eq_idx = index( rel_expr, dot_eq)

      ne_idx = index( rel_expr, dot_ne)

      gt_idx = index( rel_expr, dot_gt)

      ge_idx = index( rel_expr, dot_ge)

      le_idx = index( rel_expr, dot_le)

      lt_idx = index( rel_expr, dot_lt)

! ----------------------------------------------------------------------

!  find one

      dot_rel_op: if( eq_idx > 0 )then

         expr_str = rel_expr( 1: eq_idx - 1)

         call eval_int_expr( expr_str, l_val)

         expr_str = rel_expr( eq_idx + len( dot_eq): )

         call eval_int_expr( expr_str, r_val)

         value = l_val == r_val

      else if( ne_idx > 0 )then dot_rel_op

         expr_str = rel_expr( 1: ne_idx - 1)

         call eval_int_expr( expr_str, l_val)

         expr_str = rel_expr( ne_idx + len( dot_ne): )

         call eval_int_expr( expr_str, r_val)

         value = l_val /= r_val

      else if( ge_idx > 0 )then dot_rel_op

         expr_str = rel_expr( 1: ge_idx - 1)

         call eval_int_expr( expr_str, l_val)

         expr_str = rel_expr( ge_idx + len( dot_ge): )

         call eval_int_expr( expr_str, r_val)

         value = l_val >= r_val

      else if( le_idx > 0 )then dot_rel_op

         expr_str = rel_expr( 1: le_idx - 1)

         call eval_int_expr( expr_str, l_val)

         expr_str = rel_expr( le_idx + len( dot_le): )

         call eval_int_expr( expr_str, r_val)

         value = l_val <= r_val

      else if( gt_idx > 0 )then dot_rel_op

         expr_str = rel_expr( 1: gt_idx - 1)

         call eval_int_expr( expr_str, l_val)

         expr_str = rel_expr( gt_idx + len( dot_gt): )

         call eval_int_expr( expr_str, r_val)

         value = l_val > r_val

      else if( lt_idx > 0 )then dot_rel_op

         expr_str = rel_expr( 1: lt_idx - 1)

         call eval_int_expr( expr_str, l_val)

         expr_str = rel_expr( lt_idx + len( dot_lt): )

         call eval_int_expr( expr_str, r_val)

         value = l_val < r_val

! ----------------------------------------------------------------------

!  unknown relational operator

      else dot_rel_op

         call msg_quit( "no relational operator (.eq., .ne., .gt., .ge., .le., .lt.): " // rel_expr)

      end if dot_rel_op

! ----------------------------------------------------------------------

!  operator without dot

   else got_dot

!  seek all comparison ops

      eq_idx = index( rel_expr, ch_eq)

      ne_idx = index( rel_expr, ch_ne)

      gt_idx = index( rel_expr, ch_gt)

      ge_idx = index( rel_expr, ch_ge)

      le_idx = index( rel_expr, ch_le)

      lt_idx = index( rel_expr, ch_lt)

! ----------------------------------------------------------------------

!  find one

      ch_rel_op: if( eq_idx > 0 )then

         expr_str = rel_expr( 1: eq_idx - 1)

         call eval_int_expr( expr_str, l_val)

         expr_str = rel_expr( eq_idx + len( ch_eq): )

         call eval_int_expr( expr_str, r_val)

         value = l_val == r_val

      else if( ne_idx > 0 )then ch_rel_op

         expr_str = rel_expr( 1: ne_idx - 1)

         call eval_int_expr( expr_str, l_val)

         expr_str = rel_expr( ne_idx + len( ch_ne): )

         call eval_int_expr( expr_str, r_val)

         value = l_val /= r_val

      else if( ge_idx > 0 )then ch_rel_op

         expr_str = rel_expr( 1: ge_idx - 1)

         call eval_int_expr( expr_str, l_val)

         expr_str = rel_expr( ge_idx + len( ch_ge): )

         call eval_int_expr( expr_str, r_val)

         value = l_val >= r_val

      else if( le_idx > 0 )then ch_rel_op

         expr_str = rel_expr( 1: le_idx - 1)

         call eval_int_expr( expr_str, l_val)

         expr_str = rel_expr( le_idx + len( ch_le): )

         call eval_int_expr( expr_str, r_val)

         value = l_val <= r_val

      else if( gt_idx > 0 )then ch_rel_op

         expr_str = rel_expr( 1: gt_idx - 1)

         call eval_int_expr( expr_str, l_val)

         expr_str = rel_expr( gt_idx + len( ch_gt): )

         call eval_int_expr( expr_str, r_val)

         value = l_val > r_val

      else if( lt_idx > 0 )then ch_rel_op

         expr_str = rel_expr( 1: lt_idx - 1)

         call eval_int_expr( expr_str, l_val)

         expr_str = rel_expr( lt_idx + len( ch_lt): )

         call eval_int_expr( expr_str, r_val)

         value = l_val < r_val

! ----------------------------------------------------------------------

!  unknown relational operator

      else ch_rel_op

         call msg_quit( "no relational operator (==, /=, >, >=, <=, <): " // rel_expr)

      end if ch_rel_op

   end if got_dot

! ----------------------------------------------------------------------

!  eval_rel_expr() exit

return

! **********************************************************************

!  eval_rel_expr()

end subroutine eval_rel_expr

! **********************************************************************
! **********************************************************************

!  seek_log_primary() a relational expression is evaluated as a logical

subroutine seek_log_primary( log_expr, op_idx, rel_op_idx)

! **********************************************************************

!  seek_log_primary() interface

! ----------------------------------------------------------------------

!  the logical primary to be evaluated

character( len= *), intent( in) :: log_expr

!  the index of the next operator or end of line after the primary

integer, intent( out) :: op_idx

!  the index of the next relational operator or zero

integer, intent( out) :: rel_op_idx

! **********************************************************************

!  entry: find log op before first (if any) open paren or after matching

!  exit: length to first log op

! **********************************************************************

!  seek_log_primary() local

integer :: paren_level

! **********************************************************************

!  seek_log_primary() text

continue

!  initialize while loop parameters

   op_idx = 1

   paren_level = 0

   rel_op_idx = 0

! ----------------------------------------------------------------------

!  scan through expression

   scan_stmt: do

      if( op_idx > len_trim( log_expr)) exit scan_stmt

!  check each character

      which_char: select case( log_expr( op_idx: op_idx))

!  need to track parenthesis level

      case( open_paren) which_char

         paren_level = paren_level + 1

         op_idx = op_idx + len( open_paren)

         cycle scan_stmt

      case( close_paren) which_char

         paren_level = paren_level - 1

         op_idx = op_idx + len( close_paren)

         cycle scan_stmt

      case( dot) which_char

         log_op_at_level_zero: if( paren_level == 0 )then

!  find logical operator

            find_log_op: if( log_expr( op_idx: op_idx + len( or_str) - 1) == or_str )then

               exit scan_stmt

            else if( log_expr( op_idx: op_idx + len( and_str) - 1) == and_str )then find_log_op

               exit scan_stmt

            else if( log_expr( op_idx: op_idx + len( eqv_str) - 1) == eqv_str )then find_log_op

               exit scan_stmt

            else if( log_expr( op_idx: op_idx + len( neqv_str) - 1) == neqv_str )then find_log_op

               exit scan_stmt

            end if find_log_op

         end if log_op_at_level_zero

      end select which_char

!  check for relational operator (which diagnoses a relational expression)

      rel_op_at_level_zero: if( paren_level == 0 )then

         found_rel_op: if( log_expr( op_idx: op_idx + len( dot_eq) - 1) == dot_eq &
                      .or. log_expr( op_idx: op_idx + len( dot_ne) - 1) == dot_ne &
                      .or. log_expr( op_idx: op_idx + len( dot_lt) - 1) == dot_lt &
                      .or. log_expr( op_idx: op_idx + len( dot_le) - 1) == dot_le &
                      .or. log_expr( op_idx: op_idx + len( dot_ge) - 1) == dot_ge &
                      .or. log_expr( op_idx: op_idx + len( dot_gt) - 1) == dot_gt &
                      .or. log_expr( op_idx: op_idx + len( ch_eq) - 1) == ch_eq &
                      .or. log_expr( op_idx: op_idx + len( ch_ne) - 1) == ch_ne &
                      .or. log_expr( op_idx: op_idx + len( ch_lt) - 1) == ch_lt &
                      .or. log_expr( op_idx: op_idx + len( ch_le) - 1) == ch_le &
                      .or. log_expr( op_idx: op_idx + len( ch_ge) - 1) == ch_ge &
                      .or. log_expr( op_idx: op_idx + len( ch_gt) - 1) == ch_gt )then

            rel_op_idx = op_idx

         end if found_rel_op

      end if rel_op_at_level_zero

!  catch unbalanced parenthesis in logical expression

      unbalanced_parens: if( paren_level < 0 )then

         call msg_quit( "unbalanced parenthesis in expression: " // trim( log_expr) )

      end if unbalanced_parens

!  scan next character

      op_idx = op_idx + 1

   end do scan_stmt

!  point to last character in primary

   op_idx = op_idx - 1

! ----------------------------------------------------------------------

!  seek_log_primary() exit

return

! **********************************************************************

!  seek_log_primary()

end subroutine seek_log_primary

! **********************************************************************
! **********************************************************************

!  eval_int_primary() decode a string to get an integer value

recursive subroutine eval_int_primary( primary_str, primary_len, value)

! **********************************************************************

!  eval_int_primary() interface

! ----------------------------------------------------------------------

!  the integer primary to be evaluated

character( len= *), intent( in) :: primary_str

!  the length of the inetger primary

integer, intent( out) :: primary_len

!  the value of the primary

integer, intent( out) :: value

! **********************************************************************

!  entry: primary_str is a string containing a literal integer

!  exit: primary_len is the length decoded, value is integer value

! **********************************************************************

!  eval_int_primary() local

! ----------------------------------------------------------------------

!  process sign separately

   integer :: isign

!  pointers to characters

   integer :: next_char

   integer :: char_idx

   integer :: match_paren

!  decode digit strings

   character( len= conversion_len) :: conversion_str

!  string containing expressions

   character( len= buffer_len) :: expr_str

! **********************************************************************

!  eval_int_primary() text

continue

! ----------------------------------------------------------------------

!  decode unary operator

   next_char = 1

!  evaluate the primary using the expression string

   expr_str = primary_str

! ----------------------------------------------------------------------

!  test first character is minus

   process_sign: select case( expr_str( next_char: next_char) )

! ----------------------------------------------------------------------

   case( minus) process_sign

      next_char = next_char + len( minus)

      primary_len = len( minus)

      isign = -1

! ----------------------------------------------------------------------

!  test first character is plus

   case( plus) process_sign

      next_char = next_char + len( plus)

      primary_len = len( plus)

      isign = 1

! ----------------------------------------------------------------------

!  test first character is neither plus nor minus

   case default process_sign

      primary_len = 0

      isign = 1

   end select process_sign

! ----------------------------------------------------------------------

!  find the value of a variable, a literal, or a parenthesized primary_str

   get_value: select case( expr_str( next_char: next_char) )

! ----------------------------------------------------------------------

!  get the value from the variable

   case( 'a': 'z') get_value

!  seek the value of the symbol name

      char_idx = verify( expr_str( next_char: ) // blank, alphanum_chars) + next_char - 2

      call get_integer_value( expr_str( next_char: char_idx), value)

!  processed the alphanumeric characters

      primary_len = primary_len + char_idx

! ----------------------------------------------------------------------

!  get the value of a literal

   case( '0': '9') get_value

!  find the first character which is not a digit

      char_idx = verify( expr_str( next_char: ) // blank, digit_chars) + next_char - 2

!  decode digits

      conversion_str = expr_str( next_char: char_idx)

      conversion_str = adjustr( conversion_str)

      read( unit= conversion_str, fmt= conversion_fmt, iostat= state% status, iomsg= state% message) value

!  check read error

      decode: if( is_status_error( state) )then

         call msg_quit( "can't decode: " // primary_str)

      end if decode

!  processed the digit string

      primary_len = primary_len + char_idx

! ----------------------------------------------------------------------

!  get the value of an primary_str

   case( open_paren) get_value

      call seek_close_paren( expr_str, next_char, match_paren)

      found_match: if(  match_paren <= len_trim( primary_str) )then

!  go evaluate the nested expression

         expr_str = primary_str( next_char + 1: match_paren - 1)

         call eval_int_expr( expr_str, value)

!  unmatched parenthesis so complain and quit

      else found_match

         call msg_quit( "unmatched parenthesis: " // trim( primary_str))

      end if found_match

!  processed up to the closing parenthesis

      primary_len = match_paren

! ----------------------------------------------------------------------

!  error: cannot get the value

   case default get_value

      call msg_quit( "bad integer expression: " // trim( primary_str) )

   end select get_value

! ----------------------------------------------------------------------

!  apply sign

   value = value * isign

! ----------------------------------------------------------------------

!  eval_int_primary() exit

return

! **********************************************************************

!  eval_int_primary()

end subroutine eval_int_primary

! **********************************************************************
! **********************************************************************

!  eval_log_primary() decode a string to get an logical value

recursive subroutine eval_log_primary( primary_str, primary_len, value)

! **********************************************************************

!  eval_log_primary() interface

! ----------------------------------------------------------------------

!  the logical primary to be evaluated

character( len= *), intent( in) :: primary_str

!  the length of the logical primary

integer, intent( out) :: primary_len

!  the value of the logical primary

logical, intent( out) :: value

! **********************************************************************

!  entry: primary_str is a string containing a literal logical

!  exit: value is logical value

! **********************************************************************

!  eval_log_primary() local

! ----------------------------------------------------------------------

!  logical "sign"

   logical :: lsign

   integer :: rel_op_idx

!  next character to be decoded

   integer :: next_char

   integer :: match_paren

   character( len= buffer_len) :: expr_str

! **********************************************************************

!  eval_log_primary() text

continue

! ----------------------------------------------------------------------

!  find length of primary and whether it is a relational expression

   call seek_log_primary( primary_str, primary_len, rel_op_idx)

!  decode unary operator

   next_char = 1

! ----------------------------------------------------------------------

!  expression too short to contain a .not.

   process_sign: if( primary_len <= len( not_str) )then

      lsign = .true.

!  expression has a .not.

   else if( primary_str( next_char: len( not_str)) == not_str )then process_sign

      next_char = next_char + len( not_str)

      lsign = .false.

!  no .not.

   else process_sign

      lsign = .true.

   end if process_sign

! ----------------------------------------------------------------------

!  a logical primary is either a logical expression or a relational expression

   log_or_rel: if( rel_op_idx == 0 )then

! ----------------------------------------------------------------------

!  find the value of a variable, a literal, or a parenthesized expression

      get_value: select case( primary_str( next_char: next_char) )

! ----------------------------------------------------------------------

!  get the value from the variable

      case( 'a': 'z') get_value

!  check whether it's a logical name or error

         call get_logical_value( primary_str( next_char: primary_len), value)

! ----------------------------------------------------------------------

!  get the value of a literal

      case( dot) get_value

!  decode literal value

         literal_value: if( primary_str( next_char: next_char + len( true_str) - 1) == true_str )then

!  found a .true. string

            value = .true.

         else if( primary_str( next_char: next_char + len( false_str) - 1) == false_str )then literal_value

!  found a .false. string

            value = .false.

!  complain and quit

         else literal_value

            call msg_quit( "bad logical literal: " // trim( primary_str) )

         end if literal_value

! ----------------------------------------------------------------------

!  get the value of an expression

      case( open_paren) get_value

!  seek the closing parenthesis

         call seek_close_paren( primary_str, next_char, match_paren)

!  if found, determine whether it is a logical or (part of a) relational expression

         found_match: if( match_paren <= len_trim( primary_str) )then

!  evaluate the logical expression within parenthesis

            expr_str = primary_str( next_char + 1: match_paren - 1)

            call eval_log_expr( expr_str, value)

!  unmatched parenthesis so complain and quit

         else found_match

            call msg_quit( "unmatched parenthesis: " // trim( primary_str))

         end if found_match

! ----------------------------------------------------------------------

!  error: can't decode logical value

      case default

         call msg_quit( "bad logical primary: " // trim( primary_str))

      end select get_value

! ----------------------------------------------------------------------

!  evaluate the relational expression

   else log_or_rel

         call eval_rel_expr( primary_str( next_char: primary_len), value)

   end if log_or_rel

! ----------------------------------------------------------------------

!  apply sign

   value = value .eqv. lsign

! ----------------------------------------------------------------------

!  eval_log_primary() exit

return

! **********************************************************************

!  eval_log_primary()

end subroutine eval_log_primary

! **********************************************************************
! ----------------------------------------------------------------------

!  expressions

! $Id: coco.f90,v 2.11 2013/04/21 16:23:20 dan Exp $
! **********************************************************************
! eof
end module expressions
