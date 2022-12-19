           >>source format free
identification division.
program-id. 17b.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_cnt pic s9(8) comp value 0.
      *> NOTE: Remember to edit lib-readdata to extend the line size!
      02 rf_row pic x(99999) occurs 0 to 1 times
          depending on rf_cnt indexed by rf_idx.

    01 all_dirs.
      02 dir_cnt pic s9(8) comp.
      *> OMG, took me too long to figure out this ended up truncated.
      02 dir pic s9 occurs 0 to 99999 times
          depending on dir_cnt indexed by dir_idx.

    01 curr_dir usage is index.
    01 temp_dir_idx usage is index.
    01 curr_round pic s9(8) comp.
    01 curr_top pic s9(8) comp.
    01 curr_new_y pic s9(8) comp.
    01 curr_piece_done pic 9.
    01 curr_piece_stuff.
      02 curr_piece_num pic 9.
      02 curr_piece_cnt pic 9.
      02 curr_piece occurs 5 times indexed by curr_piece_idx.
        03 curr_piece_x pic s9(8) comp.
        03 curr_piece_y pic s9(8) comp.

    01 move_valid pic 9.

    01 piece_fallen_stuff.
      02 piece_fallen_cnt pic s9(8) comp.
      02 piece_fallen occurs 0 to 99999 times
          depending on piece_fallen_cnt indexed by piece_fallen_idx.
        03 piece_fallen_x pic s9(8) comp.
        03 piece_fallen_y pic s9(8) comp.
        03 piece_fallen_type pic 9.

    01 print_top pic s9(8) comp.
    01 print_y_limit pic s9(8) comp.
    01 print_row_idx usage is index.
    01 print_col_idx usage is index.
    01 print_found pic 9.

    *> 01 history_curr pic 9.
    *> 01 history_pos pic 9.
    *> 01 history_dupe pic 9.
    *> 01 flatten_buffer_idx usage is index.
    *> 01 flatten_buffer pic x(999).
    *> 01 flatten_history_stuff.
    *>   02 flatten_history_cnt pic s9(8) comp.
    *>   02 flatten_history occurs 99999 times
    *>       indexed by flatten_history_idx.
    *>     03 flatten_history_line pic x(999).
    *>     03 flatten_history_top pic s9(8) comp.

    01 num_rounds pic 9(18) comp.

    01 history_buffer pic x(20).
    01 history_stuff.
      02 history_cnt pic s9(8) comp.
      02 history pic x(20) occurs 99999 times
          indexed by history_idx.
    01 history_test_gap pic 9(8) comp.
    01 history_test_stuff.
      02 history_test_cnt pic s9(8) comp.
      02 history_test pic x(20) occurs 999 times
          indexed by history_test_idx.
    01 history_found pic 9.
    01 history_dupe pic 9.
    01 history_start pic s9(8) comp.
    01 history_test_end pic s9(8) comp.
    01 history_num_2 pic 9.
    01 history_num_3 pic 9(8).
    01 history_num_4 pic 9(8).

    01 cycle_end pic 9(8) comp.
    01 cycle_beg pic 9(8) comp.
    01 cycle_rounds pic 9(8) comp.
    01 cycle_height_end pic 9(8) comp.
    01 cycle_height_beg pic 9(8) comp.
    01 cycle_height pic 9(8) comp.
    01 prefix_height pic 9(8) comp.
    01 num_cycles pic 9(18) comp.
    01 left_over_rounds pic 9(8) comp.
    01 left_over_height pic 9(8) comp.

    77 total_found pic s9(18) comp.

procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
  *> call 'lib-readdata' using function module-id ".da1" rf_all_lines

  move 0 to total_found

  move length of function trim(rf_row(1)) to dir_cnt
  *> display "LENGTH: " length of function trim(rf_row(1))
  perform varying dir_idx from 1 by 1 until dir_idx > dir_cnt
    if rf_row(1)(dir_idx:1) = "<"
      move -1 to dir(dir_idx)
    else
      move 1 to dir(dir_idx)
    end-if
  end-perform

  *> display "DIRS: (" dir_cnt ") [ " no advancing
  *> perform varying dir_idx from 1 by 1 until dir_idx > dir_cnt
  *>   display dir(dir_idx) " " no advancing
  *> end-perform
  *> display " ]"

  set dir_idx to 1
  move 1 to curr_round
  move 0 to curr_top
  move 0 to history_found

  *> LOL, to think I set this as high as 100 to make damn sure things were cycling.
  move 10 to history_test_gap

  *> move 2022 to num_rounds
  *> move 5000 to num_rounds
  move 1000000000000 to num_rounds

  perform until curr_round > num_rounds or history_found = 1
    perform next_piece
    *> display "NEW PIECE: [" no advancing
    *> perform varying curr_piece_idx from 1 by 1 until curr_piece_idx > curr_piece_cnt
      *> display "(" curr_piece_x(curr_piece_idx) ", " curr_piece_y(curr_piece_idx) "), " no advancing
    *> end-perform
    *> display space
    *> perform print_stack_new

    move 0 to curr_piece_done
    perform until curr_piece_done = 1
      *> Need to test move side to side.
      move 1 to move_valid
      perform varying curr_piece_idx from 1 by 1 until curr_piece_idx > curr_piece_cnt or move_valid = 0
        if curr_piece_x(curr_piece_idx) + dir(dir_idx) = 0 or curr_piece_x(curr_piece_idx) + dir(dir_idx) = 8
          move 0 to move_valid
        end-if
        *> Test other blocks
        if move_valid = 1
          perform varying piece_fallen_idx from 1 by 1 until piece_fallen_idx > piece_fallen_cnt
            if piece_fallen_x(piece_fallen_idx) = (curr_piece_x(curr_piece_idx) + dir(dir_idx))
                and piece_fallen_y(piece_fallen_idx) = (curr_piece_y(curr_piece_idx))
              move 0 to move_valid
          end-perform
        end-if
      end-perform
      if move_valid = 1
        perform varying curr_piece_idx from 1 by 1 until curr_piece_idx > curr_piece_cnt
          compute curr_piece_x(curr_piece_idx) = curr_piece_x(curr_piece_idx) + dir(dir_idx)
        end-perform
      end-if

      *> Test move down
      move 1 to move_valid
      perform varying curr_piece_idx from 1 by 1 until curr_piece_idx > curr_piece_cnt or move_valid = 0
        if curr_piece_y(curr_piece_idx) - 1 = 0
          move 0 to move_valid
        end-if
        *> Test other blocks
        if move_valid = 1
          perform varying piece_fallen_idx from 1 by 1 until piece_fallen_idx > piece_fallen_cnt
            if piece_fallen_x(piece_fallen_idx) = curr_piece_x(curr_piece_idx)
                and piece_fallen_y(piece_fallen_idx) = (curr_piece_y(curr_piece_idx) - 1)
              move 0 to move_valid
          end-perform
        end-if
      end-perform

      if move_valid = 1
        *> Move piece down
        perform varying curr_piece_idx from 1 by 1 until curr_piece_idx > curr_piece_cnt
          compute curr_piece_y(curr_piece_idx) = curr_piece_y(curr_piece_idx) - 1
        end-perform
      else
        *> If can't move down more, then it's done.
        move 1 to curr_piece_done

        *> Add new piece as a fallen piece
        perform varying curr_piece_idx from 1 by 1 until curr_piece_idx > curr_piece_cnt
          add 1 to piece_fallen_cnt
          move curr_piece_x(curr_piece_idx) to piece_fallen_x(piece_fallen_cnt)
          move curr_piece_y(curr_piece_idx) to piece_fallen_y(piece_fallen_cnt)
        end-perform

        *> display "FALLEN: [" no advancing
        *> perform varying piece_fallen_idx from 1 by 1 until piece_fallen_idx > piece_fallen_cnt
        *>   display "(" piece_fallen_x(piece_fallen_idx) ", " piece_fallen_y(piece_fallen_idx) "), "
        *> end-perform
      end-if

      *> display "DIR: " dir(dir_idx)
      *> perform print_stack_new

      *> Advance dir pointer
      compute dir_idx = function mod((dir_idx + 1) dir_cnt)
      if dir_idx = 0 move dir_cnt to dir_idx end-if

    end-perform

    *> perform print_stack

    *> Get highest piece
    if curr_piece_y(curr_piece_cnt) > curr_top
      move curr_piece_y(curr_piece_cnt) to curr_top
    end-if

    perform flatten_and_check

    *> display "ROUND " curr_round " HEIGHT: " curr_top
    add 1 to curr_round
  end-perform

  compute num_cycles = (num_rounds - (cycle_beg - 1)) / cycle_rounds
  display "NUM CYCLES: " num_cycles

  *> This includes prefix_rounds!
  compute left_over_rounds = num_rounds - (cycle_rounds * num_cycles)
  display "LEFTOVER ROUNDS: " left_over_rounds

  *> This includes prefix_height, so we can leave off on final calc.
  move history(left_over_rounds)(13:8) to left_over_height
  display "LEFTOVER HEIGHT: " left_over_height

  *> display "LAST PIECE: " function trim(history(history_cnt))

  compute total_found = (num_cycles * cycle_height) + left_over_height
  display "FINAL: " total_found " " curr_top

  *> perform print_stack

  goback.


next_piece.
  initialize curr_piece_stuff
  compute curr_new_y = curr_top + 4

  if function mod (curr_round 5) = 1
    move 4 to curr_piece_cnt
    move 1 to curr_piece_num
    move          3 to curr_piece_x(1)
    move curr_new_y to curr_piece_y(1)
    move          4 to curr_piece_x(2)
    move curr_new_y to curr_piece_y(2)
    move          5 to curr_piece_x(3)
    move curr_new_y to curr_piece_y(3)
    move          6 to curr_piece_x(4)
    move curr_new_y to curr_piece_y(4)
  end-if
  if function mod (curr_round 5) = 2
    move 5 to curr_piece_cnt
    move 2 to curr_piece_num
    move          4 to curr_piece_x(1)
    move curr_new_y to curr_piece_y(1)
    move          3 to curr_piece_x(2)
    compute curr_piece_y(2) = curr_new_y + 1
    move          4 to curr_piece_x(3)
    compute curr_piece_y(3) = curr_new_y + 1
    move          5 to curr_piece_x(4)
    compute curr_piece_y(4) = curr_new_y + 1
    move          4 to curr_piece_x(5)
    compute curr_piece_y(5) = curr_new_y + 2
  end-if
  if function mod (curr_round 5) = 3
    move 5 to curr_piece_cnt
    move 3 to curr_piece_num
    move          3 to curr_piece_x(1)
    move curr_new_y to curr_piece_y(1)
    move          4 to curr_piece_x(2)
    move curr_new_y to curr_piece_y(2)
    move          5 to curr_piece_x(3)
    move curr_new_y to curr_piece_y(3)
    move          5 to curr_piece_x(4)
    compute curr_piece_y(4) = curr_new_y + 1
    move          5 to curr_piece_x(5)
    compute curr_piece_y(5) = curr_new_y + 2
  end-if
  if function mod (curr_round 5) = 4
    move 4 to curr_piece_cnt
    move 4 to curr_piece_num
    move          3 to curr_piece_x(1)
    move curr_new_y to curr_piece_y(1)
    move          3 to curr_piece_x(2)
    compute curr_piece_y(2) = curr_new_y + 1
    move          3 to curr_piece_x(3)
    compute curr_piece_y(3) = curr_new_y + 2
    move          3 to curr_piece_x(4)
    compute curr_piece_y(4) = curr_new_y + 3
  end-if
  if function mod (curr_round 5) = 0
    move 4 to curr_piece_cnt
    move 5 to curr_piece_num
    move          3 to curr_piece_x(1)
    move curr_new_y to curr_piece_y(1)
    move          3 to curr_piece_x(2)
    compute curr_piece_y(2) = curr_new_y + 1
    move          4 to curr_piece_x(3)
    move curr_new_y to curr_piece_y(3)
    move          4 to curr_piece_x(4)
    compute curr_piece_y(4) = curr_new_y + 1
  end-if
  .


flatten_and_check.
  *> Write into the history in a compact format.
  move spaces to history_buffer
  move curr_piece_x(1) to history_num_2
  move curr_round to history_num_3
  move curr_top to history_num_4
  string curr_piece_num history_num_2 " " history_num_3 " " history_num_4 into history_buffer end-string

  add 1 to history_cnt
  move history_buffer to history(history_cnt)

  *> Check to see if we've seen this before, using the custom gap amount.
  move 0 to history_test_cnt
  if history_cnt > (history_test_gap * 2) and history_found = 0
    perform varying history_idx from curr_round by -1 until history_idx < (curr_round - history_test_gap + 1)
      add 1 to history_test_cnt
      move history(history_idx) to history_test(history_test_cnt)
    end-perform
    *> display "TEST BLOCK: [" no advancing
    *> perform varying history_test_idx from 1 by 1 until history_test_idx > history_test_cnt
    *>   display function trim(history_test(history_test_idx)) ", " no advancing
    *> end-perform
    *> display "]"
    *> display "HISTORY: [" no advancing
    *> perform varying history_idx from 1 by 1 until history_idx > history_cnt
    *>   display function trim(history(history_idx)) ", " no advancing
    *> end-perform
    *> display "]"

    compute history_start = curr_round - history_test_gap + 1
    *> display "START: " history_start
    set history_idx to history_start
    perform varying history_idx from history_start by -1 until history_idx < history_test_gap or history_found = 1
      *> display "TESTING: [" history_idx "] " history(history_idx) " vs " history_test(1)
      if history(history_idx)(1:2) = history_test(1)(1:2)
        move 1 to history_dupe
        set history_test_idx to 2
        perform varying history_test_idx from 2 by 1 until history_test_idx > history_test_cnt or history_dupe = 0
          *> display "COMPARE: " history_idx " " history_test_idx " >> " history(history_idx - (history_test_idx - 1))(1:2) " = " history_test(history_test_idx)(1:2)
          if history(history_idx - (history_test_idx - 1))(1:2) <> history_test(history_test_idx)(1:2)
            move 0 to history_dupe
          end-if
        end-perform
        if history_dupe = 1
          display "FOUND DUPE: " function trim(history(history_idx - history_test_gap + 1)) " = " function trim(history_test(history_test_gap))
          move 1 to history_found

          *> Cycle ends 1 before the dupes begin, but it gets remove if you use the first dupe round
          move history_test(history_test_gap)(4:8) to cycle_end
          move history(history_idx - history_test_gap + 1)(4:8) to cycle_beg
          compute cycle_rounds = cycle_end - cycle_beg
          display "CYCLE ROUNDS: " cycle_end " - " cycle_beg " = " cycle_rounds
          *> 1730
          *> Cycle ends 1 before the dupes begin, so be wary of height calcs. Also, history_test goes down as idx goes up.
          move history_test(history_test_gap)(13:8) to cycle_height_end
          move history(history_idx - history_test_gap + 1)(13:8) to cycle_height_beg
          compute cycle_height = cycle_height_end - cycle_height_beg
          display "CYCLE HEIGHTS: " cycle_height_end " - " cycle_height_beg " = " cycle_height
          *> 2644
          move history(history_idx - history_test_gap + 1)(13:8) to prefix_height
          display "PREFIX HEIGHT: " prefix_height
          *> 114
        end-if
      end-if
    end-perform
  end-if

*> Garbage. Using the output is intensive and we don't have an idea of what round produced what fallen items.
*> Left to gawk at my incompetence.
*>   set print_row_idx to 1
*>   compute print_top = curr_top + 1
*>   compute print_y_limit = print_top - 100
*>   move spaces to flatten_buffer
*>   set flatten_buffer_idx to 0
*> *>   if print_top > 100
*>     perform varying print_row_idx from print_top by -1 until print_row_idx < print_y_limit
*>     *> perform varying print_row_idx from print_top by -1 until print_row_idx < 1
*>        set print_col_idx to 1
*>        perform varying print_col_idx from 1 by 1 until print_col_idx > 7
*>          move 0 to print_found
*>          perform varying piece_fallen_idx from 1 by 1 until piece_fallen_idx > piece_fallen_cnt or print_found = 1
*>            if piece_fallen_x(piece_fallen_idx) = print_col_idx and piece_fallen_y(piece_fallen_idx) = print_row_idx
*>              move 1 to print_found
*>            end-if
*>          end-perform
*>          add 1 to flatten_buffer_idx
*>          if print_found = 1
*>            move "#" to flatten_buffer(flatten_buffer_idx:1)
*>          else
*>            move "." to flatten_buffer(flatten_buffer_idx:1)
*>          end-if
*>        end-perform
*>       *>  display space
*>     end-perform
*>
*>     move 0 to history_dupe
*>     set flatten_buffer_idx to 1
*>     perform varying flatten_history_idx from 1 by 1 until flatten_history_idx > flatten_history_cnt
*>       if flatten_history_line(flatten_history_idx) = flatten_buffer
*>         move 1 to history_dupe
*>         display "DUPE: RND[" curr_round "] TOP[" curr_top "] IDX[" flatten_history_idx "] TOP[" flatten_history_top(flatten_history_idx) "] " function trim(flatten_history(flatten_history_idx))
*>       end-if
*>     end-perform
*>     if history_dupe = 0
*>       add 1 to flatten_history_cnt
*>       move flatten_buffer to flatten_history_line(flatten_history_cnt)
*>       move curr_top to flatten_history_top(flatten_history_cnt)
*>     *>   display "ADDED: " function trim(flatten_buffer)
*>     end-if
*> *>   end-if
  .


*> Just for showing the stack and debug.
print_stack.
  display "STACK: "
  set print_row_idx to 1
  compute print_top = curr_top + 4
  perform varying print_row_idx from 1 by 1 until print_row_idx > curr_top
     set print_col_idx to 1
     display "[" print_row_idx "] " no advancing
     perform varying print_col_idx from 1 by 1 until print_col_idx > 7
       move 0 to print_found
       perform varying piece_fallen_idx from 1 by 1 until piece_fallen_idx > piece_fallen_cnt or print_found = 1

         if piece_fallen_x(piece_fallen_idx) = print_col_idx and piece_fallen_y(piece_fallen_idx) = print_row_idx
           move 1 to print_found
         end-if
       end-perform
       if print_found = 1
         display "#" no advancing
       else
         display "." no advancing
       end-if
     end-perform
     display space
  end-perform
  .

*> Just for showing the stack and debug.
print_stack_new.
  *> display "NEW: "
  set print_row_idx to 1
  compute print_top = curr_top + 7
  compute print_y_limit = print_top - 20
  perform varying print_row_idx from print_top by -1 until print_row_idx < print_y_limit
  *> perform varying print_row_idx from print_top by -1 until print_row_idx < 1
     set print_col_idx to 1
     perform varying print_col_idx from 1 by 1 until print_col_idx > 7
       move 0 to print_found
       perform varying piece_fallen_idx from 1 by 1 until piece_fallen_idx > piece_fallen_cnt or print_found = 1
         if piece_fallen_x(piece_fallen_idx) = print_col_idx and piece_fallen_y(piece_fallen_idx) = print_row_idx
           move 1 to print_found
         end-if
       end-perform
       if print_found = 1
         display "#" no advancing
       else
         perform varying curr_piece_idx from 1 by 1 until curr_piece_idx > curr_piece_cnt or print_found = 1
           if curr_piece_x(curr_piece_idx) = print_col_idx and curr_piece_y(curr_piece_idx) = print_row_idx
             move 1 to print_found
           end-if
         end-perform
         if print_found = 1
           display "@" no advancing
         else
           display "." no advancing
         end-if
       end-if
     end-perform
     display space
  end-perform
  .
