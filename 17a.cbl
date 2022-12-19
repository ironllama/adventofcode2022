           >>source format free
identification division.
program-id. 17a.

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
      *> OMG, took me too long to figure out this ended up truncated
      02 dir pic s9 occurs 0 to 99999 times
          depending on dir_cnt indexed by dir_idx.

    01 curr_dir usage is index.
    01 temp_dir_idx usage is index.
    01 curr_round pic s9(8) comp.
    01 curr_top pic s9(8) comp.
    01 curr_new_y pic s9(8) comp.
    01 curr_piece_done pic 9.
    01 curr_piece_stuff.
      02 curr_piece_cnt pic 9.
      02 curr_piece occurs 5 times indexed by curr_piece_idx.
        03 curr_piece_x pic s9(8) comp.
        03 curr_piece_y pic s9(8) comp.

    01 move_valid pic 9.

    01 piece_fallen_stuff.
      02 piece_fallen_cnt pic s9(8) comp.
      02 piece_fallen occurs 0 to 9999 times
          depending on piece_fallen_cnt indexed by piece_fallen_idx.
        03 piece_fallen_x pic s9(8) comp.
        03 piece_fallen_y pic s9(8) comp.

    01 print_top pic s9(8) comp. 
    01 print_y_limit pic s9(8) comp.
    01 print_row_idx usage is index.
    01 print_col_idx usage is index.
    01 print_found pic 9.

    77 total_found pic s9(8) comp.

procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
  *> call 'lib-readdata' using function module-id ".da1" rf_all_lines

  move 0 to total_found

  move length of function trim(rf_row(1)) to dir_cnt
  *> display "LENGTH: " length of function trim(rf_row(1))
  perform varying dir_idx from 1 by 1 until dir_idx > dir_cnt
    *> move rf_row(1)(dir_idx:1) to dir(dir_idx)
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

  *> perform until curr_round > 20
  perform until curr_round > 2022
    perform next_piece
    *> display "NEW PIECE: [" no advancing
    *> perform varying curr_piece_idx from 1 by 1 until curr_piece_idx > curr_piece_cnt
      *> display "(" curr_piece_x(curr_piece_idx) ", " curr_piece_y(curr_piece_idx) "), " no advancing
    *> end-perform
    *> display space
    *> display "NEW PIECE"
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
      *>    display "(" piece_fallen_x(piece_fallen_idx) ", " piece_fallen_y(piece_fallen_idx) "), "
        *> end-perform

      end-if
          
      *> display "DIR: " dir(dir_idx)
      *> perform print_stack_new

      *> Advance dir pointer
      compute dir_idx = function mod((dir_idx + 1) dir_cnt)
      if dir_idx = 0 move dir_cnt to dir_idx end-if

      *> if dir_idx = dir_cnt display "LAST ELEMENT"
      *> else if dir_idx = 1 display "FIRST ELEMENT" end-if
      *> end-if


    end-perform

    *> perform print_stack

    *> Get highest piece
    if curr_piece_y(curr_piece_cnt) > curr_top
      move curr_piece_y(curr_piece_cnt) to curr_top
    end-if

    *> display "ROUND " curr_round " HEIGHT: " curr_top
    *> end-if

    add 1 to curr_round
  end-perform

  display "FINAL: " curr_top

  *> perform print_stack

  goback.


next_piece.
  initialize curr_piece_stuff
  compute curr_new_y = curr_top + 4

  if function mod (curr_round 5) = 1
    move 4 to curr_piece_cnt
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


print_stack.
  display "STACK: "

  set print_row_idx to 1
  compute print_top = curr_top + 4
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
         display "." no advancing
       end-if
     end-perform
     display space
  end-perform
  .

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
 