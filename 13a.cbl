           >>source format free
identification division.
program-id. 13a.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_line_cnt pic s9(8) comp value 0.
      02 rf_line_row pic x(9999) occurs 0 to 9999 times
          depending on rf_line_cnt indexed by rf_line_idx.

    77 pair_number pic s9(4) value 0.
    *> 77 pair_number_str pic z(4).
    77 order_status pic 9.
    77 temp_buffer pic x(999).

    01 tokens_stack.
      02 tokens_stack_num pic 9(4) comp.
      02 tokens_stack_val occurs 99 times indexed by tokens_stack_idx.
        03 tokens_idx pic 9(4) comp.
        03 tokens_one_stuff.
          04 tokens_one_num pic 9(4) comp.
          04 tokens_one pic x(999) occurs 99 times indexed by tokens_one_idx.
        03 tokens_two_stuff.
          04 tokens_two_num pic 9(4) comp.
          04 tokens_two pic x(999) occurs 99 times indexed by tokens_two_idx.

    77 left_string pic x(999).
    77 right_string pic x(999).
    77 left_item_val pic s9(4) comp.
    77 right_item_val pic s9(4) comp.
    77 curr_child pic x(999).
    77 curr_child_len pic s9(4) comp.
    77 curr_child_idx pic s9(4) comp.
    77 curr_child_level pic s9(4) comp.
    77 curr_child_buffer pic x(999).
    01 curr_child_stuff.
      02 curr_child_item_num pic s9(4) comp.
      02 curr_child_item pic x(999) occurs 99 times indexed by curr_child_item_idx.

    77 total_found pic s9(8) comp.

procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
*>   call 'lib-readdata' using function module-id ".da1" rf_all_lines

  move 0 to total_found
  perform varying rf_line_idx from 1 by 3 until rf_line_idx > rf_line_cnt
    add 1 to pair_number

    initialize tokens_stack

    move function trim(rf_line_row(rf_line_idx)) to left_string
    move function trim(rf_line_row(rf_line_idx + 1)) to right_string
    perform process_groups

    if order_status = 1
      compute total_found = total_found + pair_number
    end-if

    *> Print for debugging.
    *> move pair_number to pair_number_str
    *> if order_status = 1
      *> display "[" pair_number "] RIGHT: " function trim(left_string) " vs " function trim(right_string)
      *> *> display "[" function trim(pair_number_str) "] RIGHT"
    *> else
      *> if order_status = 2
        *> display "[" pair_number "] WRONG: " function trim(left_string) " vs " function trim(right_string)
        *> *> display "[" function trim(pair_number_str) "] WRONG"
      *> else
        *> display "[" pair_number "] ???: " function trim(left_string) " vs " function trim(right_string)
        *> *> display "[" function trim(pair_number_str) "] WRONG"
      *> end-if
    *> end-if

  end-perform

  display "FINISHED: " total_found

  goback.


process_groups.
  add 1 to tokens_stack_num

  move function trim(left_string) to curr_child
  perform process_children
  add 1 to tokens_one_num(tokens_stack_num)
  move curr_child_stuff to tokens_one_stuff(tokens_stack_num)

  *> display "TOKENS ONE [" tokens_stack_num "]: " no advancing
  *> perform varying tokens_one_idx from 1 by 1 until tokens_one_idx > tokens_one_num(tokens_stack_num)
  *>   display function trim(tokens_one(tokens_stack_num tokens_one_idx)) ", " no advancing
  *> end-perform
  *> display space

  move function trim(right_string) to curr_child
  perform process_children
  add 1 to tokens_two_num(tokens_stack_num)
  move curr_child_stuff to tokens_two_stuff(tokens_stack_num)

  *> display "TOKENS TWO [" tokens_stack_num "]: " no advancing
  *> perform varying tokens_two_idx from 1 by 1 until tokens_two_idx > tokens_two_num(tokens_stack_num)
  *>   display function trim(tokens_two(tokens_stack_num tokens_two_idx)) ", " no advancing
  *> end-perform
  *> display space

  *> For each child item in the list
  move 0 to order_status
  perform varying tokens_idx(tokens_stack_num) from 1 by 1 until
      (tokens_one(tokens_stack_num tokens_idx(tokens_stack_num)) = space
      and tokens_two(tokens_stack_num tokens_idx(tokens_stack_num)) = space)
      or order_status <> 0

    *> display "COMPARING: " function trim(tokens_one(tokens_stack_num tokens_idx(tokens_stack_num))) " vs " function trim(tokens_two(tokens_stack_num tokens_idx(tokens_stack_num)))

    *> Check to see if left or right are empty values.
    if order_status = 0
        and tokens_one(tokens_stack_num tokens_idx(tokens_stack_num)) = space
        and tokens_two(tokens_stack_num tokens_idx(tokens_stack_num)) <> space
        *> display "LEFT EMPTY - RIGHT!"
      move 1 to order_status
    end-if
    if order_status = 0
        and tokens_two(tokens_stack_num tokens_idx(tokens_stack_num)) = space
        and tokens_one(tokens_stack_num tokens_idx(tokens_stack_num)) <> space
        *> display "RIGHT EMPTY - WRONG!"
      move 2 to order_status
    end-if

    *> Would normally want to put this logically after handling arrays and recursion, below.
    *> However, not fully trusting my own manual stack implementation... heh.
    if order_status = 0
        and tokens_one(tokens_stack_num tokens_idx(tokens_stack_num))(1:1) <> "["
        and tokens_two(tokens_stack_num tokens_idx(tokens_stack_num))(1:1) <> "["
      if order_status = 0 and tokens_idx(tokens_stack_num) > tokens_one_num(tokens_stack_num)
        *> display "LEFT FINISHED FIRST - RIGHT!"
        move 1 to order_status
      end-if
      if order_status = 0 and tokens_idx(tokens_stack_num) > tokens_two_num(tokens_stack_num)
        *> display "RIGHT FINISHED FIRST - WRONG!"
        move 2 to order_status
      end-if

      if order_status = 0
        *> Covert to number for value comparisons.
        move tokens_one(tokens_stack_num tokens_idx(tokens_stack_num)) to left_item_val
        move tokens_two(tokens_stack_num tokens_idx(tokens_stack_num)) to right_item_val

        if left_item_val < right_item_val
          *> display "RIGHT!"
          move 1 to order_status
        else
          if left_item_val > right_item_val
            *> display "WRONG!"
            move 2 to order_status
          end-if
        end-if
      end-if
    end-if

    *> If only one side is an array, promote non-array to have both be arrays.
    if order_status = 0
        and tokens_one(tokens_stack_num tokens_idx(tokens_stack_num))(1:1) <> "["
        and tokens_two(tokens_stack_num tokens_idx(tokens_stack_num))(1:1) = "["
      move spaces to temp_buffer
      string "[" tokens_one(tokens_stack_num tokens_idx(tokens_stack_num)) delimited by space "]" into temp_buffer end-string
      move temp_buffer to tokens_one(tokens_stack_num tokens_idx(tokens_stack_num))
    *>   display "PROMOTED ONE: " function trim(temp_buffer)
    end-if
    if order_status = 0
        and tokens_one(tokens_stack_num tokens_idx(tokens_stack_num))(1:1) = "["
        and tokens_two(tokens_stack_num tokens_idx(tokens_stack_num))(1:1) <> "["
      move spaces to temp_buffer
      string "[" tokens_two(tokens_stack_num tokens_idx(tokens_stack_num)) delimited by space "]" into temp_buffer end-string
      move temp_buffer to tokens_two(tokens_stack_num tokens_idx(tokens_stack_num))
    *>   display "PROMOTED TWO: " function trim(temp_buffer)
    end-if
    *> If both are arrays, recurse.
    if order_status = 0
        and tokens_one(tokens_stack_num tokens_idx(tokens_stack_num))(1:1) = "["
        and tokens_two(tokens_stack_num tokens_idx(tokens_stack_num))(1:1) = "["
      move tokens_one(tokens_stack_num tokens_idx(tokens_stack_num)) to left_string
      move tokens_two(tokens_stack_num tokens_idx(tokens_stack_num)) to right_string
      perform process_groups
      subtract 1 from tokens_stack_num
    end-if
  end-perform
  .

process_children.
  move length of function trim(curr_child) to curr_child_len
  move 0 to curr_child_level
  move 0 to curr_child_item_num
  initialize curr_child_stuff
  perform varying curr_child_idx from 1 by 1 until curr_child_idx > curr_child_len
    *> display "CHAR: " curr_child(curr_child_idx:1)
    if curr_child(curr_child_idx:1) = "["
      *> display "Start new level"
      add 1 to curr_child_level
      if curr_child_level > 1
          move spaces to temp_buffer
          string curr_child_buffer delimited by space curr_child(curr_child_idx:1) into temp_buffer end-string
          move temp_buffer to curr_child_buffer
      end-if
    else
      if curr_child(curr_child_idx:1) = "]"
        *> display "End level"
        subtract 1 from curr_child_level
        *> If end of array child, write as value. Else, keep adding to buffer.
        if curr_child_level = 0
          *> display "END: " tokens_stack_num " : " function trim(curr_child_buffer)
          *> display "END ADD: " tokens_stack_num " : " function trim(curr_child_buffer)
          add 1 to curr_child_item_num
          move curr_child_buffer to curr_child_item(curr_child_item_num)
          move spaces to curr_child_buffer
        else
          *> display "Keep trucking"
          move spaces to temp_buffer
          string curr_child_buffer delimited by space curr_child(curr_child_idx:1) into temp_buffer end-string
          move temp_buffer to curr_child_buffer
        end-if
      else
        if curr_child_level = 1 and curr_child(curr_child_idx:1) = ","
          *> display "End of number at parent level"
          *> display "COMMA ADD: " tokens_stack_num " : " function trim(curr_child_buffer)
          add 1 to curr_child_item_num
          move curr_child_buffer to curr_child_item(curr_child_item_num)
          move spaces to curr_child_buffer
        end-if
        *> Should catch anything, regardless of level, and add to the buffer.
        if curr_child_level > 1 or (curr_child_level = 1 and curr_child(curr_child_idx:1) <> ",")
          *> display "BEG ALL: " function trim(curr_child_buffer) " + " curr_child(curr_child_idx:1)
          move spaces to temp_buffer
          string curr_child_buffer delimited by space curr_child(curr_child_idx:1) into temp_buffer end-string
          move temp_buffer to curr_child_buffer
          *> display "END ALL: " function trim(curr_child_buffer)
        end-if
      end-if
    end-if
  end-perform
  .





*> JUNK YARD. ALL THE STUFF THAT DIDN't QUITE WORK
*> Left for posterity.



    *> 77 num_row_one_chars pic s9(4) comp.
    *> 77 num_row_two_chars pic s9(4) comp.

    *> 77 start_row_one_chars pic s9(4) comp.
    *> 77 end_row_one_chars pic s9(4) comp.
    *> 77 start_row_two_chars pic s9(4) comp.
    *> 77 end_row_two_chars pic s9(4) comp.


    *> 77 pos_one pic 9(4) comp value 1.
    *> 77 pos_two pic 9(4) comp value 1.
    *> 77 next_one pic 9(2) comp.
    *> 77 next_two pic 9(2) comp.


    *> 77 level_one pic s9(4) comp value 0.
    *> 77 level_two pic s9(4) comp value 0.
    *> 77 level_one_val pic x(999).
    *> 77 level_two_val pic x(999).
    *> 77 char_idx pic s9(4) comp.
    *> 77 finished pic 9.
    *> 77 char_buffer pic x(4).


    *> 77 comma_break_ptr pic s9(4) comp.
    *> 77 comma_break_done pic s9.
    *> 01 level_one_nums.
    *>   02 level_one_num_len pic s9(4) comp.
    *>   02 level_one_num_val pic s9(4) comp occurs 0 to 999 times
    *>     depending on level_one_num_len
    *>     indexed by level_one_num_idx.
    *> 01 level_two_nums.
    *>   02 level_two_num_len pic s9(4) comp.
    *>   02 level_two_num_val pic s9(4) comp occurs 0 to 999 times
    *>     depending on level_two_num_len
    *>     indexed by level_two_num_idx.

    *> 77 row_idx pic 9(4) comp.
    *> 77 row_two_idx pic 9(4) comp.
    *> 77 row_two_found pic 9.


    *> move length of function trim(rf_line_row(rf_line_idx)) to num_row_one_chars
    *> move length of function trim(rf_line_row(rf_line_idx + 1)) to num_row_two_chars

    *> display "LINE 1: (" num_row_one_chars ")" function trim(rf_line_row(rf_line_idx))
    *> display "LINE 2: (" num_row_two_chars ")" function trim(rf_line_row(rf_line_idx + 1))


    *> perform until order_status <> 0
      *> perform get_next_one
      *> perform get_next_two

      *> display "AFTER GET: (" level_one ") " function trim(level_one_val) " vs (" level_two ") " function trim(level_two_val)

      *> if level_one_val(1:1) = "[" and level_two_val(1:1) <> "["
        *> move spaces to temp_buffer
        *> string "[" function trim(level_two_val) "]" into temp_buffer end-string
        *> move temp_buffer to level_two_val
        *> display "PROMOTED TWO: " level_two_val
      *> else
        *> if level_two_val(1:1) = "[" and level_one_val(1:1) <> "["
          *> move spaces to temp_buffer
          *> string "[" function trim(level_one_val) "]" into temp_buffer end-string
          *> move temp_buffer to level_one_val
          *> display "PROMOTED ONE: " level_one_val
        *> else
          *> if level_one_val(1:1) <> "[" and level_two_val(1:1) <> "["
            *> initialize level_one_nums
            *> set comma_break_ptr to 1
            *> move spaces to temp_buffer
            *> move 0 to comma_break_done
            *> perform until comma_break_done = 1
              *> unstring function trim(level_one_val) delimited by ","
                *> into temp_buffer
                *> with pointer comma_break_ptr
                *> on overflow
                  *> if comma_break_ptr > length of function trim(level_one_val)
                    *> move 1 to comma_break_done
                  *> end-if
                  *> add 1 to level_one_num_len
                  *> move temp_buffer to level_one_num_val(level_one_num_len)
              *> end-unstring
            *> end-perform

            *> initialize level_two_nums
            *> set comma_break_ptr to 1
            *> move spaces to temp_buffer
            *> move 0 to comma_break_done
            *> perform until comma_break_done = 1
              *> unstring function trim(level_two_val) delimited by ","
                *> into temp_buffer
                *> with pointer comma_break_ptr
                *> on overflow
                  *> if comma_break_ptr > length of function trim(level_two_val)
                    *> move 1 to comma_break_done
                  *> end-if
                  *> add 1 to level_two_num_len
                  *> move temp_buffer to level_two_num_val(level_two_num_len)
              *> end-unstring
            *> end-perform

            *> display "level_one_nums: " no advancing
            *> perform varying level_one_num_idx from 1 by 1 until level_one_num_idx > level_one_num_len
              *> display level_one_num_val(level_one_num_idx) ", " no advancing
            *> end-perform
            *> display space
            *> display "level_two_nums: " no advancing
            *> perform varying level_two_num_idx from 1 by 1 until level_two_num_idx > level_two_num_len
              *> display level_two_num_val(level_two_num_idx) ", " no advancing
            *> end-perform
            *> display space

            *> perform varying level_one_num_idx from 1 by 1 until level_one_num_idx > level_one_num_len or order_status > 0
              *> display "COMPARING ONE: " level_one_num_val(level_one_num_idx) " vs " level_two_num_val(level_one_num_idx)
              *> if level_one_num_val(level_one_num_idx) < level_two_num_val(level_one_num_idx)
                *> display "RIGHT"
                *> move 1 to order_status
                *> compute total_found = total_found + pair_number
                *> display "TOTAL: " total_found
              *> else
                *> if level_one_num_idx > level_two_num_len
                  *> display "TWO OUT - WRONG"
                  *> move 2 to order_status
                *> else
                  *> if level_two_num_val(level_one_num_idx) < level_one_num_val(level_one_num_idx)
                    *> display "WRONG"
                    *> move 2 to order_status
                  *> end-if
                *> end-if
              *> end-if
            *> end-perform

            *> if order_status = 0 and level_one_num_idx < level_one_num_len
                *> display "LEFT OUT - RIGHT"
                *> move 1 to order_status
                *> compute total_found = total_found + pair_number
                *> display "TOTAL: " total_found
            *> end-if



            *> *> move spaces to level_two_val
            *> *> perform varying char_idx from row_idx by 1 until char_idx > num_row_two_chars or finished = 1
              *> *> if rf_line_row(rf_line_idx + 1)(char_idx:1) = "]"
                *> *> move 1 to finished
                *> *> subtract 1 from level_two
              *> *> else
              *> *> *>   move rf_line_row(rf_line_idx + 1)(char_idx:1) to char_buffer
                *> *> move spaces to temp_buffer
                *> *> string level_two_val delimited by space
                  *> *> rf_line_row(rf_line_idx + 1)(char_idx:1)
                  *> *> into temp_buffer
                *> *> end-string
                *> *> move temp_buffer to level_two_val
              *> *> *>   display "two: ADDED char_buffer: " char_buffer
              *> *> end-if
            *> *> end-perform
          *> end-if
        *> end-if
      *> end-if

      *> *> if level_one_val < 0 or level_one_val < level_two_val
        *> *> if level_two_val < 0
      *> *>    if level_one < level_two
      *> *>      display "WRONG"
      *> *>      move 2 to order_status
      *> *>    end-if
        *> *> else
      *> *>    display "RIGHT"
      *> *>    move 1 to order_status
      *> *>    compute total_found = total_found + pair_number
      *> *>    display "TOTAL: " total_found
        *> *> end-if
      *> *> else
        *> *> if level_one_val > level_two_val
      *> *>    display "WRONG"
      *> *>    move 2 to order_status
        *> *> end-if
      *> *> end-if
    *> end-perform












          *> *> move rf_line_row(rf_line_idx)(row_idx:1) to next_one

          *> *> move 0 to row_two_found
          *> *> *> display "STARRTING WITH: " row_two_idx
          *> *> perform until row_two_idx > num_row_two_chars or row_two_found = 1
            *> *> if rf_line_row(rf_line_idx + 1)(row_two_idx:1) <> ","
              *> *> if rf_line_row(rf_line_idx + 1)(row_two_idx:1) <> "["
                *> *> if not (rf_line_row(rf_line_idx + 1)(row_two_idx:1) = "]" and rf_line_row(rf_line_idx + 1)(row_two_idx - 1:1) = "[")
                *> *> *>   display "FOUND AT: " row_two_idx
                  *> *> move rf_line_row(rf_line_idx + 1)(row_two_idx:1) to next_two
                  *> *> move 1 to row_two_found
                *> *> end-if
              *> *> end-if
            *> *> end-if
            *> *> add 1 to row_two_idx
          *> *> end-perform
          *> *> if row_two_found = 0
            *> *> display "RIGHT RAN OUT - WRONG"
          *> *> else
            *> *> if rf_line_row(rf_line_idx)(row_idx:1) = "]" AND rf_line_row(rf_line_idx)(row_idx - 1:1) =  "["
              *> *> display "COMPARING: " rf_line_row(rf_line_idx)(row_idx:1) " vs " rf_line_row(rf_line_idx + 1)(row_two_idx - 1:1)
              *> *> display "LEFT RAN OUT - RIGHT"
            *> *> else
              *> *> display "COMPARING: " next_one " vs " next_two
              *> *> if next_one < next_two
                *> *> display "RIGHT!"
                *> *> move 1 to order_status;
                *> *> compute total_found = total_found + ((rf_line_idx + 1) / 2)
              *> *> else
                *> *> if next_one > next_two
                  *> *> display "WRONG!"
                  *> *> move 2 to order_status;
                *> *> end-if
              *> *> end-if
            *> *> end-if
          *> *> end-if
        *> *> else
          *> *> *> Left side is "[", right side can be a number or "["
          *> *> move 0 to row_two_found
          *> *> perform until row_two_idx > num_row_two_chars or row_two_found = 1
            *> *> if rf_line_row(rf_line_idx + 1)(row_two_idx:1) <> "," and rf_line_row(rf_line_idx + 1)(row_two_idx:1) <> "["
              *> *> if not (rf_line_row(rf_line_idx + 1)(row_two_idx:1) = "]" and rf_line_row(rf_line_idx + 1)(row_two_idx - 1:1) = "[")
              *> *> *>   display "FOUND AT: " row_two_idx
                *> *> move rf_line_row(rf_line_idx + 1)(row_two_idx:1) to next_two
                *> *> move 1 to row_two_found
              *> *> end-if
            *> *> end-if
            *> *> add 1 to row_two_idx
          *> *> end-perform

          *> *> if (rf_line_row(rf_line_idx + 1)(pos_two:1) <> "["
          *> *> end-if

        *> *> end-if
      *> *> end-if
    *> *> end-perform
  *> end-perform

  *> display "FINAL: " total_found
  *> *> NOT 3744 8665 5880(L) 6061

  *> goback.

*> get_next_one.
  *> move space to level_one_val
  *> move 0 to finished
  *> perform varying row_idx from start_row_one_chars by 1 until row_idx > num_row_one_chars or finished = 1
    *> *> display "ONE IDX: " row_idx " CHAR: " rf_line_row(rf_line_idx)(row_idx:1)
    *> if rf_line_row(rf_line_idx)(row_idx:1) <> ","
      *> if rf_line_row(rf_line_idx)(row_idx:1) <> "["
        *> if rf_line_row(rf_line_idx)(row_idx:1) = "]"
          *> subtract 1 from level_one
          *> if rf_line_row(rf_line_idx)(row_idx - 1:1) = "["
            *> move 1 to finished
          *> end-if
        *> else
          *> move spaces to level_one_val
          *> perform varying char_idx from row_idx by 1 until char_idx > num_row_one_chars or finished = 1
            *> if rf_line_row(rf_line_idx)(char_idx:1) = "]"
              *> move 1 to finished
              *> subtract 1 from level_one
            *> else
            *> *>   move rf_line_row(rf_line_idx)(char_idx:1) to char_buffer
              *> move spaces to temp_buffer
              *> string level_one_val delimited by space
                *> rf_line_row(rf_line_idx)(char_idx:1)
                *> into temp_buffer
              *> end-string
              *> move temp_buffer to level_one_val
            *> *>   display "ONE: ADDED char_buffer: " char_buffer
            *> end-if
          *> end-perform
        *> end-if
      *> else
         *> add 1 to level_one

         *> move spaces to level_one_val
         *> perform varying char_idx from row_idx by 1 until char_idx > num_row_one_chars or finished = 1
           *> *> move rf_line_row(rf_line_idx)(char_idx:1) to char_buffer
           *> move spaces to temp_buffer
           *> string level_one_val delimited by space
             *> rf_line_row(rf_line_idx)(char_idx:1)
             *> into temp_buffer
           *> end-string
           *> move temp_buffer to level_one_val
           *> *> display "ONE: ADDED char_buffer: " char_buffer
           *> if rf_line_row(rf_line_idx)(char_idx:1) = "]"
             *> move 1 to finished
             *> subtract 1 from level_one
           *> end-if
         *> end-perform

      *> end-if
    *> end-if
  *> end-perform
  *> move row_idx to start_row_one_chars
  *> move char_idx to end_row_one_chars

  *> *> move -1 to level_one_val
  *> *> move 0 to finished
  *> *> perform varying row_idx from start_row_one_chars by 1 until row_idx > num_row_one_chars or finished = 1
    *> *> *> display "ONE IDX: " row_idx " CHAR: " rf_line_row(rf_line_idx)(row_idx:1)
    *> *> if rf_line_row(rf_line_idx)(row_idx:1) <> ","
      *> *> if rf_line_row(rf_line_idx)(row_idx:1) <> "["
        *> *> if rf_line_row(rf_line_idx)(row_idx:1) = "]"
          *> *> subtract 1 from level_one
          *> *> if rf_line_row(rf_line_idx)(row_idx - 1:1) = "["
            *> *> move 1 to finished
          *> *> end-if
        *> *> else
          *> *> move spaces to char_buffer
          *> *> perform varying char_idx from row_idx by 1 until char_idx > num_row_one_chars or finished = 1
            *> *> if rf_line_row(rf_line_idx)(char_idx:1) = "," or rf_line_row(rf_line_idx)(char_idx:1) = "]"
              *> *> move char_buffer to level_one_val
              *> *> move 1 to finished
             *> *> if rf_line_row(rf_line_idx)(row_idx:1) = "]"
               *> *> subtract 1 from level_one
             *> *> end-if
            *> *> else
            *> *> *>   move rf_line_row(rf_line_idx)(char_idx:1) to char_buffer
              *> *> move spaces to temp_buffer
              *> *> string char_buffer delimited by space
                *> *> rf_line_row(rf_line_idx)(char_idx:1)
                *> *> into temp_buffer
              *> *> end-string
              *> *> move temp_buffer to char_buffer
            *> *> *>   display "ONE: ADDED char_buffer: " char_buffer
            *> *> end-if
          *> *> end-perform
        *> *> end-if
      *> *> else
         *> *> add 1 to level_one
      *> *> end-if
    *> *> end-if
  *> *> end-perform
  *> *> move row_idx to start_row_one_chars
  *> .

*> get_next_two.
  *> move space to level_two_val
  *> move 0 to finished
  *> perform varying row_idx from start_row_two_chars by 1 until row_idx > num_row_two_chars or finished = 1
    *> *> display "two IDX: " row_idx " CHAR: " rf_line_row(rf_line_idx + 1)(row_idx:1)
    *> if rf_line_row(rf_line_idx + 1)(row_idx:1) <> ","
      *> if rf_line_row(rf_line_idx + 1)(row_idx:1) <> "["
        *> if rf_line_row(rf_line_idx + 1)(row_idx:1) = "]"
          *> subtract 1 from level_two
          *> if rf_line_row(rf_line_idx + 1)(row_idx - 1:1) = "["
            *> move 1 to finished
          *> end-if
        *> else
          *> move spaces to level_two_val
          *> perform varying char_idx from row_idx by 1 until char_idx > num_row_two_chars or finished = 1
            *> if rf_line_row(rf_line_idx + 1)(char_idx:1) = "]"
              *> move 1 to finished
              *> subtract 1 from level_two
            *> else
            *> *>   move rf_line_row(rf_line_idx + 1)(char_idx:1) to char_buffer
              *> move spaces to temp_buffer
              *> string level_two_val delimited by space
                *> rf_line_row(rf_line_idx + 1)(char_idx:1)
                *> into temp_buffer
              *> end-string
              *> move temp_buffer to level_two_val
            *> *>   display "two: ADDED char_buffer: " char_buffer
            *> end-if
          *> end-perform
        *> end-if
      *> else
         *> add 1 to level_two
      *> end-if
    *> end-if
  *> end-perform
  *> move row_idx to start_row_two_chars
  *> move char_idx to end_row_two_chars

  *> *> move -1 to level_two_val
  *> *> move 0 to finished
  *> *> perform varying row_idx from start_row_two_chars by 1 until row_idx > num_row_two_chars or finished = 1
    *> *> *> display "TWO IDX: " row_idx " CHAR: " rf_line_row(rf_line_idx + 1)(row_idx:1)
    *> *> if rf_line_row(rf_line_idx + 1)(row_idx:1) <> ","
      *> *> if rf_line_row(rf_line_idx + 1)(row_idx:1) <> "["
        *> *> if rf_line_row(rf_line_idx + 1)(row_idx:1) = "]"
          *> *> subtract 1 from level_two
          *> *> if rf_line_row(rf_line_idx + 1)(row_idx - 1:1) = "["
            *> *> move 1 to finished
          *> *> end-if
        *> *> else
          *> *> move spaces to char_buffer
          *> *> perform varying char_idx from row_idx by 1 until char_idx > num_row_two_chars or finished = 1
            *> *> if rf_line_row(rf_line_idx + 1)(char_idx:1) = "," or rf_line_row(rf_line_idx + 1)(char_idx:1) = "]"
              *> *> move char_buffer to level_two_val
              *> *> move 1 to finished
             *> *> if rf_line_row(rf_line_idx)(row_idx:1) = "]"
               *> *> subtract 1 from level_one
             *> *> end-if
            *> *> else
            *> *> *>   move rf_line_row(rf_line_idx + 1)(char_idx:1) to char_buffer
              *> *> move spaces to temp_buffer
              *> *> string char_buffer delimited by space
                *> *> rf_line_row(rf_line_idx + 1)(char_idx:1)
                *> *> into temp_buffer
              *> *> end-string
              *> *> move temp_buffer to char_buffer
            *> *> *>   display "TWO: ADDED char_buffer: " char_buffer
            *> *> end-if
          *> *> end-perform
        *> *> end-if
      *> *> else
         *> *> add 1 to level_two
      *> *> end-if
    *> *> end-if
  *> *> end-perform
  *> *> move row_idx to start_row_two_chars
  *> .


    *> 77 get_number_buffer pic x(999).
    *> 77 get_number_buffer_len pic s9(4) comp.
    *> 77 get_number_done pic 9.
    *> 77 get_number_idx pic s9(4) comp.

*> *> get_number.
*> *>   move length of function trim(get_number_val) to get_number_len
*> *>   move 0 to get_number_done
*> *>   move spaces to temp_buffer
*> *>   move spaces to get_number_buffer
*> *>   move 0 to get_number_buffer_len
*> *>   perform varying get_number_idx from 1 by 1 until get_number_idx > get_number_len or get_number_done = 1
*> *>     move get_number_val(get_number_idx) to get_number_next_char
*> *>     if get_number_len or get_number_next_char = "," or get_number_next_char = "]"
*> *>       move 1 to get_number_done
*> *>     else
*> *>       string get_number_buffer get_number_next_char into temp_buffer end-string
*> *>       move temp_buffer to get_number_buffer
*> *>       add 1 to get_number_buffer_len
*> *>     end-if
*> *>   end-perform
*> *>   .
