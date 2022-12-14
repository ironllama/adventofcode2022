           >>source format free
identification division.
program-id. 13b.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_line_cnt pic s9(8) comp value 0.
      02 rf_line_row pic x(9999) occurs 0 to 9999 times
          depending on rf_line_cnt indexed by rf_line_idx.

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

    77 temp_buffer pic x(999).
    77 order_status pic 9.
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

    *> For lib-heapsort
    01 new_lines.
      02 new_cnt pic s9(4) comp value 0.
      02 new_row pic x(999) occurs 0 to 999 times
          depending on new_cnt indexed by new_idx.
    01 compare.
      02 compare_func procedure-pointer.
      02 compare_one pic x(999).
      02 compare_two pic x(999).
      02 compare_res pic 9 value 0.

    77 packet_one_pos pic 9(4) comp.
    77 packet_two_pos pic 9(4) comp.

    77 total_found pic 9(8).

procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
*>   call 'lib-readdata' using function module-id ".da1" rf_all_lines

  move 0 to total_found

  *> Filter out the blank lines from the input.
  perform varying rf_line_idx from 1 by 1 until rf_line_idx > rf_line_cnt
    *> display function trim(rf_line_row(rf_line_idx))
    if rf_line_row(rf_line_idx) <> space
      add 1 to new_cnt
      move function trim(rf_line_row(rf_line_idx)) to new_row(new_cnt)
    end-if
  end-perform
  *> perform varying new_idx from 1 by 1 until new_idx > new_cnt
  *>   display function trim(new_row(new_idx))
  *> end-perform

  *> Add in the new lines, as requested.
  add 1 to new_cnt
  move "[[2]]" to new_row(new_cnt)
  add 1 to new_cnt
  move "[[6]]" to new_row(new_cnt)

  *> BOO. What kind of sort is this? Doesn't work.
  *> sort new_row descending

  *> Custom heap sort.
  set compare_func to entry "sort-compare"
  call 'lib-heapsort' using new_lines compare

  *> Get positions of injected packets.
  perform varying new_idx from 1 by 1 until new_idx > new_cnt
    *> display function trim(new_row(new_idx))
    if function trim(new_row(new_idx)) = "[[2]]"
      move new_idx to packet_one_pos
    end-if
    if function trim(new_row(new_idx)) = "[[6]]"
      move new_idx to packet_two_pos
    end-if
  end-perform

  compute total_found = packet_one_pos * packet_two_pos
  display "FINAL: " total_found

  goback.


entry "sort-compare"
  *> display "COMPARE: " function trim(compare_one) " vs " function trim(compare_two)
  initialize tokens_stack

  move compare_one to left_string
  move compare_two to right_string
  perform process_groups

  *> move order_status to compare_res
  if order_status = 1
    move 2 to compare_res
  else
    if order_status = 2
      move 1 to compare_res
    end-if
  end-if
  .

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
