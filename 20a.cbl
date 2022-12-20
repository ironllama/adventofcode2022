           >>source format free
identification division.
program-id. 20a.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_cnt pic s9(8) comp value 0.
      02 rf_row pic x(9999) occurs 0 to 5001 times
          depending on rf_cnt indexed by rf_idx.

    01 all_nums.
      02 num_cnt pic s9(8) comp value 0.
      02 num_row occurs 5001 times indexed by num_idx.
        03 num pic s9(6) comp.
        03 num_orig_idx usage is index.

    01 current.
      02 curr_num pic s9(6) comp.
      02 curr_orig_idx usage is index.

    77 curr_num_idx usage is index.
    77 move_idx usage is index.
    77 move_old_idx usage is index.
    77 move_dir pic s9.
    77 move_num pic s9(6) comp.
    77 start_idx usage is index.
    77 end_idx usage is index.
    77 num_found pic 9.
    77 coord_val pic 9(4) comp.

    77 print_idx usage is index.

    77 cycle_idx usage is index.

    77 num_1 pic s9(6) comp.
    77 num_2 pic s9(6) comp.
    77 num_3 pic s9(6) comp.
    77 total_found pic s9(8) comp.

procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
*>   call 'lib-readdata' using function module-id ".da1" rf_all_lines

  move 0 to total_found
  perform varying rf_idx from 1 by 1 until rf_idx > rf_cnt
    *> display "LINE: " function trim(rf_row(rf_idx))

    *> There are duplicate values. AARG. >.<
    *> perform varying num_idx from 1 by 1 until num_idx > num_cnt
    *>   move rf_row(rf_idx) to move_num
    *>   if num(num_idx) = move_num display "DUPE: " move_num end-if
    *> end-perform

    add 1 to num_cnt
    move rf_row(rf_idx) to num(num_cnt)
    move rf_idx to num_orig_idx(num_cnt)
  end-perform

  *> display "NUMS: [" no advancing
  *> perform varying num_idx from 1 by 1 until num_idx > num_cnt
  *>   display num(num_idx) no advancing
  *>   if num_idx < num_cnt display ", " no advancing
  *> end-perform
  *> display "]"

  perform varying rf_idx from 1 by 1 until rf_idx > rf_cnt
    *> display "CURR NUM: " function trim(rf_row(rf_idx))
    move rf_row(rf_idx) to curr_num
    move rf_idx to curr_orig_idx
    *> Find the number
    if curr_num <> 0
      move 0 to num_found
      set curr_num_idx to -1
      perform varying num_idx from 1 by 1 until num_idx > num_cnt
        if num(num_idx) = curr_num and num_orig_idx(num_idx) = curr_orig_idx
          move 1 to num_found
          set curr_num_idx to num_idx
          move num_row(num_idx) to current
        end-if
      end-perform

      if curr_num < 0 set move_dir to -1 end-if
      if curr_num > 0 set move_dir to 1 end-if

      move function mod(function abs(curr_num) (num_cnt - 1)) to move_num

      *> display "MOVING: curr_num " curr_num " curr_num_idx " curr_num_idx " move_dir " move_dir " move_num " move_num " num_cnt " num_cnt
      perform varying move_idx from curr_num_idx by move_dir until move_num = 0
        move move_idx to cycle_idx
        perform cycle_nums
        move cycle_idx to move_idx

        compute move_old_idx = move_idx + move_dir
        move move_old_idx to cycle_idx
        perform cycle_nums
        move cycle_idx to move_old_idx

        *> move num(move_old_idx) to num(move_idx)
        move num_row(move_old_idx) to num_row(move_idx)

        subtract 1 from move_num
      end-perform
      *> Move the curr_num using leftover index from above loop.
      move move_idx to cycle_idx
      perform cycle_nums
      move cycle_idx to move_idx

      *> move curr_num to num_row(move_idx)
      move current to num_row(move_idx)

      *> perform print_nums
    end-if
  end-perform

  *> perform print_nums

  move 0 to num_found
  perform varying num_idx from 1 by 1 until num_idx > num_cnt
    if num(num_idx) = 0
      move 1 to num_found
      move num_idx to start_idx
    end-if
  end-perform
  *> display "ZERO AT: " start_idx " OF " num_cnt

  *> Run through cycle function, in case they land at the last idx.
  move 1000 to coord_val
  perform get_coords
  move num(end_idx) to num_1

  move 2000 to coord_val
  perform get_coords
  move num(end_idx) to num_2

  move 3000 to coord_val
  perform get_coords
  move num(end_idx) to num_3

  *> display "NUMS: " num_1  " " num_2 " " num_3

  compute total_found = num_1 + num_2 + num_3
  display "FINAL: " total_found

  goback.


print_nums.
  display "NUMS: [" no advancing
  perform varying print_idx from 1 by 1 until print_idx > num_cnt
    display num(print_idx) no advancing
    if print_idx < num_cnt display ", " no advancing
  end-perform
  display "]"
  .

cycle_nums.
  if cycle_idx = 0
    set cycle_idx to num_cnt
  end-if
  if cycle_idx > num_cnt
    set cycle_idx to 1
  end-if
  .

get_coords.
  set end_idx to function mod((start_idx + coord_val) num_cnt)
  move end_idx to cycle_idx
  perform cycle_nums
  move cycle_idx to end_idx
  *> display "END: " end_idx
  .
