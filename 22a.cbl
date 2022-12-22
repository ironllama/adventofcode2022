           >>source format free
identification division.
program-id. 22a.

data division.
  working-storage section.
    *> For lib-readfile
    *> NOTE: WHITE SPACE IS SIGNIFICANT IN THE INPUT. DISABLE THE TRIM!
    01 rf_all_lines.
      02 rf_cnt pic s9(8) comp value 0.
      02 rf_row pic x(9999) occurs 0 to 9999 times
          depending on rf_cnt indexed by rf_idx.

    01 rf_char_idx usage is index.
    01 rf_chars_in_row pic s9(4) comp.
    01 rf_chars_in_col pic s9(4) comp.

    01 map_stuff.
    *>   02 map_cnt pic s9(8) comp.
      02 map_row occurs 200 times indexed by map_row_idx.
        *> 03 map_row_start usage is index.
        *> 03 map_row_end usage is index.
        *> 03 map_row_size pic s9(4) comp.
        03 map_col occurs 150 times indexed by map_col_idx.
          04 map pic x.

    01 dir_str pic x(5650).
    *> 01 dir_str_cnt pic 9(4) comp.
    01 dir_str_idx usage is index.
    01 dir_buff pic x(4).
    01 dir_num pic 9(4) comp.
    01 test_facing pic 9.
    01 dir_blocked pic 9.
    01 move_idx usage is index.

    01 test_x pic s9(3) comp.
    01 test_y pic s9(3) comp.
    01 curr_x pic s9(3) comp value 0.
    01 curr_y pic s9(3) comp value 0.
    01 curr_facing pic s9.

    01 path_stuff.
      02 path occurs 9999 times indexed by path_idx.
        03 path_dir pic x.
        03 path_x pic s9(4) comp.
        03 path_y pic s9(4) comp.

    77 total_found pic s9(8) comp.

procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
*>   call 'lib-readdata' using function module-id ".da1" rf_all_lines

  *> This happens to work with both the example and my data set, but may not work with all data!
  move length of function trim(rf_row(10) trailing) to rf_chars_in_row

  move 0 to total_found
  perform varying rf_idx from 1 by 1 until rf_idx > rf_cnt or rf_row(rf_idx) = spaces
    *> display "LINE: " function trim(rf_row(rf_idx))
    perform varying rf_char_idx from 1 by 1 until rf_char_idx > rf_chars_in_row
    *> *> perform varying rf_char_idx from 1 by 1 until rf_char_idx > length of function trim(rf_row(1))
    *>   if rf_char_idx > length of function trim(rf_row(rf_idx))
    *>     move space to map(rf_idx rf_char_idx)
    *>   end-if
      if curr_x = 0
        if rf_row(rf_idx)(rf_char_idx:1) = '.'
          move rf_idx to curr_y
          move rf_char_idx to curr_x
        end-if
      end-if
    end-perform
    continue
  end-perform
  compute rf_chars_in_col = rf_idx - 1

  add 1 to rf_idx
  move rf_row(rf_idx) to dir_str

  move 0 to curr_facing

  *> display "DIRS: " function trim(dir_str)
  *> display "START: " curr_x ", " curr_y " "

  move spaces to dir_buff
  perform varying dir_str_idx from 1 by 1 until dir_str_idx > length of function trim(dir_str)
    move 5 to test_facing
    evaluate dir_str(dir_str_idx:1)
      when "L"
        move curr_facing to test_facing
        subtract 1 from curr_facing
        compute curr_facing = function mod(curr_facing 4)
      when "R"
        move curr_facing to test_facing
        add 1 to curr_facing
        compute curr_facing = function mod(curr_facing 4)
      when other
        move dir_str(dir_str_idx:1) to dir_buff(length of function trim(dir_buff) + 1:)
        *> move dir_str(dir_str_idx:1) to dir_buff(1:)
    end-evaluate
    *> if curr_facing = 0 move 4 to curr_facing end-if
    *> display "STAT: " curr_facing " " test_facing " BUFF: " dir_buff

    *> If the direction buffer is spaces, then a rotation was given. So, we can execute the previous forward amount.
    if test_facing <> 5
      move dir_buff to dir_num
      *> display "MOVING: " dir_num " " test_facing " FROM (" curr_y " " curr_x "): " rf_row(curr_y)(curr_x:1)
      move 0 to dir_blocked
      perform varying move_idx from 1 by 1 until move_idx > dir_num or dir_blocked = 1
        move curr_x to test_x
        move curr_y to test_y
        evaluate test_facing
          when 0
            perform with test after until rf_row(test_y)(test_x:1) <> space
              compute test_x = function mod((test_x + 1) rf_chars_in_row)
              if test_x = 0 move rf_chars_in_row to test_x end-if
            end-perform
          when 1
            perform with test after until rf_row(test_y)(test_x:1) <> space
              compute test_y = function mod((test_y + 1) rf_chars_in_col)
              if test_y = 0 move rf_chars_in_col to test_y end-if
            end-perform
          when 2
            perform with test after until rf_row(test_y)(test_x:1) <> space
              compute test_x = function mod((test_x - 1) rf_chars_in_row)
              if test_x = 0 move rf_chars_in_row to test_x end-if
            end-perform
          when 3
            perform with test after until rf_row(test_y)(test_x:1) <> space
              compute test_y = function mod((test_y - 1) rf_chars_in_col)
              if test_y = 0 move rf_chars_in_col to test_y end-if
            end-perform
        end-evaluate
        *> display "CHAR (" test_y " " test_x "): " rf_row(test_y)(test_x:1)

        *> If it's a wall, stop moving.
        if rf_row(test_y)(test_x:1) = '#'
          *> display "WALL! "
          move 1 to dir_blocked
        else
          move test_x to curr_x
          move test_y to curr_y
        end-if

      end-perform
      move spaces to dir_buff
      move 0 to dir_num
    end-if
  end-perform

  display "END: " curr_x ", " curr_y

  compute total_found = (1000 * curr_y) + (4 * curr_x) + curr_facing

  display "FINAL: " total_found

  goback.
