           >>source format free
identification division.
program-id. 22b.

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

    01 dir_str pic x(5650).
    01 dir_str_idx usage is index.
    01 dir_buff pic x(4).
    01 dir_num pic 9(4) comp.
    01 dir_blocked pic 9.
    01 move_idx usage is index.

    01 test_x pic s9(3) comp.
    01 test_y pic s9(3) comp.
    01 test_facing pic 9.
    01 test_curr_facing pic 9.
    01 curr_x pic s9(3) comp value 0.
    01 curr_y pic s9(3) comp value 0.
    01 curr_facing pic s9.

    77 total_found pic s9(8) comp.

procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
*>   call 'lib-readdata' using function module-id ".da1" rf_all_lines

  *> This happens to work with both the example and my data set, but may not work with all data!
  move length of function trim(rf_row(10) trailing) to rf_chars_in_row

  *> Scan the input for the starting position, and the line separating map from directions.
  perform varying rf_idx from 1 by 1 until rf_idx > rf_cnt or rf_row(rf_idx) = spaces
    *> display "LINE: " function trim(rf_row(rf_idx))
    perform varying rf_char_idx from 1 by 1 until rf_char_idx > rf_chars_in_row or curr_x > 0
      if curr_x = 0
        if rf_row(rf_idx)(rf_char_idx:1) = '.'
          move rf_idx to curr_y
          move rf_char_idx to curr_x
        end-if
      end-if
    end-perform
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
        move dir_str(dir_str_idx:1) to dir_buff(length of function trim(dir_buff) + 1:)  *> So ugly... Whatevs.
    end-evaluate
    move curr_facing to test_curr_facing
    *> display "STAT: " curr_facing " " test_facing " BUFF: " dir_buff

    *> Make sure you get last moves, where there is no subsequent dir to trigger move.
    if dir_str_idx = length of function trim(dir_str)
      move curr_facing to test_facing
    end-if

    *> If the direction buffer is spaces, then a rotation was given. So, we can execute the previous forward amount.
    if test_facing <> 5
      move dir_buff to dir_num
      *> display "MOVING: " dir_num " " test_facing " FROM (" curr_y " " curr_x "): " rf_row(curr_y)(curr_x:1)
      move 0 to dir_blocked
      perform varying move_idx from 1 by 1 until move_idx > dir_num or dir_blocked = 1
        move curr_x to test_x
        move curr_y to test_y

        *> display "START: test_facing: " test_facing " Y: " test_y " X: " test_x " char: " rf_row(test_y)(test_x:1)
        evaluate test_facing
          when 0 add 1 to test_x
          when 1 add 1 to test_y
          when 2 subtract 1 from test_x
          when 3 subtract 1 from test_y
        end-evaluate
        *> display "CHAR (" test_y " " test_x "): " rf_row(test_y)(test_x:1)

        *> display "MID: test_facing: " test_facing " Y: " test_y " X: " test_x " char: " rf_row(test_y)(test_x:1)

        *> If moved out of range, teleport to proper place within range.
        *> Mind the order of assignments on these, since the order will change the values.
        evaluate test_facing
          when 0
            *> 2 to 4
            if test_x = 151 and test_y >= 1 and test_y <= 50
              compute test_y = 151 - test_y
              compute test_x = 100
              compute test_facing = function mod((test_facing + 2) 4)
              compute test_curr_facing = function mod((curr_facing + 2) 4)
              *> display "WARP: 2 to 4"
            end-if
            *> 3 to 2
            if test_x = 101 and test_y >= 51 and test_y <= 100
              compute test_x = test_y + 50
              compute test_y = 50
              compute test_facing = function mod((test_facing - 1) 4)
              compute test_curr_facing = function mod((curr_facing - 1) 4)
              *> display "WARP: 3 to 2"
            end-if
            *> 4 to 2
            if test_x = 101 and test_y >= 101 and test_y <= 150
              compute test_y = 151 - test_y
              compute test_x = 150
              compute test_facing = function mod((test_facing - 2) 4)
              compute test_curr_facing = function mod((curr_facing - 2) 4)
              *> display "WARP: 4 to 2"
            end-if
            *> 6 to 4
            if test_x = 51 and test_y >= 151 and test_y <= 200
              compute test_x = test_y - 100
              compute test_y = 150
              compute test_facing = function mod((test_facing - 1) 4)
              compute test_curr_facing = function mod((curr_facing - 1) 4)
              *> display "WARP: 6 to 4"
            end-if
          when 1
            *> 2 to 3
            if test_y = 51 and test_x >= 101 and test_x <= 150
              compute test_y = test_x - 50
              compute test_x = 100
              compute test_facing = function mod((test_facing + 1) 4)
              compute test_curr_facing = function mod((curr_facing + 1) 4)
              *> display "WARP: 2 to 3"
            end-if
            *> 4 to 6
            if test_y = 151 and test_x >= 51 and test_x <= 100
              compute test_y = test_x + 100
              compute test_x = 50
              compute test_facing = function mod((test_facing + 1) 4)
              compute test_curr_facing = function mod((curr_facing + 1) 4)
              *> display "WARP: 4 to 6"
            end-if
            *> 6 to 2
            if test_y = 201 and test_x >= 1 and test_x <= 50
              compute test_y = 1
              compute test_x = test_x + 100
              *> display "WARP: 6 to 2"
            end-if
          when 2
            *> 1 to 5
            if test_x = 50 and test_y >= 1 and test_y <= 50
              compute test_y = 151 - test_y
              compute test_x = 1
              compute test_facing = function mod((test_facing + 2) 4)
              compute test_curr_facing = function mod((curr_facing + 2) 4)
              *> display "WARP: 1 to 5"
            end-if
             *> 3 to 5
            if test_x = 50 and test_y >= 51 and test_y <= 100
              compute test_x = test_y - 50
              compute test_y = 101
              compute test_facing = function mod((test_facing - 1) 4)
              compute test_curr_facing = function mod((curr_facing - 1) 4)
              *> display "WARP: 3 to 5"
            end-if
             *> 5 to 1
            if test_x = 0 and test_y >= 101 and test_y <= 150
              compute test_y = 151 - test_y
              compute test_x = 51
              compute test_facing = function mod((test_facing - 2) 4)
              compute test_curr_facing = function mod((curr_facing - 2) 4)
              *> display "WARP: 5 to 1"
            end-if
            *> 6 to 1
            if test_x = 0 and test_y >= 151 and test_y <= 200
              compute test_x = test_y - 100
              compute test_y = 1
              compute test_facing = function mod((test_facing - 1) 4)
              compute test_curr_facing = function mod((curr_facing - 1) 4)
              *> display "WARP: 6 to 1"
            end-if
          when 3
            *> 1 to 6
            if test_y = 0 and test_x >= 51 and test_x <= 100
              compute test_y = test_x + 100
              compute test_x = 1
              compute test_facing = function mod((test_facing + 1) 4)
              compute test_curr_facing = function mod((curr_facing + 1) 4)
              *> display "WARP: 1 to 6"
            end-if
            *> 2 to 6
            if test_y = 0 and test_x >= 101 and test_x <= 150
              compute test_y = 200
              compute test_x = test_x - 100
              *> display "WARP: 2 to 6"
            end-if
            *> 5 to 3
            if test_y = 100 and test_x >= 1 and test_x <= 50
              compute test_y = test_x + 50
              compute test_x = 51
              compute test_facing = function mod((test_facing + 1) 4)
              compute test_curr_facing = function mod((curr_facing + 1) 4)
              *> display "WARP: 5 to 3"
            end-if
        end-evaluate
        *> display "END: test_facing: " test_facing " Y: " test_y " X: " test_x " char: " rf_row(test_y)(test_x:1)

        *> If it's a wall, stop moving.
        if rf_row(test_y)(test_x:1) = '#'
          *> display "WALL! "
          move 1 to dir_blocked
        else
          move test_x to curr_x
          move test_y to curr_y
          move test_curr_facing to curr_facing
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
