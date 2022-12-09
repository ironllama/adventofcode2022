           >>source format free
identification division.
program-id. 09a.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_line_cnt pic s9(8) comp value 0.
      02 rf_line_row pic x(9999) occurs 0 to 9999 times
          depending on rf_line_cnt indexed by rf_line_idx.

    77 dir pic x.
    77 amt pic 9(2) comp.
    77 follow_idx pic 9(8) comp.

    01 head.
      02 head_x pic s9(8) comp.
      02 head_y pic s9(8) comp.
    01 tail.
      02 tail_x pic s9(8) comp.
      02 tail_y pic s9(8) comp.

    77 tail_visit_found pic 9.
    01 tail_visited.
      02 tail_visited_cnt pic s9(8) comp.
      02 tail_visited_pos occurs 0 to 9999 times
          depending on tail_visited_cnt
          indexed by tail_visited_idx.
        03 tail_visited_x pic s9(8) comp.
        03 tail_visited_y pic s9(8) comp.


    77 total_found pic s9(8) comp.

procedure division.
  call 'lib-readfile' using function module-id rf_all_lines
*>   move "R 4" to rf_line_row(1)
*>   move "U 4" to rf_line_row(2)
*>   move "L 3" to rf_line_row(3)
*>   move "D 1" to rf_line_row(4)
*>   move "R 4" to rf_line_row(5)
*>   move "D 1" to rf_line_row(6)
*>   move "L 5" to rf_line_row(7)
*>   move "R 2" to rf_line_row(8)
*>   move 8 to rf_line_cnt


  move 0 to head_x
  move 0 to head_y
  move 0 to tail_x
  move 0 to tail_y

  move 0 to tail_visited_cnt

  perform varying rf_line_idx from 1 by 1 until rf_line_idx > rf_line_cnt
    *> display "LINE: " function trim(rf_line_row(rf_line_idx))
    unstring rf_line_row(rf_line_idx) delimited by spaces
      into dir amt
    end-unstring
    *> display "DIR: " dir " AMT: " amt

    perform varying follow_idx from 1 by 1 until follow_idx > amt
      if dir = "U" add 1 to head_y end-if
      if dir = "D" subtract 1 from head_y end-if
      if dir = "R" add 1 to head_x end-if
      if dir = "L" subtract 1 from head_x end-if
      *> display "HEAD: " head_x "," head_y " TAIL: " tail_x "," tail_y

      if head_x > (tail_x + 1)
        add 1 to tail_x
        if head_y > tail_y add 1 to tail_y end-if
        if head_y < tail_y subtract 1 from tail_y end-if
        perform add_visited
      end-if
      if head_x < (tail_x - 1)
        subtract 1 from tail_x
        if head_y > tail_y add 1 to tail_y end-if
        if head_y < tail_y subtract 1 from tail_y end-if
        perform add_visited
      end-if
      if head_y > (tail_y + 1)
        add 1 to tail_y
        if head_x > tail_x add 1 to tail_x end-if
        if head_x < tail_x subtract 1 from tail_x end-if
        perform add_visited
      end-if
      if head_y < (tail_y - 1)
        subtract 1 from tail_y
        if head_x > tail_x add 1 to tail_x end-if
        if head_x < tail_x subtract 1 from tail_x end-if
        perform add_visited
      end-if

    end-perform
  end-perform

  compute total_found = tail_visited_cnt + 1
  display "FINAL: " total_found

  goback.

add_visited.
  move 0 to tail_visit_found
  set tail_visited_idx to 1
  perform varying tail_visited_idx from 1 by 1 until tail_visited_idx > tail_visited_cnt or tail_visit_found = 1
    if tail_visited_x(tail_visited_idx) = tail_x and tail_visited_y(tail_visited_idx) = tail_y
      *> display "DUPE!"
      move 1 to tail_visit_found
    end-if
  end-perform

  if tail_visit_found = 0
    add 1 to tail_visited_cnt
    move tail_x to tail_visited_x(tail_visited_cnt)
    move tail_y to tail_visited_y(tail_visited_cnt)
    *> display "ADDED: " tail_x "," tail_y
  end-if
  .
