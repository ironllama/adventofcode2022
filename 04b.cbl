           >>source format free
identification division.
program-id. 04b.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_line_cnt pic s9(8) comp value 0.
      02 rf_line_row pic x(99) occurs 0 to 9999 times
          depending on rf_line_cnt indexed by rf_line_idx.

    77 left_string pic x(15).
    77 left_from pic 9(2) comp.
    77 left_to pic 9(2) comp.
    77 left_idx pic 9(4) comp.
    77 right_string pic x(15).
    77 right_from pic 9(2) comp.
    77 right_to pic 9(2) comp.
    77 right_idx pic 9(4) comp.

    77 match_found pic 9 comp.
    77 total_found pic 9(8) comp.

procedure division.
  call 'lib-readfile' using function module-id rf_all_lines
  *> move "1-4,6-8" to rf_line_row(1)
  *> move "1-3,4-5" to rf_line_row(2)
  *> move "4-7,7-9" to rf_line_row(3)
  *> move "1-8,3-7" to rf_line_row(4)
  *> move "5-6,4-6" to rf_line_row(5)
  *> move "1-6,4-8" to rf_line_row(6)
  *> move 6 to rf_line_cnt

  move 0 to total_found
  perform varying rf_line_idx from 1 by 1 until rf_line_idx > rf_line_cnt
    *> display "LINE: " function trim(rf_line_row(rf_line_idx))
    unstring function trim(rf_line_row(rf_line_idx)) delimited by ","
      into left_string right_string
    end-unstring

    unstring function trim(left_string) delimited by "-"
      into left_from left_to
    end-unstring
    unstring function trim(right_string) delimited by "-"
      into right_from right_to
    end-unstring

    move 0 to match_found
    perform varying left_idx from left_from by 1 until left_idx > left_to or match_found = 1
      if (left_idx >= right_from and left_idx <= right_to)
        move 1 to match_found
      end-if
    end-perform
    perform varying right_idx from right_from by 1 until right_idx > right_to or match_found = 1
      if (right_idx >= left_from and right_idx <= left_to)
        move 1 to match_found
      end-if
    end-perform

    if match_found = 1
      add 1 to total_found
    end-if
  end-perform

  display "FINAL: " total_found

  goback.
