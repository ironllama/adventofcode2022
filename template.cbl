           >>source format free
identification division.
program-id. template.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_line_cnt pic s9(8) comp value 0.
      02 rf_line_row pic x(99) occurs 0 to 9999 times
          depending on rf_line_cnt indexed by rf_line_idx.

    77 total_found pic s9(8) comp.

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

  end-perform

  display "FINAL: " total_found

  goback.
