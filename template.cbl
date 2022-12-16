           >>source format free
identification division.
program-id. template.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_cnt pic s9(8) comp value 0.
      02 rf_row pic x(9999) occurs 0 to 9999 times
          depending on rf_cnt indexed by rf_idx.

    77 total_found pic s9(8) comp.

procedure division.
  *> call 'lib-readdata' using function module-id ".dat" rf_all_lines
  call 'lib-readdata' using function module-id ".da1" rf_all_lines

  move 0 to total_found
  perform varying rf_idx from 1 by 1 until rf_idx > rf_cnt
    *> display "LINE: " function trim(rf_row(rf_idx))

  end-perform

  display "FINAL: " total_found

  goback.
