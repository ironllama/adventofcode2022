           >>source format free
identification division.
program-id. lib-rotatetable.

data division.
  working-storage section.
    01 new_all_lines.
      02 new_line_cnt pic s9(8) comp value 0.
      02 new_line_row pic x(9999) occurs 0 to 9999 times
          depending on new_line_cnt indexed by new_line_idx.

    77 line_width pic s9(8) comp.
    77 line_char_idx pic s9(8) comp.
    77 new_temp_line pic x(9999).

  linkage section.
    01 ln-all_lines.
      02 ln-line_cnt pic s9(8) comp value 0.
      02 ln-line_row pic x(9999) occurs 0 to 9999 times
          depending on ln-line_cnt indexed by ln-line_idx.

procedure division using ln-all_lines.
  *> Rotates table counter-clockwise. Assumes rectangle table.
  display "ROTATE TABLE"

*>   display "ORIG: [" ln-line_cnt "]"
*>   perform varying ln-line_idx from 1 by 1 until ln-line_idx > ln-line_cnt
*>     display function trim(ln-line_row(ln-line_idx))
*>   end-perform

  inspect function trim(ln-line_row(1)) tallying line_width for all characters

  move ln-line_cnt to new_line_cnt
  set ln-line_idx to 1
  perform varying ln-line_idx from 1 by 1 until ln-line_idx > ln-line_cnt
    set new_line_idx to 1
    set line_char_idx to 1
    perform varying line_char_idx from line_width by -1 until line_char_idx < 1
    *>   move ln-line_row(ln-line_idx)(line_char_idx:1) new_line_row(new_line_idx)
      display "INNER: [" ln-line_idx
      move spaces to new_temp_line
      string new_line_row(new_line_idx) delimited by spaces
          ln-line_row(ln-line_idx)(line_char_idx:1) delimited by space
        into new_temp_line
      end-string
      display "STRING: [" new_line_idx "]: " function trim(new_temp_line) " + " ln-line_row(ln-line_idx)(line_char_idx:1)

      move new_temp_line to new_line_row(new_line_idx)
      add 1 to new_line_idx
    end-perform
  end-perform

  move new_all_lines to ln-all_lines

  display "NEW: [" new_line_cnt "]"
  perform varying ln-line_idx from 1 by 1 until ln-line_idx > ln-line_cnt
    display function trim(ln-line_row(ln-line_idx))
  end-perform

  goback.
