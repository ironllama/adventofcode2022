           >>source format free
identification division.
program-id. 06a.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_line_cnt pic s9(8) comp value 0.
      02 rf_line_row pic x(9999) occurs 0 to 9999 times
          depending on rf_line_cnt indexed by rf_line_idx.

  77 new_char pic x.
  77 temp_chars pic x(14).
  77 window_chars pic x(14).
  77 window_size pic 9(2) comp.

  77 dup_out_idx pic 9(4).
  77 dup_out_idx_offset pic 9(4).
  77 dup_in_idx pic 9(4).

  77 dup_found pic 9(4) comp.
  77 pos_idx pic s9(8) comp.

procedure division.
  call 'lib-readfile' using function module-id rf_all_lines
*>   move "mjqjpqmgbljsphdztnvjfqwrcgsmlb" to rf_line_row(1)
*>   move "bvwbjplbgvbhsrlpgdmjqwftvncz" to rf_line_row(1)
*>   move "nppdvjthqldpwncqszvftbrmjlhg" to rf_line_row(1)
*>   move "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" to rf_line_row(1)
*>   move "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" to rf_line_row(1)
*>   move 1 to rf_line_cnt

  move spaces to window_chars
  move 4 to window_size

  *> display "LINE: " function trim(rf_line_row(1)) " LENGTH: " length of function trim(rf_line_row(1))
  perform varying pos_idx from 1 by 1 until
      pos_idx > length of function trim(rf_line_row(rf_line_idx))
      or (length of function trim(window_chars) = window_size and dup_found = 0)

    move rf_line_row(1)(pos_idx:1) to new_char
    string window_chars(2:window_size - 1) new_char
      into temp_chars
    end-string
    move temp_chars to window_chars
    *> display "BUFFER: " window_chars

    move 0 to dup_found
    if window_chars(1:1) <> " "
      perform varying dup_out_idx from 1 by 1 until dup_out_idx > window_size or dup_found = 1
        compute dup_out_idx_offset = dup_out_idx + 1
        perform varying dup_in_idx from dup_out_idx_offset by 1 until dup_in_idx > window_size or dup_found = 1
          *> display "COMPARING: " window_chars(dup_out_idx:1) " = " window_chars(dup_in_idx:1)
          if window_chars(dup_out_idx:1) = window_chars(dup_in_idx:1)
            *> display "DUPE!"
            move 1 to dup_found
          end-if
        end-perform
      end-perform
    end-if
  end-perform

  subtract 1 from pos_idx
  display "FINAL: " pos_idx

  goback.
