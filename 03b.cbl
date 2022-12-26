           >>source format free
identification division.
program-id. 03b.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_line_cnt pic s9(8) comp value 0.
      02 rf_line_row pic x(99) occurs 0 to 9999 times
          depending on rf_line_cnt indexed by rf_line_idx.

    77 num_chars pic 9(4) comp.
    77 match_line_idx pic 9(4) comp.
    77 match_char pic x.
    77 match_found pic 9 comp.
    77 total_found pic 9 comp.

    77 group_idx pic 9 comp.
    77 group_num_chars pic 9(4) comp.
    77 group_line_idx pic 9(4) comp.

    77 all_chars pic x(52).
    77 all_chars_idx pic 9(2) comp.
    77 final_points pic 9(4) comp.

procedure division.
  call 'lib-readfile' using function module-id rf_all_lines
  *> move "vJrwpWtwJgWrhcsFMMfFFhFp" to rf_line_row(1)
  *> move "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL" to rf_line_row(2)
  *> move "PmmdzqPrVvPwwTWBwg" to rf_line_row(3)
  *> move "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn" to rf_line_row(4)
  *> move "ttgJtRGJQctTZtZT" to rf_line_row(5)
  *> move "CrZsJsPPZsGzwwsLwLmpwMDw" to rf_line_row(6)
  *> move 6 to rf_line_cnt

  move "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" to all_chars
  *> display "[" all_chars "]"
  move 0 to final_points

  perform varying rf_line_idx from 1 by 3 until rf_line_idx > rf_line_cnt
    compute num_chars = function length(function trim(rf_line_row(rf_line_idx)))
    *> display "LINE: " num_chars " " function trim(rf_line_row(rf_line_idx)) " " num_chars
    move 0 to total_found
    perform varying match_line_idx from 1 by 1 until match_line_idx > num_chars or total_found = 2
      move rf_line_row(rf_line_idx)(match_line_idx:1) to match_char
      *> display "CHAR: " match_char
      move 0 to total_found
      perform varying group_idx from 1 by 1 until group_idx > 2
        compute group_num_chars = function length(function trim(rf_line_row(rf_line_idx + group_idx)))
        *> display "TEST: " group_num_chars " " rf_line_row(rf_line_idx + group_idx)
        move 0 to match_found
        perform varying group_line_idx from 1 by 1 until group_line_idx > group_num_chars or match_found = 1
        *> display "COMPARING: " rf_line_row(rf_line_idx + group_idx)(group_line_idx:1) " " match_char
          if rf_line_row(rf_line_idx + group_idx)(group_line_idx:1) = match_char
            move 1 to match_found
            compute total_found = total_found + 1
            *> display "FOUND: [" group_idx ":" group_line_idx "] " match_char " total_found:" total_found
          end-if
        end-perform
      end-perform

      if total_found = 2
        perform varying all_chars_idx from 1 by 1 until all_chars_idx > 52
          if all_chars(all_chars_idx:1) = match_char
            *> display "MATCH: " match_char " POINTS: " all_chars_idx
            compute final_points = final_points + all_chars_idx
          end-if
        end-perform
      end-if
    end-perform
  end-perform

  display "FINAL: " final_points

  goback.
