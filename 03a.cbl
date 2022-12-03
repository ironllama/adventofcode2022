           >>source format free
identification division.
program-id. 03a.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_line_cnt pic s9(8) comp value 0.
      02 rf_line_row pic x(99) occurs 0 to 9999 times
          depending on rf_line_cnt indexed by rf_line_idx.

    77 num_chars pic 9(4) comp.

    77 left_string pic x(50).
    77 right_string pic x(50).
    77 left_idx pic 9(4) comp.
    77 right_idx pic 9(4) comp.
    77 left_char pic x.
    77 match_found pic 9 comp.

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
  display "[" all_chars "]"
  move 0 to final_points

  perform varying rf_line_idx from 1 by 1 until rf_line_idx > rf_line_cnt
    compute num_chars = function length(function trim(rf_line_row(rf_line_idx)))
    *> display "LINE: " num_chars " " function trim(rf_line_row(rf_line_idx))

    compute num_chars = num_chars / 2
    move rf_line_row(rf_line_idx)(1:num_chars) to left_string
    move rf_line_row(rf_line_idx)(num_chars + 1:) to right_string
    display left_string " " right_string

    move 0 to match_found
    perform varying left_idx from 1 by 1 until left_idx > num_chars or match_found = 1
      move left_string(left_idx:1) to left_char
      perform varying right_idx from 1 by 1 until right_idx > num_chars or match_found = 1
        if left_char = right_string(right_idx:1)
          *> display "MATCH: " left_char no advancing 
          perform varying all_chars_idx from 1 by 1 until all_chars_idx > 52
            if all_chars(all_chars_idx:1) = left_char
              *> display "POINTS: " all_chars_idx
              compute final_points = final_points + all_chars_idx
            end-if
          end-perform
          move 1 to match_found
        end-if
      end-perform
    end-perform  
  end-perform

  display "FINAL: " final_points

  goback.
