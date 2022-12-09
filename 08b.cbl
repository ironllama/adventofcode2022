           >>source format free
identification division.
program-id. 08a.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_line_cnt pic s9(8) comp value 0.
      02 rf_line_row pic x(9999) occurs 0 to 9999 times
          depending on rf_line_cnt indexed by rf_line_idx.

    77 forest_width pic s9(8) comp.
    *> 77 temp_limit pic s9(8) comp.
    *> 77 view_dir pic s9.
    *> 77 view_limit pic s9(8) comp.

    77 line_char_idx pic s9(8) comp.
    77 check_iter pic s9(8) comp.
    77 temp_idx pic s9(8) comp.
    77 temp_total pic s9(8) comp.
    77 temp_done pic 9.
    77 curr_tree pic 9.
    77 test_tree pic 9.
    77 test_total pic s9(8) comp.



    77 highest_so_far pic 9.
    77 temp_limit pic s9(8) comp.
    77 temp_line pic x(99).

    01 vis_all_lines.
      02 vis_line_cnt pic s9(8) comp value 0.
      02 vis_line_row pic x(9999) occurs 0 to 9999 times
          depending on vis_line_cnt indexed by vis_line_idx.

    *> 77 new_line_idx pic s9(8) comp.
    77 new_temp_line pic x(99).
    01 new_all_lines.
      02 new_line_cnt pic s9(8) comp value 0.
      02 new_line_row pic x(9999) occurs 0 to 9999 times
          depending on new_line_cnt indexed by new_line_idx.

    01 vis_temp_all_lines.
      02 vis_temp_line_cnt pic s9(8) comp value 0.
      02 vis_temp_line_row pic x(9999) occurs 0 to 9999 times
          depending on vis_temp_line_cnt indexed by vis_temp_line_idx.

    77 edges_total pic s9(8) comp.
    77 total_found pic s9(8) comp.

procedure division.
  call 'lib-readfile' using function module-id rf_all_lines
*>   move "30373" to rf_line_row(1)
*>   move "25512" to rf_line_row(2)
*>   move "65332" to rf_line_row(3)
*>   move "33549" to rf_line_row(4)
*>   move "35390" to rf_line_row(5)
*>   move 5 to rf_line_cnt

  inspect function trim(rf_line_row(1)) tallying forest_width for all characters
  move 0 to total_found

  *> From left
  perform varying rf_line_idx from 2 by 1 until rf_line_idx > (rf_line_cnt - 1)
    display "LINE: " function trim(rf_line_row(rf_line_idx))
    perform varying line_char_idx from 2 by 1 until line_char_idx > (forest_width - 1)
      move rf_line_row(rf_line_idx)(line_char_idx:1) to curr_tree
      move 1 to test_total
      display "CHAR: " curr_tree

      *> Check up.
      move 0 to temp_total
      move 0 to temp_done
      perform varying check_iter from 1 by 1 until temp_done = 1
        compute temp_idx = rf_line_idx - check_iter
        if temp_idx < 1
          move 1 to temp_done
        else
          move rf_line_row(temp_idx)(line_char_idx:1) to test_tree
          display "UP TEST: " curr_tree test_tree temp_idx
        *>   if curr_tree > test_tree
        *>     add 1 to temp_total
        *>   else
        *>     move 1 to temp_done
        *>   end-if
          add 1 to temp_total
          if curr_tree <= test_tree
            move 1 to temp_done
          end-if
        end-if
      end-perform
      compute test_total = test_total * temp_total
      display "UP: " temp_total " TOTAL: " test_total

      *> Check down.
      move 0 to temp_total
      move 0 to temp_done
      perform varying check_iter from 1 by 1 until temp_done = 1
        compute temp_idx = rf_line_idx + check_iter
        if temp_idx > rf_line_cnt
          move 1 to temp_done
        else
          move rf_line_row(temp_idx)(line_char_idx:1) to test_tree
        *>   if curr_tree > test_tree
        *>     add 1 to temp_total
        *>   else
        *>     move 1 to temp_done
        *>   end-if
          add 1 to temp_total
          if curr_tree <= test_tree
            move 1 to temp_done
          end-if
        end-if
      end-perform
      compute test_total = test_total * temp_total
      display "DN: " temp_total " TOTAL: " test_total

      *> Check left.
      move 0 to temp_total
      move 0 to temp_done
      perform varying check_iter from 1 by 1 until temp_done = 1
        compute temp_idx = line_char_idx - check_iter
        if temp_idx < 1
          move 1 to temp_done
        else
          move rf_line_row(rf_line_idx)(temp_idx:1) to test_tree
        *>   if curr_tree > test_tree
        *>     add 1 to temp_total
        *>   else
        *>     move 1 to temp_done
        *>   end-if
          add 1 to temp_total
          if curr_tree <= test_tree
            move 1 to temp_done
          end-if
        end-if
      end-perform
      compute test_total = test_total * temp_total
      display "LF: " temp_total " TOTAL: " test_total

      *> Check right.
      move 0 to temp_total
      move 0 to temp_done
      perform varying check_iter from 1 by 1 until temp_done = 1
        compute temp_idx = line_char_idx + check_iter
        if temp_idx > rf_line_cnt
          move 1 to temp_done
        else
          move rf_line_row(rf_line_idx)(temp_idx:1) to test_tree
        *>   if curr_tree > test_tree
        *>     add 1 to temp_total
        *>   else
        *>     move 1 to temp_done
        *>   end-if
          add 1 to temp_total
          if curr_tree <= test_tree
            move 1 to temp_done
          end-if
        end-if
      end-perform
      compute test_total = test_total * temp_total
      display "RT: " temp_total " TOTAL: " test_total

      if test_total > total_found
        move test_total to total_found
      end-if

    end-perform
  end-perform

  display "FINAL: " total_found

  goback.
