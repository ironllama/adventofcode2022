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

    77 test_done pic 9.

    77 forest_width pic s9(8) comp.

    77 temp_limit pic s9(8) comp.
    77 view_dir pic s9.
    77 view_limit pic s9(8) comp.

    77 highest_so_far pic 9.
    77 curr_tree pic 9.
    *> 77 temp_limit pic s9(8) comp.
    77 temp_line pic x(9999).
    77 line_char_idx pic s9(8) comp.

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
*>   call 'lib-readfile' using function module-id rf_all_lines
  move "30373" to rf_line_row(1)
  move "25512" to rf_line_row(2)
  move "65332" to rf_line_row(3)
  move "33549" to rf_line_row(4)
  move "35390" to rf_line_row(5)
  move 5 to rf_line_cnt

  inspect function trim(rf_line_row(1)) tallying forest_width for all characters
  *> display "WIDTH: " forest_width

  move 0 to total_found

  move rf_all_lines to vis_all_lines

  *> From left
*>   perform varying rf_line_idx from 2 by 1 until rf_line_idx > (rf_line_cnt - 1)
*>     display "LINE: " function trim(rf_line_row(rf_line_idx))
*>     move rf_line_row(rf_line_idx)(1:1) to highest_so_far
*>     perform varying line_char_idx from 2 by 1 until line_char_idx > (forest_width - 1) or highest_so_far = 9
*>     *>   if rf_line_row(rf_line_idx)(line_char_idx:1) <> 'V'
*>     *>   if vis_line_row(rf_line_idx)(line_char_idx:1) <> 'V'
*>          move rf_line_row(rf_line_idx)(line_char_idx:1) to curr_tree
*>          if curr_tree > highest_so_far
*>            display "HIGHER: " curr_tree " > " highest_so_far
*>            move curr_tree to highest_so_far
*>            string vis_line_row(rf_line_idx)(1:line_char_idx - 1) delimited by size
*>                   "V"
*>                   vis_line_row(rf_line_idx)(line_char_idx + 1:forest_width) delimited by size
*>                   into temp_line
*>            end-string
*>            move temp_line to vis_line_row(rf_line_idx)
*>            display "NEW: [" rf_line_idx "]" temp_line
*>         *>  end-if
*>       end-if
*>     end-perform
*>   end-perform
*>   display "TEST 1"
*>   perform varying vis_line_idx from 1 by 1 until vis_line_idx > vis_line_cnt
*>     display vis_line_row(vis_line_idx)
*>   end-perform
  move 1 to view_dir
  perform test_view

  *> From right
*>   perform varying rf_line_idx from 2 by 1 until rf_line_idx > (rf_line_cnt - 1)
*>     *> display "LINE: " function trim(rf_line_row(rf_line_idx))
*>     move rf_line_row(rf_line_idx)(forest_width:1) to highest_so_far
*>     compute temp_limit = forest_width - 1
*>     perform varying line_char_idx from temp_limit by -1 until line_char_idx < 2 or highest_so_far = 9
*>     *>   if vis_line_row(rf_line_idx)(line_char_idx:1) <> 'V'
*>         move rf_line_row(rf_line_idx)(line_char_idx:1) to curr_tree
*>         if curr_tree > highest_so_far
*>         *>   display "HIGHER: " curr_tree " > " highest_so_far
*>           move curr_tree to highest_so_far
*>           string vis_line_row(rf_line_idx)(1:line_char_idx - 1) delimited by size
*>                  "V"
*>                  vis_line_row(rf_line_idx)(line_char_idx + 1:forest_width) delimited by size
*>                  into temp_line
*>           end-string
*>           move temp_line to vis_line_row(rf_line_idx)
*>         *>   display "NEW: [" rf_line_idx "]" temp_line
*>         *> end-if
*>       end-if
*>     end-perform
*>   end-perform

*>   compute temp_limit = forest_width - 1
*>   move 2 to view_limit
*>   display "TEST 2"
*>   perform varying vis_line_idx from 1 by 1 until vis_line_idx > vis_line_cnt
*>     display vis_line_row(vis_line_idx)
*>   end-perform
  move -1 to view_dir
  perform test_view

  *> ROTATE
*>   move forest_width to new_line_cnt
*>   compute temp_limit = forest_width
*>   perform varying rf_line_idx from 1 by 1 until rf_line_idx > rf_line_cnt
*>     set new_line_idx to 1
*>     perform varying line_char_idx from temp_limit by -1 until line_char_idx < 1
*>     *>   move rf_line_row(rf_line_idx)(line_char_idx:1) new_line_row(new_line_idx)
*>       string new_line_row(new_line_idx) delimited by spaces
*>           rf_line_row(rf_line_idx)(line_char_idx:1) delimited by space
*>         into new_temp_line
*>       end-string
*>       move new_temp_line to new_line_row(new_line_idx)
*>       add 1 to new_line_idx
*>     end-perform
*>   end-perform
  display "ROTATE 1"
  call 'lib-rotatetable' using rf_all_lines

*>   move forest_width to vis_temp_line_cnt
*> *>   compute temp_limit = forest_width
*>   perform varying vis_line_idx from 1 by 1 until vis_line_idx > vis_line_cnt
*>     set vis_temp_line_idx to 1
*>     perform varying line_char_idx from temp_limit by -1 until line_char_idx < 1
*>     *>   move vis_line_row(vis_line_idx)(line_char_idx:1) vis_temp_line_row(vis_temp_line_idx)
*>     *>   display "ADDING " vis_line_row(vis_line_idx)(line_char_idx:1) " TO: " vis_temp_line_row(vis_temp_line_idx)
*>       move spaces to new_temp_line
*>       string vis_temp_line_row(vis_temp_line_idx) delimited by spaces
*>           vis_line_row(vis_line_idx)(line_char_idx:1) delimited by space
*>         into new_temp_line
*>       end-string
*>       move new_temp_line to vis_temp_line_row(vis_temp_line_idx)
*>       add 1 to vis_temp_line_idx
*>     end-perform
*>   end-perform
  display "ROTATE 2"
  perform varying vis_line_idx from 1 by 1 until vis_line_idx > vis_line_cnt
    display vis_line_row(vis_line_idx)
  end-perform
  call 'lib-rotatetable' using vis_all_lines
*>   display "WHAT :"
*>   perform varying vis_temp_line_idx from 1 by 1 until vis_temp_line_idx > vis_temp_line_cnt
*>     display vis_temp_line_row(vis_temp_line_idx)
*>   end-perform

*>   display "ORIG:"
*>   perform varying rf_line_idx from 1 by 1 until rf_line_idx > rf_line_cnt
*>     display rf_line_row(rf_line_idx)
*>   end-perform
*>   move new_all_lines to rf_all_lines
*>   display "NEW:"
*>   perform varying rf_line_idx from 1 by 1 until rf_line_idx > rf_line_cnt
*>     display rf_line_row(rf_line_idx)
*>   end-perform
*>   display "ORIG:"
*>   perform varying vis_line_idx from 1 by 1 until vis_line_idx > vis_line_cnt
*>     display vis_line_row(vis_line_idx)
*>   end-perform
*>   move vis_temp_all_lines to vis_all_lines
*>   display "NEW:"
*>   perform varying vis_line_idx from 1 by 1 until vis_line_idx > vis_line_cnt
*>     display vis_line_row(vis_line_idx)
*>   end-perform

  *> From left
*>   perform varying rf_line_idx from 2 by 1 until rf_line_idx > (rf_line_cnt - 1)
*>     display "LINE: " function trim(rf_line_row(rf_line_idx))
*>     move rf_line_row(rf_line_idx)(1:1) to highest_so_far
*>     perform varying line_char_idx from 2 by 1 until line_char_idx > (forest_width - 1) or highest_so_far = 9
*>     *>   if vis_line_row(rf_line_idx)(line_char_idx:1) <> 'V'
*>          move rf_line_row(rf_line_idx)(line_char_idx:1) to curr_tree
*>          if curr_tree > highest_so_far
*>            display "HIGHER: " curr_tree " > " highest_so_far
*>            move curr_tree to highest_so_far
*>            string vis_line_row(rf_line_idx)(1:line_char_idx - 1) delimited by size
*>                   "V"
*>                   vis_line_row(rf_line_idx)(line_char_idx + 1:forest_width) delimited by size
*>                   into temp_line
*>            end-string
*>            move temp_line to vis_line_row(rf_line_idx)
*>            display "NEW: [" rf_line_idx "]" temp_line
*>          end-if
*>     *>   end-if
*>     end-perform
*>   end-perform

   *> From right
*>   perform varying rf_line_idx from 2 by 1 until rf_line_idx > (rf_line_cnt - 1)
*>     display "LINE: " function trim(rf_line_row(rf_line_idx))
*>     move rf_line_row(rf_line_idx)(forest_width:1) to highest_so_far
*>     compute temp_limit = forest_width - 1
*>     perform varying line_char_idx from temp_limit by -1 until line_char_idx < 2 or highest_so_far = 9
*>     *>   if vis_line_row(rf_line_idx)(line_char_idx:1) <> 'V'
*>         move rf_line_row(rf_line_idx)(line_char_idx:1) to curr_tree
*>         if curr_tree > highest_so_far
*>           display "HIGHER: " curr_tree " > " highest_so_far
*>           move curr_tree to highest_so_far
*>           string vis_line_row(rf_line_idx)(1:line_char_idx - 1) delimited by size
*>                  "V"
*>                  vis_line_row(rf_line_idx)(line_char_idx + 1:forest_width) delimited by size
*>                  into temp_line
*>           end-string
*>           move temp_line to vis_line_row(rf_line_idx)
*>           display "NEW: [" rf_line_idx "]" temp_line
*>         end-if
*>     *>   end-if
*>     end-perform
*>   end-perform
  display "TEST 3 " vis_line_cnt
  perform varying vis_line_idx from 1 by 1 until vis_line_idx > vis_line_cnt
    display vis_line_row(vis_line_idx)
  end-perform
  move 1 to view_dir
  perform test_view

  display "TEST 4"
  perform varying vis_line_idx from 1 by 1 until vis_line_idx > vis_line_cnt
    display vis_line_row(vis_line_idx)
  end-perform
  move -1 to view_dir
  perform test_view

  display "TESTING DONE"
  perform varying vis_line_idx from 1 by 1 until vis_line_idx > vis_line_cnt
    display vis_line_row(vis_line_idx)
  end-perform

  display "COUNTING"

  perform varying vis_line_idx from 2 by 1 until vis_line_idx > (vis_line_cnt - 1)
    display vis_line_row(vis_line_idx)
    perform varying line_char_idx from 2 by 1 until line_char_idx > (forest_width - 1)
    *>   display "TESTING: " vis_line_row(vis_line_idx)(line_char_idx:1)
      if vis_line_row(vis_line_idx)(line_char_idx:1) = 'V'
        add 1 to total_found
      end-if
    end-perform
  end-perform

  compute edges_total = (2 * rf_line_cnt) + (2 * (rf_line_cnt - 2))
  display "total_found: " total_found " + edges: " edges_total
  compute total_found = total_found + edges_total

  display "FINAL: " total_found

  goback.

test_view.
  perform varying rf_line_idx from 2 by 1 until rf_line_idx > (rf_line_cnt - 1)
    display "LINE: " function trim(rf_line_row(rf_line_idx))
    if view_dir = 1
      move rf_line_row(rf_line_idx)(1:1) to highest_so_far
      move 2 to temp_limit
    else
      move rf_line_row(rf_line_idx)(forest_width:1) to highest_so_far
      compute temp_limit = forest_width - 1
    end-if
    *> compute temp_limit = forest_width - 1
    *> perform varying line_char_idx from temp_limit by -1 until line_char_idx < 2 or highest_so_far = 9
    move 0 to test_done
    perform varying line_char_idx from temp_limit by view_dir until test_done = 1 or highest_so_far = 9
      if (view_dir = 1 and line_char_idx > (forest_width - 1))
          or (view_dir = -1 and line_char_idx < 2)
        move 1 to test_done
      else
        if rf_line_row(rf_line_idx)(line_char_idx:1) <> 'V'
          move rf_line_row(rf_line_idx)(line_char_idx:1) to curr_tree
          if curr_tree > highest_so_far
            display "HIGHER: " curr_tree " > " highest_so_far
            move curr_tree to highest_so_far
            move spaces to temp_line
            string vis_line_row(rf_line_idx)(1:line_char_idx - 1) delimited by size
                   "V"
                   vis_line_row(rf_line_idx)(line_char_idx + 1:forest_width) delimited by size
                   into temp_line
            end-string
            display "MOVING: " function trim(temp_line) " TO " function trim(vis_line_row(rf_line_idx))
            move temp_line to vis_line_row(rf_line_idx)
            display "NEW: [" rf_line_idx "]" function trim(temp_line)
          end-if
        end-if
      end-if
    end-perform
  end-perform
  .
