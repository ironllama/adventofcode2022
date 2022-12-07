           >>source format free
identification division.
program-id. 07a.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_line_cnt pic s9(8) comp value 0.
      02 rf_line_row pic x(99) occurs 0 to 9999 times
          depending on rf_line_cnt indexed by rf_line_idx.

    77 line_word_one pic x(10).
    *> 77 line_num_one redefines line_word_one pic 9(8) comp.
    77 line_num_one pic 9(8) comp.
    77 line_word_two pic x(10).
    77 line_word_three pic x(10).
    77 finished_dir pic 9.

    01 dir_sizes.
      02 dir_depth pic s9(4) comp.
      02 dir occurs 0 to 999 times
          depending on dir_depth indexed by dir_idx.
        03 dir_name pic x(10).
        03 dir_total pic s9(8) comp.

    77 to_delete pic s9(8) comp.
    77 total_found pic s9(8) comp.
    01 all_totals_table. 
      02 all_totals_cnt pic s9(8) comp.
      02 all_totals occurs 0 to 9999 times
          depending on all_totals_cnt
          descending all_totals_size
          indexed by all_totals_idx.
        03 all_totals_name pic x(10).
        03 all_totals_size pic s9(8) comp.

procedure division.
  call 'lib-readfile' using function module-id rf_all_lines
  *> move "$ cd /" to rf_line_row(1)
  *> move "$ ls" to rf_line_row(2)
  *> move "dir a" to rf_line_row(3)
  *> move "14848514 b.txt" to rf_line_row(4)
  *> move "8504156 c.dat" to rf_line_row(5)
  *> move "dir d" to rf_line_row(6)
  *> move "$ cd a" to rf_line_row(7)
  *> move "$ ls" to rf_line_row(8)
  *> move "dir e" to rf_line_row(9)
  *> move "29116 f" to rf_line_row(10)
  *> move "2557 g" to rf_line_row(11)
  *> move "62596 h.lst" to rf_line_row(12)
  *> move "$ cd e" to rf_line_row(13)
  *> move "$ ls" to rf_line_row(14)
  *> move "584 i" to rf_line_row(15)
  *> move "$ cd .." to rf_line_row(16)
  *> move "$ cd .." to rf_line_row(17)
  *> move "$ cd d" to rf_line_row(18)
  *> move "$ ls" to rf_line_row(19)
  *> move "4060174 j" to rf_line_row(20)
  *> move "8033020 d.log" to rf_line_row(21)
  *> move "5626152 d.ext" to rf_line_row(22)
  *> move "7214296 k" to rf_line_row(23)
  *> move 23 to rf_line_cnt


  set rf_line_idx to 1
  set dir_depth to 1
  move 0 to all_totals_cnt
  perform process_dir

  sort all_totals

  compute to_delete = all_totals_size(1) - 40000000 
  move 0 to total_found
  perform varying all_totals_idx from 1 by 1 until all_totals_idx > all_totals_cnt
    if all_totals_size(all_totals_idx) > to_delete
       move all_totals_size(all_totals_idx) to total_found
    end-if
  end-perform

  display "FINAL: " total_found

  goback.

process_dir.
  *> display "PROCESS: " dir_name(dir_depth)
  move 0 to finished_dir
  move 0 to dir_total(dir_depth)
  perform varying rf_line_idx from rf_line_idx by 1 until rf_line_idx > rf_line_cnt or finished_dir = 1
    *> display "LINE: " function trim(rf_line_row(rf_line_idx))
    unstring rf_line_row(rf_line_idx) delimited by space
      into line_word_one line_word_two line_word_three
    end-unstring
    *> display "LINE: " line_word_one " ( " line_num_one " ) " line_word_two " " line_word_three
    *> display "LINE: " line_word_one " " line_word_two " " line_word_three

    if line_word_two <> "ls"
      if line_word_two = "cd"
        if line_word_three = ".."
          *> display "Finished " dir_name(dir_depth) " size " dir_total(dir_depth)
      *>    if dir_total(dir_depth) < 100000
      *>      compute total_found = total_found + dir_total(dir_depth)
      *>    end-if
          move 1 to finished_dir
        else
          *> Get ready to recurse!
          add 1 to dir_depth
          add 1 to rf_line_idx
          move line_word_three to dir_name(dir_depth)
          perform process_dir

          *> Undo recursion and redo last loop check.
          subtract 1 from dir_depth
          subtract 1 from rf_line_idx
          move 0 to finished_dir

          compute dir_total(dir_depth) = dir_total(dir_depth) + dir_total(dir_depth + 1)
          *> display "Returned from " dir_name(dir_depth + 1) " adding " dir_total(dir_depth + 1) " to " dir_name(dir_depth)

          *> For later processing...
          add 1 to all_totals_cnt
          move dir(dir_depth + 1) to all_totals(all_totals_cnt)
        end-if
      else
        if line_word_one <> "dir"
          move line_word_one to line_num_one
          compute dir_total(dir_depth) = dir_total(dir_depth) + line_num_one
          *> display "Add " line_num_one " to " dir_name(dir_depth)
        end-if
      end-if
    end-if
  end-perform
  .
