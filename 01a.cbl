           >>source format free
identification division.
program-id. 01a.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_line_cnt pic s9(8) comp value 0.
      02 rf_line_row pic x(9) occurs 0 to 999 times
          depending on rf_line_cnt indexed by rf_line_idx.

    01 line_as_num pic s9(8) comp.

    01 elves.
      02 elf_num pic s9(4) comp value 1.
      02 elf_total pic 9(8) comp value 0 occurs 0 to 99999 times
          depending on elf_num indexed by elf_idx.

    01 highest pic s9(8) comp value 0.

procedure division.
  call 'lib-readfile' using function module-id rf_all_lines.
  *> move "1000" to rf_line_row(1)
  *> move "2000" to rf_line_row(2)
  *> move "3000" to rf_line_row(3)
  *> move "    " to rf_line_row(4)
  *> move "4000" to rf_line_row(5)
  *> move "    " to rf_line_row(6)
  *> move "5000" to rf_line_row(7)
  *> move "6000" to rf_line_row(8)
  *> move "    " to rf_line_row(9)
  *> move "7000" to rf_line_row(10)
  *> move "8000" to rf_line_row(11)
  *> move "9000" to rf_line_row(12)
  *> move "    " to rf_line_row(13)
  *> move "10000" to rf_line_row(14)
  *> move 14 to rf_line_cnt

  *> Initialize variables when using lots of data! Else, chaos.
  move 1 to elf_num
  move 0 to elf_total(elf_num)
  perform varying rf_line_idx from 1 by 1 until rf_line_idx > rf_line_cnt
    if rf_line_row(rf_line_idx)(1:1) = " "
      compute elf_num = elf_num + 1
      move 0 to elf_total(elf_num)
    else
      move rf_line_row(rf_line_idx) to line_as_num
      compute elf_total(elf_num) = elf_total(elf_num) + line_as_num
      *> display "ADDED ELF[" elf_num "]: " line_as_num " " elf_total(elf_num)
  end-perform

  perform varying elf_idx from 1 by 1 until elf_idx > elf_num
    *> display "ELF[" elf_idx "] " elf_total(elf_idx)
    if elf_total(elf_idx) > highest
      move elf_total(elf_idx) to highest
    end-if
  end-perform

  display "HIGHEST: " highest

  goback.
