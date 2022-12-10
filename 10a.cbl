           >>source format free
identification division.
program-id. 10a.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_line_cnt pic s9(8) comp value 0.
      02 rf_line_row pic x(9999) occurs 0 to 9999 times
          depending on rf_line_cnt indexed by rf_line_idx.

    77 x_val pic s9(8) comp.
    77 total_cycles pic s9(8) comp.

    77 gap_cycles pic s9(4) comp.
    77 addx_cycle_idx pic s9.

    77 instr pic x(8).
    77 amt pic s9(4) comp.

    77 total_found pic s9(8) comp.
    77 curr_found pic s9(8) comp.

procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
  *> call 'lib-readdata' using function module-id ".da1" rf_all_lines
  *> move "noop" to rf_line_row(1)
  *> move "addx 3" to rf_line_row(2)
  *> move "addx -5" to rf_line_row(3)
  *> move 3 to rf_line_cnt

  move 0 to total_found
  move 0 to total_cycles
  move 1 to x_val

  perform varying rf_line_idx from 1 by 1 until rf_line_idx > rf_line_cnt
    *> display "LINE: " function trim(rf_line_row(rf_line_idx))
    *> Process the line.
    move spaces to instr
    move zeros to amt
    unstring function trim(rf_line_row(rf_line_idx)) delimited by space
      into instr amt
    end-unstring
    *> display "NEW: " function trim(instr) " : " amt " x-before: " x_val
    
    if instr = "noop"
      add 1 to total_cycles
      perform check_cycle
    else
      if instr = "addx"
        *> Since the addx only acts at the end of its cycles, it has its own loop.
        perform varying addx_cycle_idx from 1 by 1 until addx_cycle_idx > 2
          add 1 to total_cycles
          perform check_cycle

          if addx_cycle_idx = 2
            compute x_val = x_val + amt
          end-if
        end-perform
      end-if
    end-if
  end-perform

  display "FINAL: " total_found

  goback.

check_cycle.
    *> display "check_cycle: " function trim(instr) " : " amt " x-before: " x_val
    compute gap_cycles = total_cycles - 20 
    if total_cycles = 20 or function mod(gap_cycles 40) = 0
      compute curr_found = total_cycles * x_val
      compute total_found = total_found + curr_found
      *> display total_cycles ": " x_val " C: " curr_found " T: " total_found
    end-if

    *> Check before and after interesting cycles for debugging.
    *> if total_cycles = 19 or total_cycles = 21
        *> or total_cycles = 59 or total_cycles = 61
        *> or total_cycles = 99 or total_cycles = 101
        *> or total_cycles = 139 or total_cycles = 141
        *> or total_cycles = 179 or total_cycles = 181
        *> or total_cycles = 219 or total_cycles = 221
      *> display total_cycles ": " x_val " C: " curr_found " T: " total_found
    *> end-if
  .
  