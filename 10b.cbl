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
    77 add_cycle_idx pic s9.

    77 line_pos pic s9(2) comp.
    77 line_so_far pic x(40).

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
      perform check_cycle  *> Doing this before first increment, CRT line is 0-indexed
      add 1 to total_cycles
      *> perform check_cycle
    else if instr = "addx"
        *> Since the addx only acts at the end of its cycles, it has its own loop.
        perform varying add_cycle_idx from 1 by 1 until add_cycle_idx > 2
          perform check_cycle
          add 1 to total_cycles
          *> perform check_cycle
  
          if add_cycle_idx = 2
            compute x_val = x_val + amt
          end-if
        end-perform
      end-if
    end-if
  end-perform

  goback.

check_cycle.
  *> Clear out the line buffer and get ready to roll.
  if function mod(total_cycles 40) = 0
    display space
  end-if
  
  *> Get current drawing pixel position
  move function mod(total_cycles 40) to line_pos

  *> Check line pixel pos against sprite pos (+/- 1)
  if line_pos = x_val - 1 or line_pos = x_val or line_pos = x_val + 1
    display "#" no advancing
  else
    display "." no advancing
  end-if
  .
  