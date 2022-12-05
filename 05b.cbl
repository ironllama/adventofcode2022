           >>source format free
identification division.
program-id. 05b.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_line_cnt pic s9(8) comp value 0.
      02 rf_line_row pic x(99) occurs 0 to 9999 times
          depending on rf_line_cnt indexed by rf_line_idx.

    77 dock_done pic 9 comp.
    77 dock_char_found pic 9 comp.
    77 dock_char_idx pic s9(4) comp.
    77 dock_char_adv pic s9(4) comp.
    77 crate_char pic x.

    01 dock.
      02 dock_cnt pic s9(4) comp.
      02 dock_row occurs 9 times indexed by dock_idx.
        03 crate_stack.
          04 crate_stack_cnt pic s9(8) comp value 0.
          04 crate_stack_row pic x(99).

    01 directions.
      02 move_amt pic 9(2) comp.
      02 move_from pic 9(2) comp.
      02 move_to pic 9(2) comp.

    77 unstring_idx pic 9(2) comp.
    77 crane pic x(99).
    77 crane_idx pic 9(2) comp.
    77 temp_crate pic x(99).

    77 total_found pic x(99).

procedure division.
  call 'lib-readfile' using function module-id rf_all_lines
  move 9 to dock_cnt

*>   move "    [D]    " to rf_line_row(1)
*>   move "[N] [C]    " to rf_line_row(2)
*>   move "[Z] [M] [P]" to rf_line_row(3)
*>   move " 1   2   3 " to rf_line_row(4)
*>   move space to rf_line_row(5)
*>   move "move 1 from 2 to 1" to rf_line_row(6)
*>   move "move 3 from 1 to 3" to rf_line_row(7)
*>   move "move 2 from 2 to 1" to rf_line_row(8)
*>   move "move 1 from 1 to 2" to rf_line_row(9)
*>   move 9 to rf_line_cnt
*>   move 3 to dock_cnt

  move 0 to total_found
  perform varying rf_line_idx from 1 by 1 until rf_line_idx > rf_line_cnt
    *> display "LINE! [" rf_line_row(rf_line_idx) "]"

        *> First, parse the dock/crate initial setup.
    if dock_done = 0
      *> Check to see if line is a [x] line, or just a num line
      move 0 to dock_char_found
      perform varying dock_char_idx from 1 by 1 until dock_char_idx > ((dock_cnt * 4) - 1) or dock_char_found = 1
        *> display "SEARCH: " dock_char_idx
        if rf_line_row(rf_line_idx)(dock_char_idx:1) = "["
          add 1 to dock_char_found
        end-if
      end-perform

      if dock_char_found = 1
        move 1 to dock_char_idx
        perform varying dock_char_adv from 1 by 1 until dock_char_adv > ((dock_cnt * 4) - 1)
          move rf_line_row(rf_line_idx)(dock_char_adv + 1:1) to crate_char
          if crate_char <> " "
            *> display "FOUND: " crate_char " ADDING TO: " dock_char_idx " VALUE [" crate_stack_row(dock_char_idx) "]"
            add 1 to crate_stack_cnt(dock_char_idx)
            string crate_stack_row(dock_char_idx) delimited by spaces
              crate_char
              into crate_stack_row(dock_char_idx)
            end-string
          end-if
          add 3 to dock_char_adv
          add 1 to dock_char_idx
        end-perform
      else
        move 1 to dock_done
        add 1 to rf_line_idx
        *> perform display_dock
      end-if

    else
    *> Then parse all the moves.

      move 6 to unstring_idx  *> Skip the "move " in the line.
      unstring rf_line_row(rf_line_idx) delimited by " from " or " to "
          *> into directions  *> WHY DOESN'T THIS WORK?!?! AARRRGGG!!
          into move_amt move_from move_to
          with pointer unstring_idx
      end-unstring
    *>   display "amt " move_amt " from " move_from " to " move_to

      move crate_stack_row(move_from)(1:move_amt) to crane

      move crate_stack_row(move_from)(move_amt + 1:) to crate_stack_row(move_from)
      subtract move_amt from crate_stack_cnt(move_from)

      move spaces to temp_crate
      string
          crane delimited by space
          crate_stack_row(move_to) delimited by space
        into temp_crate
      end-string
      move temp_crate to crate_stack_row(move_to)
    end-if
  end-perform

  move spaces to total_found
  perform varying dock_idx from 1 by 1 until dock_idx > 9
    string total_found delimited by space
      crate_stack_row(dock_idx)(1:1)
      into total_found
    end-string
  end-perform

  perform display_dock
  display "FINAL: " function trim(total_found)

  goback.

display_dock.
  set dock_idx to 1
  perform varying dock_idx from 1 by 1 until dock_idx > dock_cnt
    display "[" dock_idx "]: " function trim(crate_stack_row(dock_idx))
  end-perform
  .
