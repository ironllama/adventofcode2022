           >>source format free
identification division.
program-id. 02a.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_line_cnt pic s9(8) comp value 0.
      02 rf_line_row pic x(9) occurs 0 to 9999 times
          depending on rf_line_cnt indexed by rf_line_idx.

    01 game_round.
      02 their_move pic x.
      02 your_move pic x.
    01 combos.
      02 winning_combos occurs 3 times indexed by winning_combos_idx.
        03 win_their_move pic x.
        03 win_your_move pic x.
    01 total_points pic 9(8) comp.

procedure division.
  call 'lib-readfile' using function module-id rf_all_lines.
*>   move "A Y" to rf_line_row(1)
*>   move "B X" to rf_line_row(2)
*>   move "C Z" to rf_line_row(3)
*>   move 3 to rf_line_cnt

  move "AB" to winning_combos(1)
  move "BC" to winning_combos(2)
  move "CA" to winning_combos(3)

  move 0 to total_points
  perform varying rf_line_idx from 1 by 1 until rf_line_idx > rf_line_cnt
    unstring function trim(rf_line_row(rf_line_idx)) delimited by space
      into their_move your_move
    end-unstring
    *> display "[" their_move " <> " your_move "]: " no advancing
    if your_move = "X" move "A" to your_move
    else
      if your_move = "Y" move "B" to your_move
      else move "C" to your_move
      end-if
    end-if

    if their_move = your_move
      compute total_points = total_points + 3
    else
      set winning_combos_idx to 1
      search winning_combos
        when win_their_move(winning_combos_idx) = their_move
          if win_your_move(winning_combos_idx) = your_move
            compute total_points = total_points + 6
          end-if
      end-search
    end-if

    if your_move = "A" compute total_points = total_points + 1
    else
      if your_move = "B" compute total_points = total_points + 2
      else compute total_points = total_points + 3
      end-if
    end-if
  end-perform

  display "SCORE: " total_points

  goback.
