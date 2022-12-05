           >>source format free
identification division.
program-id. 02a.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_line_cnt pic s9(8) comp value 0.
      02 rf_line_row pic x(99) occurs 0 to 9999 times
          depending on rf_line_cnt indexed by rf_line_idx.

    01 game_round.
      02 their_move pic x.
      02 your_move pic x.
    01 total_points pic 9(8) comp.

procedure division.
  call 'lib-readfile' using function module-id rf_all_lines.
*>   move "A Y" to rf_line_row(1)
*>   move "B X" to rf_line_row(2)
*>   move "C Z" to rf_line_row(3)
*>   move 3 to rf_line_cnt

  move 0 to total_points
  perform varying rf_line_idx from 1 by 1 until rf_line_idx > rf_line_cnt
    unstring function trim(rf_line_row(rf_line_idx)) delimited by space
      into their_move your_move
    end-unstring
    *> display "[" their_move " <> " your_move "]: " no advancing
    if their_move = "A"  *> rock
      if your_move = "X" perform tie
      else
        if your_move = "Y" perform win
        else perform lose
        end-if
      end-if
    else
      if their_move = "B"  *> paper
        if your_move = "X" perform lose
        else
          if your_move = "Y" perform tie
          else perform win
          end-if
        end-if
      else  *> scissors
        if your_move = "X" perform win
        else
          if your_move = "Y" perform lose
          else perform tie
          end-if
        end-if
    end-if
  end-perform

  display "SCORE: " total_points

  goback.

round_end.
  if your_move = "X" compute total_points = total_points + 1
  else
    if your_move = "Y" compute total_points = total_points + 2
    else compute total_points = total_points + 3
    end-if
  end-if
  .

win.
  perform round_end
  compute total_points = total_points + 6
  *> display "WIN: " total_points
  .

tie.
  perform round_end
  compute total_points = total_points + 3
  *> display "TIE: " total_points
  .

lose.
  perform round_end
  *> display "LOSE: " total_points
  .
