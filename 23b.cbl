           >>source format free
identification division.
program-id. 23b.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_cnt pic s9(8) comp value 0.
      02 rf_row pic x(9999) occurs 0 to 9999 times
          depending on rf_cnt indexed by rf_idx.
    77 rf_line_len pic s9(2) comp.
    77 rf_char_idx pic s9(2) comp.

    01 map_elves_cnt pic s9(4) comp.
    01 map_width pic s9(8) comp.
    01 map_height pic s9(8) comp.
    01 map_stuff.
      02 map_row occurs 9999 times indexed by map_row_idx.
        03 map_char pic x occurs 9999 times indexed by map_col_idx.

    01 temp_map.
      02 temp_map_row occurs 9999 times indexed by temp_map_row_idx.
        03 temp_map_char pic x occurs 9999 times indexed by temp_map_col_idx.

    77 check_start pic 9.
    77 check_idx usage is index.
    77 check_now pic 9.

    01 propose_stuff.
      02 propose_cnt pic s9(8) comp.
      02 propose_dupe_idx usage is index.
      02 propose occurs 9999 times indexed by propose_idx.
        03 propose_x pic s9(8) comp.
        03 propose_y pic s9(8) comp.
        03 orig_x pic s9(8) comp.
        03 orig_y pic s9(8) comp.
      02 propose_found_yn pic x value 'N'.
        88 propose_not_found value 'N'.
        88 propose_found value 'Y'.
      02 neighbor_found_yn pic x value 'N'.
        88 neighbor_not_found value 'N'.
        88 neighbor_found value 'Y'.
    *> 77 propose_check_idx usage is index.

    77 min_width pic s9(8) comp.
    77 min_height pic s9(8) comp.
    77 max_width pic s9(8) comp.
    77 max_height pic s9(8) comp.

    77 total_elves pic s9(4) comp.
    77 elves_not_moving pic s9(4) comp.
    77 round_cnt pic s9(8) comp.
    01 rounds_control pic x value 'Y'.
      88 more_rounds value 'Y'.
      88 no_more_rounds value 'N'.

    77 total_found pic s9(8) comp.

procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
  *> call 'lib-readdata' using function module-id ".da1" rf_all_lines
  *> call 'lib-readdata' using function module-id ".da2" rf_all_lines

  move 0 to total_found
  move length of function trim(rf_row(rf_idx)) to rf_line_len

  perform varying rf_idx from 1 by 1 until rf_idx > rf_cnt
    *> display "LINE: " function trim(rf_row(rf_idx))
    perform varying rf_char_idx from 1 by 1 until rf_char_idx > rf_line_len
      move rf_row(rf_idx)(rf_char_idx:1) to map_char(rf_idx rf_char_idx)
      if rf_row(rf_idx)(rf_char_idx:1) = "#" add 1 to total_elves end-if
    end-perform
  end-perform
  move rf_cnt to map_height
  move rf_line_len to map_width
  display "TOTAL ELVES: " total_elves

  *> perform 10 times
  perform until no_more_rounds
    add 1 to round_cnt
    perform do_round
  end-perform

  *> display "SCORING: min_height: " min_height " max_height: " max_height " min_width: " min_width " max_width: " max_width
  *> perform varying map_row_idx from min_height by 1 until map_row_idx > max_height
  *>   perform varying map_col_idx from min_width by 1 until map_col_idx > max_width
  *>     *> display map_char(map_row_idx map_col_idx) no advancing
  *>      if map_char(map_row_idx map_col_idx) = "." add 1 to total_found end-if
  *>   end-perform
  *>   *> display space
  *> end-perform

  compute total_found = round_cnt

  display "FINAL: " total_found

  goback.


do_round.
  initialize propose_stuff
  move 0 to elves_not_moving
  *> Expand the map.
  *> display "ORIG HEIGHT: " map_height " WIDTH: " map_width
  perform expand_map
  *> display "EXPANDED HEIGHT: " map_height " WIDTH: " map_width
  *> display "EXPANDED MAP:"
  *> perform display_map

  *> Generate proposals.
  move 0 to map_elves_cnt
  perform varying map_row_idx from 2 by 1 until map_row_idx > map_height - 1
    perform varying map_col_idx from 2 by 1 until map_col_idx > map_width - 1
      *> Check if visited.
      if map_char(map_row_idx map_col_idx) = "#"
        add 1 to map_elves_cnt
        perform check_surroundings
      end-if
    end-perform
  end-perform
  *> Sanity check.
  if propose_cnt <> map_elves_cnt display ">>>>> MISSING ELVES!!! (PROPS: " propose_cnt " ELVES: " map_elves_cnt ") <<<<<" end-if

  *> Remove dupes on proposals.
  perform varying propose_idx from 1 by 1 until propose_idx > propose_cnt or no_more_rounds
    set propose_not_found to true  *> Assuming there's only 1 duplicate, if any.
    perform varying propose_dupe_idx from 1 by 1 until propose_dupe_idx > propose_cnt or propose_found
      *> If overlapping proposals, set both back to orig positions, so both don't move.
      if propose_idx <> propose_dupe_idx
          and propose_x(propose_idx) = propose_x(propose_dupe_idx)
          and propose_y(propose_idx) = propose_y(propose_dupe_idx)
        move orig_x(propose_idx) to propose_x(propose_idx)
        move orig_y(propose_idx) to propose_y(propose_idx)
        move orig_x(propose_dupe_idx) to propose_x(propose_dupe_idx)
        move orig_y(propose_dupe_idx) to propose_y(propose_dupe_idx)
        set propose_found to true
      end-if
    end-perform
  end-perform

  *> Everyone move! New map!
  *> if more_rounds
  perform gen_new_map
  *> end-if

  perform shrink_map

  *> display "NEW MAP: [" round_cnt "]"
  *> perform display_map

  compute check_start = function mod((check_start + 1) 4)
  .

shrink_map.
  initialize temp_map
  set temp_map_row_idx to 0
  perform varying map_row_idx from min_height by 1 until map_row_idx > max_height
    add 1 to temp_map_row_idx
    set temp_map_col_idx to 0
    perform varying map_col_idx from min_width by 1 until map_col_idx > max_width
      add 1 to temp_map_col_idx
      move map_char(map_row_idx map_col_idx) to temp_map_char(temp_map_row_idx temp_map_col_idx)
    end-perform
  end-perform
  initialize map_stuff
  move temp_map to map_stuff
  move max_height to map_height
  move max_width to map_width
  .

gen_new_map.
  move 0 to max_height
  move 0 to max_width
  move map_height to min_height
  move map_width to min_width

  perform varying map_row_idx from 1 by 1 until map_row_idx > map_height
    perform varying map_col_idx from 1 by 1 until map_col_idx > map_width
      set propose_not_found to true
      perform varying propose_idx from 1 by 1 until propose_idx > propose_cnt or propose_found
        if propose_x(propose_idx) = map_col_idx and propose_y(propose_idx) = map_row_idx
          set propose_found to true
        end-if
      end-perform

      if propose_found
        move "#" to map_char(map_row_idx map_col_idx)
        if map_row_idx < min_height move map_row_idx to min_height end-if
        if map_col_idx < min_width move map_col_idx to min_width end-if
        if map_row_idx > max_height move map_row_idx to max_height end-if
        if map_col_idx > max_width move map_col_idx to max_width end-if
      else
        move "." to map_char(map_row_idx map_col_idx)
    end-perform
  end-perform
  .

check_surroundings.
  set propose_not_found to true
  set neighbor_not_found to true
  perform varying check_idx from 0 by 1 until check_idx > 3 or (propose_found and neighbor_found)
    compute check_now = function mod((check_start + check_idx) 4)
    *> display "check_now: " check_now " Y: " map_row_idx " X: " map_col_idx
    evaluate check_now
      when 0
        if map_char(map_row_idx - 1, map_col_idx - 1) <> '#'
            and map_char(map_row_idx - 1, map_col_idx) <> '#'
            and map_char(map_row_idx - 1, map_col_idx + 1) <> '#'
          if propose_not_found
            add 1 to propose_cnt
            compute propose_y(propose_cnt) = map_row_idx - 1
            compute propose_x(propose_cnt) = map_col_idx
            compute orig_y(propose_cnt) = map_row_idx
            compute orig_x(propose_cnt) = map_col_idx
            set propose_found to true
          end-if
        else set neighbor_found to true
        end-if
      when 1
        if map_char(map_row_idx + 1, map_col_idx - 1) <> '#'
            and map_char(map_row_idx + 1, map_col_idx) <> '#'
            and map_char(map_row_idx + 1, map_col_idx + 1) <> '#'
          if propose_not_found
            add 1 to propose_cnt
            compute propose_y(propose_cnt) = map_row_idx + 1
            compute propose_x(propose_cnt) = map_col_idx
            compute orig_y(propose_cnt) = map_row_idx
            compute orig_x(propose_cnt) = map_col_idx
            set propose_found to true
          end-if
        else set neighbor_found to true
        end-if
      when 2
        if map_char(map_row_idx + 1, map_col_idx - 1) <> '#'
            and map_char(map_row_idx, map_col_idx - 1) <> '#'
            and map_char(map_row_idx - 1, map_col_idx - 1) <> '#'
          if propose_not_found
            add 1 to propose_cnt
            compute propose_y(propose_cnt) = map_row_idx
            compute propose_x(propose_cnt) = map_col_idx - 1
            compute orig_y(propose_cnt) = map_row_idx
            compute orig_x(propose_cnt) = map_col_idx
            set propose_found to true
          end-if
        else set neighbor_found to true
        end-if
      when 3
        if map_char(map_row_idx + 1, map_col_idx + 1) <> '#'
            and map_char(map_row_idx, map_col_idx + 1) <> '#'
            and map_char(map_row_idx - 1, map_col_idx + 1) <> '#'
          if propose_not_found
            add 1 to propose_cnt
            compute propose_y(propose_cnt) = map_row_idx
            compute propose_x(propose_cnt) = map_col_idx + 1
            compute orig_y(propose_cnt) = map_row_idx
            compute orig_x(propose_cnt) = map_col_idx
            set propose_found to true
          end-if
        else set neighbor_found to true
        end-if
      when other
        display ">>>>> CHECK CYCLE OVERFLOW!!! >>>>>"
    end-evaluate
  end-perform
  if neighbor_not_found
    compute propose_y(propose_cnt) = orig_y(propose_cnt)
    compute propose_x(propose_cnt) = orig_x(propose_cnt)

    add 1 to elves_not_moving
    if elves_not_moving = total_elves
      display "END: " round_cnt
      set no_more_rounds to true
    end-if
  else
    if propose_not_found
      *> display ">>>>> NO PROPOSAL FOUND (DIR: " check_now " Y: " map_row_idx " X: " map_col_idx ") <<<<<"
      add 1 to propose_cnt
      compute propose_y(propose_cnt) = map_row_idx
      compute propose_x(propose_cnt) = map_col_idx
      compute orig_y(propose_cnt) = map_row_idx
      compute orig_x(propose_cnt) = map_col_idx
    end-if
  end-if
  *> display "PROPOSE: [" round_cnt "] ORIGIN: Y: " orig_y(propose_cnt) " X: " orig_x(propose_cnt) " PRO: Y: " propose_y(propose_cnt) " X: " propose_x(propose_cnt)
  .

expand_map.
  initialize temp_map
  move map_stuff to temp_map
  compute map_height = map_height + 2
  compute map_width = map_width + 2
  perform varying map_row_idx from 1 by 1 until map_row_idx > map_height
    *> display "LINE: " function trim(rf_row(rf_idx))
    perform varying map_col_idx from 1 by 1 until map_col_idx > map_width
      if map_row_idx = 1 or map_row_idx = map_height
        move '.' to map_char(map_row_idx map_col_idx)
      else
        if map_col_idx = 1 or map_col_idx = map_width
          move '.' to map_char(map_row_idx map_col_idx)
        else
          move temp_map_char(map_row_idx - 1, map_col_idx - 1)  *> Commas are optional, but visually easier.
            to map_char(map_row_idx map_col_idx)
        end-if
      end-if
    end-perform
  end-perform
  .

display_map.
  perform varying map_row_idx from 1 by 1 until map_row_idx > map_height
    perform varying map_col_idx from 1 by 1 until map_col_idx > map_width
      display map_char(map_row_idx map_col_idx) no advancing
    end-perform
    display space
  end-perform
  .
