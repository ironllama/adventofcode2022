           >>source format free
identification division.
program-id. 23a.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_cnt pic s9(8) comp value 0.
      02 rf_row pic x(9999) occurs 0 to 9999 times
          depending on rf_cnt indexed by rf_idx.
    77 rf_line_len pic s9(2) comp.
    77 rf_char_idx pic s9(2) comp.

    01 all_maps.
      02 map_cnt pic s9(2) comp.
      02 map_old_cnt usage is index.
      02 map_elves_cnt pic s9(4) comp.
      02 map occurs 99 times indexed by map_idx.
        03 map_width pic s9(2) comp.
        03 map_height pic s9(2) comp.
        03 map_row occurs 99 times indexed by map_row_idx.
          04 map_char pic x occurs 99 times indexed by map_col_idx.

    77 check_start pic 9.
    77 check_idx usage is index.
    77 check_now pic 9.

    01 propose_stuff.
      02 propose_cnt pic s9(4) comp.
      02 propose_dupe_idx usage is index.
      02 propose occurs 9999 times indexed by propose_idx.
        03 propose_x pic s9(2) comp.
        03 propose_y pic s9(2) comp.
        03 orig_x pic s9(2) comp.
        03 orig_y pic s9(2) comp.
      02 propose_found_yn pic x value 'N'.
        88 propose_not_found value 'N'.
        88 propose_found value 'Y'.
    *> 77 propose_check_idx usage is index.

    77 total_found pic s9(8) comp.

procedure division.
  *> call 'lib-readdata' using function module-id ".dat" rf_all_lines
  call 'lib-readdata' using function module-id ".da1" rf_all_lines

  move 0 to total_found
  move length of function trim(rf_row(rf_idx)) to rf_line_len

  add 1 to map_cnt
  perform varying rf_idx from 1 by 1 until rf_idx > rf_cnt
    *> display "LINE: " function trim(rf_row(rf_idx))
    perform varying rf_char_idx from 1 by 1 until rf_char_idx > rf_line_len
      move rf_row(rf_idx)(rf_char_idx:1) to map_char(map_cnt rf_idx rf_char_idx)
    end-perform
  end-perform
  move rf_cnt to map_height(map_cnt)
  move rf_line_len to map_width(map_cnt)

*>   perform 10 times
    perform do_round
*>   end-perform

  display "FINAL: " total_found

  goback.

do_round.
  initialize propose_stuff
  *> Expand the map.
  *> display "ORIG HEIGHT: " map_height(map_cnt) " WIDTH: " map_width(map_cnt)
  perform expand_map
  *> display "EXPANDED HEIGHT: " map_height(map_cnt) " WIDTH: " map_width(map_cnt)
  *> display "EXPANDED MAP:"
  perform display_map

  *> Generate proposals.
  move 0 to map_elves_cnt
  perform varying map_row_idx from 2 by 1 until map_row_idx > map_height(map_cnt) - 1
    perform varying map_col_idx from 2 by 1 until map_col_idx > map_width(map_cnt) - 1
      *> Check if visited.
      if map_char(map_cnt map_row_idx map_col_idx) = "#"
        add 1 to map_elves_cnt
        perform check_surroundings
      end-if
    end-perform
  end-perform
  *> Sanity check.
  if propose_cnt <> map_elves_cnt display ">>>>> MISSING ELVES!!! (PROPS: " propose_cnt " ELVES: " map_elves_cnt ") <<<<<" end-if

  *> Remove dupes on proposals.
  perform varying propose_idx from 1 by 1 until propose_idx > propose_cnt
    set propose_not_found to true  *> Assuming there's only 1 duplicate, if any.
    perform varying propose_dupe_idx from 1 by 1 until propose_dupe_idx > propose_cnt or propose_found
      *> If overlapping proposals, set both back to orig positions, so both don't move.
      if propose_idx <> propose_dupe_idx
          and propose_x(propose_idx) = propose_x(propose_dupe_idx)
          and propose_y(propose_idx) = propose_y(propose_dupe_idx)
        move orig_x(propose_idx) to propose_x(propose_idx)
        move orig_y(propose_idx) to propose_y(propose_idx)
        move orig_x(propose_dupe_idx) to propose_x(propose_dupe_idx)
        move orig_y(propose_idx) to propose_y(propose_idx)
        set propose_found to true
      end-if
    end-perform
  end-perform

  *> Everyone move! New map!
  perform gen_new_map
  display "NEW MAP: "
  perform display_map

  compute check_start = function mod((check_start + 1) 4)
  .

gen_new_map.
  perform varying map_row_idx from 1 by 1 until map_row_idx > map_height(map_cnt)
    perform varying map_col_idx from 1 by 1 until map_col_idx > map_width(map_cnt)
      set propose_not_found to true
      perform varying propose_idx from 1 by 1 until propose_idx > propose_cnt or propose_found
        if propose_x(propose_idx) = map_col_idx and propose_y(propose_idx) = map_row_idx
          set propose_found to true
        end-if
      end-perform

      if propose_found
        move "#" to map_char(map_cnt map_row_idx map_col_idx)
      else
        move "." to map_char(map_cnt map_row_idx map_col_idx)
    end-perform
  end-perform
  .

check_surroundings.
  set propose_not_found to true
  perform varying check_idx from 0 by 1 until check_idx > 3 or propose_found
    compute check_now = function mod((check_start + check_idx) 4)
    evaluate check_now
      when 0
        if map_char(map_cnt, map_row_idx - 1, map_col_idx - 1) <> '#'
            and map_char(map_cnt, map_row_idx - 1, map_col_idx) <> '#'
            and map_char(map_cnt, map_row_idx - 1, map_col_idx + 1) <> '#'
         add 1 to propose_cnt
         compute propose_y(propose_cnt) = map_row_idx - 1
         compute propose_x(propose_cnt) = map_col_idx
         compute orig_y(propose_cnt) = map_row_idx
         compute orig_x(propose_cnt) = map_col_idx
         set propose_found to true
        end-if
      when 1
        if map_char(map_cnt, map_row_idx + 1, map_col_idx - 1) <> '#'
            and map_char(map_cnt, map_row_idx + 1, map_col_idx) <> '#'
            and map_char(map_cnt, map_row_idx + 1, map_col_idx + 1) <> '#'
         add 1 to propose_cnt
         compute propose_y(propose_cnt) = map_row_idx + 1
         compute propose_x(propose_cnt) = map_col_idx
         compute orig_y(propose_cnt) = map_row_idx
         compute orig_x(propose_cnt) = map_col_idx
         set propose_found to true
        end-if
      when 2
        if map_char(map_cnt, map_row_idx + 1, map_col_idx - 1) <> '#'
            and map_char(map_cnt, map_row_idx, map_col_idx - 1) <> '#'
            and map_char(map_cnt, map_row_idx - 1, map_col_idx - 1) <> '#'
         add 1 to propose_cnt
         compute propose_y(propose_cnt) = map_row_idx
         compute propose_x(propose_cnt) = map_col_idx - 1
         compute orig_y(propose_cnt) = map_row_idx
         compute orig_x(propose_cnt) = map_col_idx
         set propose_found to true
        end-if
      when 3
        if map_char(map_cnt, map_row_idx + 1, map_col_idx + 1) <> '#'
            and map_char(map_cnt, map_row_idx, map_col_idx + 1) <> '#'
            and map_char(map_cnt, map_row_idx - 1, map_col_idx + 1) <> '#'
         add 1 to propose_cnt
         compute propose_y(propose_cnt) = map_row_idx
         compute propose_x(propose_cnt) = map_col_idx + 1
         compute orig_y(propose_cnt) = map_row_idx
         compute orig_x(propose_cnt) = map_col_idx
         set propose_found to true
        end-if
      when other
        display ">>>>> CHECK CYCLE OVERFLOW!!! >>>>>"
    end-evaluate
  end-perform
  if propose_not_found
    display ">>>>> NOT PROPOSAL FOUND (DIR: " check_now " Y: " map_row_idx " X: " map_col_idx ") <<<<<"
    add 1 to propose_cnt
    compute propose_y(propose_cnt) = map_row_idx
    compute propose_x(propose_cnt) = map_col_idx + 1
    compute orig_y(propose_cnt) = map_row_idx
    compute orig_x(propose_cnt) = map_col_idx
  end-if
  .

expand_map.
  move map_cnt to map_old_cnt
  add 1 to map_cnt
  compute map_height(map_cnt) = map_height(map_old_cnt) + 2
  compute map_width(map_cnt) = map_width(map_old_cnt) + 2
  perform varying map_row_idx from 1 by 1 until map_row_idx > map_height(map_cnt)
    *> display "LINE: " function trim(rf_row(rf_idx))
    perform varying map_col_idx from 1 by 1 until map_col_idx > map_width(map_cnt)
      if map_row_idx = 1 or map_row_idx = map_height(map_cnt)
        move '.' to map_char(map_cnt map_row_idx map_col_idx)
      else
        if map_col_idx = 1 or map_col_idx = map_width(map_cnt)
          move '.' to map_char(map_cnt map_row_idx map_col_idx)
        else
          move map_char(map_old_cnt, map_row_idx - 1, map_col_idx - 1)  *> Commas are optional, but visually easier.
            to map_char(map_cnt map_row_idx map_col_idx)
        end-if
      end-if
    end-perform
  end-perform
  .

display_map.
  perform varying map_row_idx from 1 by 1 until map_row_idx > map_height(map_cnt)
    perform varying map_col_idx from 1 by 1 until map_col_idx > map_width(map_cnt)
      display map_char(map_cnt map_row_idx map_col_idx) no advancing
    end-perform
    display space
  end-perform
  .
