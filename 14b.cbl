           >>source format free
identification division.
program-id. 14b.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_cnt pic s9(8) comp value 0.
      02 rf_row pic x(9999) occurs 0 to 9999 times
          depending on rf_cnt indexed by rf_idx.

    01 line_split.
      02 split_done pic 9.
      02 split_ptr pic 9(4).
      02 split_cnt pic 9(4).
      02 split occurs 0 to 99 times
          depending on split_cnt indexed by split_idx.
        03 split_x pic 9(4).
        03 split_y pic 9(4).
    01 split_inner_idx pic 9(4).
    01 split_inner_beg pic 9(4).
    01 split_inner_end pic 9(4).

    01 temp_line pic x(9).
    01 temp_x pic 9(4).
    01 temp_y pic 9(4).

    01 all_rocks.
      02 rock_cnt pic 9(8).
      02 rock occurs 0 to 99999 times
         depending on rock_cnt indexed by rock_idx.
        03 rock_x pic 9(4).
        03 rock_y pic 9(4).
    01 rock_cnt_init pic 9(4).
    01 rock_found pic 9.
    01 lowest_x pic 9(4).
    01 lowest_y pic 9(4).
    01 highest_x pic 9(4).
    01 highest_y pic 9(4).

    01 temp_idx_y pic 9(4).
    01 temp_idx_x pic 9(4).

    01 sand_x pic 9(4) comp.
    01 sand_y pic 9(4) comp.
    01 sand_last_arr.
      02 sand_last_cnt pic 9(4) comp.
      02 sand_last occurs 0 to 999 times
          depending on sand_last_cnt indexed by sand_last_idx.
        03 sand_last_x pic 9(4) comp.
        03 sand_last_y pic 9(4) comp.

    01 sand_dl pic 9(4) comp.
    01 sand_dr pic 9(4) comp.
    01 origin_blocked pic 9.
    01 sand_at_rest pic 9.
    01 rock_found_dn pic 9.
    01 rock_found_dl pic 9.
    01 rock_found_dr pic 9.

procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
*>   call 'lib-readdata' using function module-id ".da1" rf_all_lines

  move high-value to lowest_x
  move high-value to lowest_y
  move low-value to highest_x
  move low-value to highest_y

  perform varying rf_idx from 1 by 1 until rf_idx > rf_cnt
    *> display "LINE: " function trim(rf_row(rf_idx))
    move spaces to temp_line

    move 0 to split_done
    set split_ptr to 1
    move 0 to split_cnt
    *> initialize line_split

    perform until split_done = 1
      unstring function trim(rf_row(rf_idx)) delimited by " -> "
        into temp_line
        with pointer split_ptr
        on overflow
          if split_ptr > length of function trim(rf_row(rf_idx))
            move 1 to split_done
        *>   else
          end-if
            *> Split each X,Y combo into the individual coords.
            *> move temp_line to split(split_cnt)
            if temp_line <> spaces
              unstring function trim(temp_line) delimited by ","
                into temp_x temp_y
              end-unstring
              *> display "X/Y: " temp_x " " temp_y
              add 1 to split_cnt
              move temp_x to split_x(split_cnt)
              move temp_y to split_y(split_cnt)
            end-if
        *>   end-if
      end-unstring
    end-perform

    *> display "SPLIT: [" no advancing
    *> perform varying split_idx from 1 by 1 until split_idx > split_cnt
      *> display "(" split_x(split_idx) "," split_y(split_idx) "), " no advancing
    *> end-perform
    *> display "]"

    *> Move the ranges into the rock table.
    perform varying split_idx from 1 by 1 until split_idx > (split_cnt - 1)
       if split_x(split_idx) <> split_x(split_idx + 1)
         if split_x(split_idx) < split_x(split_idx + 1)
           move split_x(split_idx) to split_inner_beg
           move split_x(split_idx + 1) to split_inner_end
         else
           move split_x(split_idx + 1) to split_inner_beg
           move split_x(split_idx) to split_inner_end
         end-if

         if split_inner_beg < lowest_x or split_inner_beg = 0 move split_inner_beg to lowest_x end-if
         if split_inner_end > highest_x or split_inner_end = 0 move split_inner_end to highest_x end-if
         if split_y(split_idx) < lowest_y move split_y(split_idx) to lowest_y end-if
         if split_y(split_idx) > highest_y move split_y(split_idx) to highest_y end-if

         perform varying split_inner_idx from split_inner_beg by 1 until split_inner_idx > split_inner_end
         *> Check if it's already in the rocks table, before adding it.
           move 0 to rock_found
           perform varying rock_idx from 1 by 1 until rock_idx > rock_cnt or rock_found = 1
             if rock_x(rock_idx) = split_x(split_idx) and rock_y(rock_idx) = split_inner_idx
               move 1 to rock_found
             end-if
           end-perform
           if rock_found = 0
             add 1 to rock_cnt
             move split_inner_idx to rock_x(rock_cnt)
             move split_y(split_idx) to rock_y(rock_cnt)
           end-if
         end-perform
       end-if

       if split_y(split_idx) <> split_y(split_idx + 1)
         if split_y(split_idx) < split_y(split_idx + 1)
           move split_y(split_idx) to split_inner_beg
           move split_y(split_idx + 1) to split_inner_end
         else
           move split_y(split_idx + 1) to split_inner_beg
           move split_y(split_idx) to split_inner_end
         end-if

         if split_inner_beg < lowest_y or split_inner_beg = 0 move split_inner_beg to lowest_y end-if
         if split_inner_end > highest_y or split_inner_end = 0 move split_inner_end to highest_y end-if
         if split_x(split_idx) < lowest_x move split_x(split_idx) to lowest_x end-if
         if split_x(split_idx) > highest_x move split_x(split_idx) to highest_x end-if

         perform varying split_inner_idx from split_inner_beg by 1 until split_inner_idx > split_inner_end
           *> Check if it's already in the rocks table, before adding it.
           move 0 to rock_found
           perform varying rock_idx from 1 by 1 until rock_idx > rock_cnt or rock_found = 1
             if rock_x(rock_idx) = split_x(split_idx) and rock_y(rock_idx) = split_inner_idx
               move 1 to rock_found
             end-if
           end-perform
           if rock_found = 0
             add 1 to rock_cnt
             move split_x(split_idx) to rock_x(rock_cnt)
             move split_inner_idx to rock_y(rock_cnt)
           end-if
         end-perform
       end-if

    end-perform
  end-perform
  move rock_cnt to rock_cnt_init

  display "ROCK COUNT: " rock_cnt " LOWEST: " lowest_x " " lowest_y " HIGHEST: " highest_x " " highest_y
*>   display "ROCKS: [" no advancing
*>   perform varying rock_idx from 1 by 1 until rock_idx > rock_cnt
*>     display "(" rock_x(rock_idx) "," rock_y(rock_idx) "), " no advancing
*>   end-perform
*>   display "]"

*>   perform print_progress

  *> Drop the sand
  move 0 to origin_blocked
  add 1 to sand_last_cnt
  move 500 to sand_last_x(sand_last_cnt)
  move 0 to sand_last_y(sand_last_cnt)

  perform until origin_blocked = 1
    *> Generate a new unit of sand
    *> move 500 to sand_x
    *> move 0 to sand_y

    move sand_last_x(sand_last_cnt) to sand_x
    move sand_last_y(sand_last_cnt) to sand_y

    move 0 to sand_at_rest
    *> display "NEW SAND: " rock_cnt " (" sand_x ", " sand_y ")"

    perform until sand_at_rest = 1 or origin_blocked = 1
      *> Does this rock already exist?
      move 0 to rock_found_dn
      move 0 to rock_found_dr
      move 0 to rock_found_dl

      *> Check if not already on the 'floor'
      if sand_y = highest_y + 1
        move 1 to rock_found_dn
        move 1 to rock_found_dr
        move 1 to rock_found_dl
      else
        *> Test future positions.
        perform varying rock_idx from 1 by 1 until rock_idx > rock_cnt or (rock_found_dn <> 0 and rock_found_dl <> 0 and rock_found_dr <> 0)
          *> display "ROCK LOOP: " rock_idx " of " rock_cnt " " rock_x(rock_idx) " " rock_y(rock_idx) " SAND: " sand_x " " sand_y
          *> Straight down

          if (rock_x(rock_idx) = sand_x and rock_y(rock_idx) = sand_y + 1)
            move 1 to rock_found_dn
          else
            *> Down left
            if rock_x(rock_idx) = sand_x - 1 and rock_y(rock_idx) = sand_y + 1
              move 1 to rock_found_dl
            else
              *> Down right
              if rock_x(rock_idx) = sand_x + 1 and rock_y(rock_idx) = sand_y + 1
                move 1 to rock_found_dr
              end-if
            end-if
          end-if
        end-perform
      end-if
      *> display "TESTING: " rock_found_dn " " rock_found_dl " " rock_found_dr

      *> Determine what to do.
      if rock_found_dn = 0
        *> display "FALLING"
        add 1 to sand_y
      else
        if rock_found_dl = 0
          add 1 to sand_y
          subtract 1 from sand_x
        else
          if rock_found_dr = 0
            add 1 to sand_y
            add 1 to sand_x
          else
            *> Comes to rest -- add as a rock.
            add 1 to rock_cnt
            move sand_y to rock_y(rock_cnt)
            move sand_x to rock_x(rock_cnt)
            *> display "ADDED: " sand_x " " sand_y
            move 1 to sand_at_rest

            if sand_x = 500 and sand_y = 0
              move 1 to origin_blocked
            end-if

            subtract 1 from sand_last_cnt
          end-if
        end-if
      end-if

      if sand_at_rest = 0
        add 1 to sand_last_cnt
        move sand_x to sand_last_x(sand_last_cnt)
        move sand_y to sand_last_y(sand_last_cnt)
      end-if
    end-perform
  end-perform

  compute rock_cnt = rock_cnt - rock_cnt_init
  display "FINAL: " rock_cnt

  goback.


print_progress.
  perform varying temp_idx_y from lowest_y by 1 until temp_idx_y > highest_y
    perform varying temp_idx_x from lowest_x by 1 until temp_idx_x > highest_x
      move 0 to rock_found
      perform varying rock_idx from 1 by 1 until rock_idx > rock_cnt or rock_found = 1
        if rock_x(rock_idx) = temp_idx_x and rock_y(rock_idx) = temp_idx_y
          move 1 to rock_found
        end-if
      end-perform
      if rock_found = 1 display "#" no advancing else display "." no advancing end-if
    end-perform
    display space
  end-perform
  .
