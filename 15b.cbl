           >>source format free
identification division.
program-id. 15b.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_cnt pic s9(8) comp value 0.
      02 rf_row pic x(9999) occurs 0 to 9999 times
          depending on rf_cnt indexed by rf_idx.

    77 split_colon_l pic x(99).
    77 split_colon_r pic x(99).
    77 split_xy_str pic x(99).
    77 split_x_str pic x(9).
    77 split_y_str pic x(9).
    77 split_misc_1 pic x(99).
    77 split_num_1 pic s9(8) comp.

    01 sensors.
      02 sensor_cnt pic s9(2) comp value 0.
      02 sensor occurs 0 to 99 times
          depending on sensor_cnt indexed by sensor_idx.
        03 sensor_x pic s9(8) comp.
        03 sensor_y pic s9(8) comp.
        03 beacon_x pic s9(8) comp.
        03 beacon_y pic s9(8) comp.
        03 diff_md pic s9(8) comp.
    01 ranges.
      02 range_cnt pic s9(2) comp value 0.
      02 range_row occurs 0 to 99 times
          depending on range_cnt indexed by range_idx.
        03 range_start pic s9(8) comp.
        03 range_end pic s9(8) comp.
        03 range_use pic 9 comp value 1.
    77 range_found pic 9.
    77 range_idx2 pic s9(4) comp.
    77 num_nonoverlap pic 9(4) comp.

    *> 77 line_beacon_y pic s9(8) comp value 0.
    *> 77 line_beacon_x pic s9(8) comp value 0.
    77 line_idx usage is index.

    77 limit_start pic s9(8) comp.
    77 limit_end pic s9(8) comp.

    77 diff_x pic s9(8) comp.
    77 diff_y pic s9(8) comp.
    77 total_diff pic s9(8) comp.
    77 line_diff pic s9(8) comp.
    77 line_diff_x pic s9(8) comp.
    77 row_start pic s9(8) comp.
    77 row_end pic s9(8) comp.

    77 total_found pic s9(18) comp.

procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
*>   move 2000000 to line_beacon_y
*>   call 'lib-readdata' using function module-id ".da1" rf_all_lines
*> *>   move 10 to line_beacon_y

  *> First, get all the sensors.
  perform varying rf_idx from 1 by 1 until rf_idx > rf_cnt
    add 1 to sensor_cnt

    *> Split line into sensor and beacon sections.
    unstring function trim(rf_row(rf_idx)) delimited by ":"
      into split_colon_l split_colon_r
    end-unstring

    *> Sensor from line.
    unstring function trim(split_colon_l) delimited by " at "
      into split_misc_1 split_xy_str
    end-unstring
    unstring function trim(split_xy_str) delimited by ", "
      into split_x_str split_y_str
    end-unstring
    move function trim(split_x_str(3:)) to sensor_x(sensor_cnt)
    move function trim(split_y_str(3:)) to sensor_y(sensor_cnt)

    *> Beacon from line.
    unstring function trim(split_colon_r) delimited by " at "
      into split_misc_1 split_xy_str
    end-unstring
    unstring function trim(split_xy_str) delimited by ", "
      into split_x_str split_y_str
    end-unstring
    move function trim(split_x_str(3:)) to beacon_x(sensor_cnt)
    move function trim(split_y_str(3:)) to beacon_y(sensor_cnt)

    compute diff_x = function abs(sensor_x(sensor_cnt) - beacon_x(sensor_cnt))
    compute diff_y = function abs(sensor_y(sensor_cnt) - beacon_y(sensor_cnt))
    compute total_diff = diff_x + diff_y
    move total_diff to diff_md(sensor_cnt)
  end-perform

*>   move -5 to limit_start
*>   move 25 to limit_end
*>   move 0 to limit_start
*>   move 20 to limit_end
  move 0 to limit_start
  move 4000000 to limit_end

  *> Check the specific lines within the specific ranges!
  *> perform varying line_idx from 10 by 1 until line_idx > 10
  perform varying line_idx from limit_start by 1 until line_idx > limit_end
    *> display space
    *> display ">>>>> LINE " line_idx

    initialize ranges
    move 0 to row_start
    move 0 to row_end
    *> Compute and store final ranges.
    perform varying sensor_idx from 1 by 1 until sensor_idx > sensor_cnt
      *> display space
    *>   display "SENSOR (" sensor_x(sensor_idx) ", " sensor_y(sensor_idx) ") BEACON (" beacon_x(sensor_idx) ", " beacon_y(sensor_idx) ") MD " diff_md(sensor_idx)
      compute line_diff = function abs(sensor_y(sensor_idx) - line_idx)

      *> If the interesting line is within the sensor range.
      if line_diff <= diff_md(sensor_idx)
        compute line_diff_x = diff_md(sensor_idx) - line_diff
        compute row_start = sensor_x(sensor_idx) - line_diff_x
        compute row_end = sensor_x(sensor_idx) + line_diff_x

        *> Keep things within limits.
        if row_start < limit_start
          move limit_start to row_start
        end-if
        if row_end > limit_end
          move limit_end to row_end
        end-if

        *> Add the appropriate range for the sensor to the range list.
        move 0 to range_found
        perform varying range_idx from 1 by 1 until range_idx > range_cnt or range_found = 1
          *> display "CHECKING: " row_start " to " row_end " against " range_start(range_idx) " to " range_end(range_idx)
          if row_start < range_start(range_idx) and row_end >= range_start(range_idx)
            move row_start to range_start(range_idx)
            move 1 to range_use(range_idx)
            if row_end > range_end(range_idx)
              move row_end to range_end(range_idx)
            end-if
            move 1 to range_found
            *> display "EDIT RANGE TO: " range_start(range_idx) range_end(range_idx)
          end-if
          if row_start <= range_end(range_idx) and row_end > range_end(range_idx)
            move row_end to range_end(range_idx)
            move 1 to range_use(range_idx)
            if row_start < range_start(range_idx)
              move row_start to range_start(range_idx)
            end-if
            move 1 to range_found
            *> display "EDIT RANGE TO: " range_start(range_idx) range_end(range_idx)
          end-if
          if range_found = 0 and row_start >= range_start(range_idx) and row_end <= range_end(range_idx)
            move 1 to range_found
            *> display "OVERLAP: " range_start(range_idx) range_end(range_idx)
          end-if
        end-perform

        if range_found = 0
          add 1 to range_cnt
          move row_start to range_start(range_cnt)
          move row_end to range_end(range_cnt)
          move 1 to range_use(range_idx)
          *> display "ADD RANGE: " range_start(range_cnt) range_end(range_cnt)
        end-if

        *> perform varying range_idx from 1 by 1 until range_idx > range_cnt
        *>   display "RANGE: " range_start(range_idx) " to " range_end(range_idx) " use " range_use(range_idx)
        *> end-perform

      end-if
    end-perform

    *> Combine overlapping ranges.
    move range_cnt to num_nonoverlap
    perform varying range_idx from num_nonoverlap by -1 until range_idx < 1
      *> display "OCHECK: " range_start(range_idx) " to " range_end(range_idx) " use " range_use(range_idx)
      move 0 to range_found
      compute range_idx2 = range_idx - 1
      perform until range_idx2 < 1
        *> display "OCHECK: " range_start(range_idx) " to " range_end(range_idx) " vs " range_start(range_idx2) " to " range_end(range_idx2)
        if range_use(range_idx) = 1 and range_use(range_idx2) = 1
          if range_start(range_idx) < range_start(range_idx2) and range_end(range_idx) >= (range_start(range_idx2) - 1)
            move range_start(range_idx) to range_start(range_idx2)
            if range_end(range_idx) > range_end(range_idx2)
              move range_end(range_idx) to range_end(range_idx2)
            end-if
            *> This will "remove" the last element, even if the last element has nothing to do with this interation.
            *> subtract 1 from range_cnt
            move 0 to range_use(range_idx)
            move 1 to range_found
            subtract 1 from num_nonoverlap
            *> display "EDIT LEFT RANGE TO: " range_start(range_idx2) range_end(range_idx2)
          end-if
          if range_start(range_idx) <= (range_end(range_idx2) + 1) and range_end(range_idx) > range_end(range_idx2)
            move range_end(range_idx) to range_end(range_idx2)
            if range_start(range_idx) < range_start(range_idx2)
              move range_start(range_idx) to range_start(range_idx2)
            end-if
            *> subtract 1 from range_cnt
            move 0 to range_use(range_idx)
            move 1 to range_found
            subtract 1 from num_nonoverlap
            *> display "EDIT RIGHT RANGE TO: " range_start(range_idx2) range_end(range_idx2)
          end-if

          if range_found = 0 and range_start(range_idx) >= range_start(range_idx2) and range_end(range_idx) <= range_end(range_idx2)
            *> subtract 1 from range_cnt
            move 0 to range_use(range_idx)
            move 1 to range_found
            subtract 1 from num_nonoverlap
            *> display "OVERLAP: " range_start(range_idx) range_end(range_idx)
          end-if
        end-if
        subtract 1 from range_idx2
      end-perform
    end-perform

    if num_nonoverlap > 1
      *> display "FOUND: " line_idx
      perform varying range_idx from 1 by 1 until range_idx > range_cnt
        if range_use(range_idx) = 1
          display "COMBINED RANGE: [" line_idx "][" range_idx "] " range_start(range_idx) " to " range_end(range_idx)
          *> compute total_found = total_found + (range_end(range_idx) - range_start(range_idx))
          if total_found = 0 compute total_found = ((range_end(range_idx) + 1) * 4000000) + line_idx end-if
        end-if
      end-perform
    end-if
  end-perform

  display "FINAL: " total_found

  goback.
