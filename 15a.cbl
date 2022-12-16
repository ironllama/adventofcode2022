           >>source format free
identification division.
program-id. 15a.

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
    01 ranges.
      02 range_cnt pic s9(2) comp value 0.
      02 range_row occurs 0 to 99 times
          depending on range_cnt indexed by range_idx.
        03 range_start pic s9(8) comp.
        03 range_end pic s9(8) comp.
    77 range_found pic 9.
    77 range_idx2 pic s9(4) comp.

    77 line_beacon_y pic s9(8) comp value 0.
    *> 77 line_beacon_x pic s9(8) comp value 0.

    77 diff_x pic s9(8) comp.
    77 diff_y pic s9(8) comp.
    77 total_diff pic s9(8) comp.
    77 line_diff pic s9(8) comp.
    77 line_diff_x pic s9(8) comp.
    77 row_start pic s9(8) comp.
    77 row_end pic s9(8) comp.

    77 total_found pic s9(8) comp.

procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
  move 2000000 to line_beacon_y
*>   call 'lib-readdata' using function module-id ".da1" rf_all_lines
*> move 10 to line_beacon_y

  *> First, get all the sensors.
  perform varying rf_idx from 1 by 1 until rf_idx > rf_cnt
    *> display "LINE: " function trim(rf_row(rf_idx))
    *> display "CHECKING: " rf_row(rf_idx)(curr_line_len - sensor_str_len + 1:sensor_str_len)
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

    *> Get the beacon_x for the beacon_y, but should only appear once!
    *> Red herring. Irrelevant.
    *> if line_beacon_y = beacon_y(sensor_cnt)
    *>   if line_beacon_x = 0
    *>     move beacon_x(sensor_cnt) to line_beacon_x
    *>     *> display "BEACON: " line_beacon_x ", " line_beacon_y
    *>   end-if
    *>   if line_beacon_x <> beacon_x(sensor_cnt)
    *>     *> display ">>> ERROR: DIFFERENT BEACON ON SAME LINE!"
    *>   end-if
    *> end-if
  end-perform

  *> Compute and store final ranges.
  perform varying sensor_idx from 1 by 1 until sensor_idx > sensor_cnt
    *> display space
    *> display "SENSOR (" sensor_x(sensor_idx) ", " sensor_y(sensor_idx) ") BEACON (" beacon_x(sensor_idx) ", " beacon_y(sensor_idx) ")"
    compute line_diff = function abs(sensor_y(sensor_idx) - line_beacon_y)

    compute diff_x = function abs(sensor_x(sensor_idx) - beacon_x(sensor_idx))
    compute diff_y = function abs(sensor_y(sensor_idx) - beacon_y(sensor_idx))
    compute total_diff = diff_x + diff_y
    *> display "TOTAL_DIFF: " total_diff " LINE_DIFF: " line_diff

    *> If the interesting line is within the sensor range.
    if line_diff <= total_diff
      compute line_diff_x = total_diff - line_diff
      compute row_start = sensor_x(sensor_idx) - line_diff_x
      compute row_end = sensor_x(sensor_idx) + line_diff_x

      *> Add the appropriate range for the sensor to the range list.
      move 0 to range_found
      perform varying range_idx from 1 by 1 until range_idx > range_cnt or range_found = 1
        *> display "CHECKING: " row_start " to " row_end " against " range_start(range_idx) " to " range_end(range_idx)
        if row_start < range_start(range_idx) and row_end >= range_start(range_idx)
          move row_start to range_start(range_idx)
          if row_end > range_end(range_idx)
            move row_end to range_end(range_idx)
          end-if
          move 1 to range_found
          *> display "EDIT RANGE TO: " range_start(range_idx) range_end(range_idx)
        end-if
        if row_start <= range_end(range_idx) and row_end > range_end(range_idx)
          move row_end to range_end(range_idx)
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
        *> display "ADD RANGE: " range_start(range_cnt) range_end(range_cnt)
      end-if

      *> perform varying range_idx from 1 by 1 until range_idx > range_cnt
      *>   display "RANGE: " range_start(range_idx) " to " range_end(range_idx)
      *> end-perform

    end-if
  end-perform

  *> Combine overlapping ranges.
  perform varying range_idx from range_cnt by -1 until range_idx < 1
    *> display "OCHECK: " range_start(range_idx) " to " range_end(range_idx)
    move 0 to range_found
    compute range_idx2 = range_idx - 1
    perform until range_idx2 < 1
      *> display "OCHECK: " range_start(range_idx) " to " range_end(range_idx) " vs " range_start(range_idx2) " to " range_end(range_idx2)
      if range_start(range_idx) < range_start(range_idx2) and range_end(range_idx) >= range_start(range_idx2)
        move range_start(range_idx) to range_start(range_idx2)
        if range_end(range_idx) > range_end(range_idx2)
          move range_end(range_idx) to range_end(range_idx2)
        end-if
        subtract 1 from range_cnt
        move 1 to range_found
        *> display "EDIT LEFT RANGE TO: " range_start(range_idx2) range_end(range_idx2)
      end-if
      if range_start(range_idx) <= range_end(range_idx2) and range_end(range_idx) > range_end(range_idx2)
        move range_end(range_idx) to range_end(range_idx2)
        if range_start(range_idx) < range_start(range_idx2)
          move range_start(range_idx) to range_start(range_idx2)
        end-if
        subtract 1 from range_cnt
        move 1 to range_found
        *> display "EDIT RIGHT RANGE TO: " range_start(range_idx2) range_end(range_idx2)
      end-if

      if range_found = 0 and range_start(range_idx) >= range_start(range_idx2) and range_end(range_idx) <= range_end(range_idx2)
        subtract 1 from range_cnt
        move 1 to range_found
        *> display "OVERLAP: " range_start(range_idx) range_end(range_idx)
      end-if

      subtract 1 from range_idx2
    end-perform
  end-perform

  perform varying range_idx from 1 by 1 until range_idx > range_cnt
    display "COMBINED RANGE: " range_start(range_idx) " to " range_end(range_idx)
    compute total_found = total_found + (range_end(range_idx) - range_start(range_idx))
  end-perform

  display "FINAL: " total_found

  goback.
