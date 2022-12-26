           >>source format free
identification division.
program-id. 18b.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_cnt pic s9(8) comp value 0.
      02 rf_row pic x(9999) occurs 0 to 3000 times
          depending on rf_cnt indexed by rf_idx.

    01 all_cubes.
      02 cube_cnt pic 9(8) comp value 0.
      02 cube occurs 0 to 3000 times
          depending on cube_cnt indexed by cube_idx.
        03 cube_x pic 9(2) comp.
        03 cube_y pic 9(2) comp.
        03 cube_z pic 9(2) comp.
    77 temp_idx usage is index.
    01 num_sides pic 9.

    01 is_neighbor pic 9.

    77 total_found pic s9(8) comp.

procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
  *> call 'lib-readdata' using function module-id ".da1" rf_all_lines
  *> move "1,1,1" to rf_row(1)
  *> move "2,1,1" to rf_row(2)
  *> move 2 to rf_cnt

  move 0 to total_found

  *> Populate the cubes.
  perform varying rf_idx from 1 by 1 until rf_idx > rf_cnt
    *> display "LINE: " function trim(rf_row(rf_idx))
    add 1 to cube_cnt
    unstring function trim(rf_row(rf_idx)) delimited by ","
      into cube_x(cube_cnt) cube_y(cube_cnt) cube_z(cube_cnt)
    end-unstring
  end-perform
  *> perform varying cube_idx from 1 by 1 until cube_idx > cube_cnt
  *>   display "CUBE: " cube_x(cube_idx) " " cube_y(cube_idx) " " cube_z(cube_idx)
  *> end-perform

  perform varying cube_idx from 1 by 1 until cube_idx > cube_cnt
    *> display "LINE: " function trim(rf_row(rf_idx))
    move 6 to num_sides
    set temp_idx to 1
    perform varying temp_idx from 1 by 1 until temp_idx > cube_cnt
      *> If not the same cube
      if temp_idx <> cube_idx
        perform check_neighbor
        if is_neighbor = 1
          subtract 1 from num_sides
      end-if
    end-perform
    compute total_found = total_found + num_sides
  end-perform

  display "FINAL: " total_found

  goback.

check_neighbor.
  *> display "CHECKING NEIGHBOR: " cube_idx " " temp_idx
  move 0 to is_neighbor
  if cube_x(cube_idx) = cube_x(temp_idx)
    if cube_y(cube_idx) = cube_y(temp_idx)
      if cube_z(cube_idx) = cube_z(temp_idx) + 1
          or cube_z(cube_idx) = cube_z(temp_idx) - 1
        move 1 to is_neighbor
      end-if
    else
      if cube_z(cube_idx) = cube_z(temp_idx)
          and (cube_y(cube_idx) = cube_y(temp_idx) + 1
            or cube_y(cube_idx) = cube_y(temp_idx) - 1)
          move 1 to is_neighbor
      end-if
    end-if
  else
    if cube_y(cube_idx) = cube_y(temp_idx)
        and cube_z(cube_idx) = cube_z(temp_idx)
        and (cube_x(cube_idx) = cube_x(temp_idx) + 1
          or cube_x(cube_idx) = cube_x(temp_idx) - 1)
      move 1 to is_neighbor
    end-if
  end-if
  *> *> If this is a neighbor of
  *> if cube_x(cube_idx) = cube_x(temp_idx) + 1
    *> subtract 1 from num_sides
    *> display "SHARED SIDE X + 1"
  *> end-if
  *> if cube_x(cube_idx) = cube_x(temp_idx) - 1
    *> subtract 1 from num_sides
    *> display "SHARED SIDE X - 1"
  *> end-if
  *> if cube_y(cube_idx) = cube_y(temp_idx) + 1
    *> subtract 1 from num_sides
    *> display "SHARED SIDE Y + 1"
  *> end-if
  *> if cube_y(cube_idx) = cube_y(temp_idx) - 1
    *> subtract 1 from num_sides
    *> display "SHARED SIDE Y - 1"
  *> end-if
  *> if cube_z(cube_idx) = cube_z(temp_idx) + 1
    *> subtract 1 from num_sides
    *> display "SHARED SIDE Z + 1"
  *> end-if
  *> if cube_z(cube_idx) = cube_z(temp_idx) - 1
    *> subtract 1 from num_sides
    *> display "SHARED SIDE Z - 1"
  *> end-if
  .
