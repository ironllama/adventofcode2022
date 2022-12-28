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
      02 cube_found_ind pic x value 'N'.
        88 cube_found value 'Y'.
        88 cube_not_found value 'N'.
      02 cube_cnt pic 9(8) comp value 0.
      02 cube occurs 3000 times indexed by cube_idx.
        03 cube_x pic s9(2) comp.
        03 cube_y pic s9(2) comp.
        03 cube_z pic s9(2) comp.
    77 temp_idx usage is index.
    01 num_sides pic 9.

    01 is_neighbor_ind pic x value 'N'.
      88 is_neighbor value 'Y'.
      88 is_not_neighbor value 'N'.

    01 all_limits.
      02 low_z pic s9(2) comp value 99.
      02 high_z pic s9(2) comp value 0.
      02 low_y pic s9(2) comp value 99.
      02 high_y pic s9(2) comp value 0.
      02 low_x pic s9(2) comp value 99.
      02 high_x pic s9(2) comp value 0.
    01 z_idx pic s9(8) comp.
    01 y_idx pic s9(8) comp.
    01 x_idx pic s9(8) comp.

    *> Since we visit all the squares only once (checked before adding to queue),
    *> we don't need a separate visited list.
    *> 01 all_explored.
    *>   02 explored_cnt pic 9(8) comp.
    *>   02 explored_exists pic x value 'N'.
    *>     88 explored_not_found value 'N'.
    *>     88 explored_found value 'Y'.
    *>   02 explored occurs 9999 times indexed by explored_idx.
    *>     03 explored_x pic s9(2) comp.
    *>     03 explored_y pic s9(2) comp.
    *>     03 explored_z pic s9(2) comp.

    01 go_queue_stuff.
      02 go_queue_cnt pic 9(8) comp.
      02 go_queue_head pic 9(8) comp.
      02 go_queue_exists pic x value 'N'.
        88 go_queue_not_found value 'N'.
        88 go_queue_found value 'Y'.
      02 go_queue occurs 9999 times indexed by go_queue_idx.
        03 go_queue_x pic s9(2) comp.
        03 go_queue_y pic s9(2) comp.
        03 go_queue_z pic s9(2) comp.

    01 curr_node.
      02 curr_x pic s9(2) comp.
      02 curr_y pic s9(2) comp.
      02 curr_z pic s9(2) comp.

    77 total_found_1 pic s9(8) comp.
    77 total_found_2 pic s9(8) comp.
    77 total_found pic s9(8) comp.

procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
*>   call 'lib-readdata' using function module-id ".da1" rf_all_lines
  *> move "1,1,1" to rf_row(1)
  *> move "2,1,1" to rf_row(2)
  *> move 2 to rf_cnt

  move 0 to total_found_1

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

  *> Part 1.
  perform varying cube_idx from 1 by 1 until cube_idx > cube_cnt
    *> display "LINE: " function trim(rf_row(rf_idx))
    move 6 to num_sides

    perform varying temp_idx from 1 by 1 until temp_idx > cube_cnt
      *> If not the same cube
      if temp_idx <> cube_idx
        perform check_neighbor
        if is_neighbor
          subtract 1 from num_sides
      end-if
    end-perform

    compute total_found_1 = total_found_1 + num_sides
    perform check_limits

  end-perform

  *> display "PART 1: " total_found_1 " X: " low_x "-" high_x " Y: " low_y "-" high_y " Z: " low_z "-" high_z
  *> perform display_z_axis


  subtract 1 from low_x
  subtract 1 from low_y
  subtract 1 from low_z
  add 1 to high_x
  add 1 to high_y
  add 1 to high_z

  compute curr_x = low_x
  compute curr_y = low_y
  compute curr_z = low_z

  perform bfs_against_outside

  *> display "EXPLORED: "
  *> perform varying explored_idx from 1 by 1 until explored_idx > explored_cnt
  *>   display "X: " explored_x(explored_idx) " Y: " explored_y(explored_idx) " Z: " explored_z(explored_idx)
  *> end-perform

  *> display "AFTER BFS: "
  *> perform display_z_axis

  *> Get surface area of inside of droplet by eliminating obsidian cubes and outer "air" cubes via bfs.
  move 0 to total_found_2
  perform varying cube_idx from 1 by 1 until cube_idx > cube_cnt
    *> display "LINE: " function trim(rf_row(rf_idx))
    move 6 to num_sides

    *> If not next to any other obsidian cube.
    perform varying temp_idx from 1 by 1 until temp_idx > cube_cnt
      *> If not the same cube
      if temp_idx <> cube_idx
        perform check_neighbor
        if is_neighbor subtract 1 from num_sides end-if
      end-if
    end-perform

    *> If not next to a BFS/air cube.
    *> perform varying temp_idx from 1 by 1 until temp_idx > explored_cnt
    *>   perform check_explored
    perform varying temp_idx from 1 by 1 until temp_idx > go_queue_cnt
      perform check_queue
      if is_neighbor subtract 1 from num_sides end-if
    end-perform

    compute total_found_2 = total_found_2 + num_sides

  end-perform

  display "1: " total_found_1 " 2: " total_found_2

  compute total_found = total_found_1 - total_found_2
  display "FINISHED: " total_found

  goback.


bfs_against_outside.
  *> add 1 to explored_cnt
  *> move curr_node to explored(explored_cnt)

  add 1 to go_queue_cnt
  move curr_node to go_queue(go_queue_cnt)

  perform until go_queue_cnt < 1 or go_queue_head > go_queue_cnt
    add 1 to go_queue_head
    *> display "X: " go_queue_x(go_queue_head) " Y: " go_queue_y(go_queue_head) " Z: " go_queue_z(go_queue_head) " GO_QUEUE_CNT: " go_queue_cnt " EXPLORED_CNT: " explored_cnt

    *> If this is not the goal...
    *> Commented out so that it doesn't finish early, but completes all possibilities.
    *> if go_queue_x(go_queue_head) <> high_x
    *>     and go_queue_y(go_queue_head) <> high_y
    *>     and go_queue_z(go_queue_head) <> high_z

      *> For each neighbor...
      if go_queue_x(go_queue_head) > low_x and go_queue_x(go_queue_head) <= high_x
        compute curr_x = go_queue_x(go_queue_head) - 1
        move go_queue_y(go_queue_head) to curr_y
        move go_queue_z(go_queue_head) to curr_z
        perform add_to_queue
      end-if
      if go_queue_x(go_queue_head) < high_x and go_queue_x(go_queue_head) >= low_x
        compute curr_x = go_queue_x(go_queue_head) + 1
        move go_queue_y(go_queue_head) to curr_y
        move go_queue_z(go_queue_head) to curr_z
        perform add_to_queue
      end-if
      if go_queue_y(go_queue_head) > low_y and go_queue_y(go_queue_head) <= high_y
        move go_queue_x(go_queue_head) to curr_x
        compute curr_y = go_queue_y(go_queue_head) - 1
        move go_queue_z(go_queue_head) to curr_z
        perform add_to_queue
      end-if
      if go_queue_y(go_queue_head) < high_y and go_queue_y(go_queue_head) >= low_y
        move go_queue_x(go_queue_head) to curr_x
        compute curr_y = go_queue_y(go_queue_head) + 1
        move go_queue_z(go_queue_head) to curr_z
        perform add_to_queue
      end-if
      if go_queue_z(go_queue_head) > low_z and go_queue_z(go_queue_head) <= high_z
        move go_queue_x(go_queue_head) to curr_x
        move go_queue_y(go_queue_head) to curr_y
        compute curr_z = go_queue_z(go_queue_head) - 1
        perform add_to_queue
      end-if
      if go_queue_z(go_queue_head) < high_z and go_queue_z(go_queue_head) >= low_z
        move go_queue_x(go_queue_head) to curr_x
        move go_queue_y(go_queue_head) to curr_y
        compute curr_z = go_queue_z(go_queue_head) + 1
        perform add_to_queue
      end-if
    *> end-if
  end-perform
  .

add_to_queue.
  *> perform check_exists_in_explored
  perform check_exists_in_queue

  perform check_exists_in_cubes

  *> if explored_not_found and cube_not_found
  if go_queue_not_found and cube_not_found
    *> add 1 to explored_cnt
    *> move curr_node to explored(explored_cnt)

    add 1 to go_queue_cnt
    move curr_node to go_queue(go_queue_cnt)
  end-if
  .

*> check_exists_in_explored.
*>   set explored_not_found to true
*>   perform varying explored_idx from 1 by 1 until explored_idx > explored_cnt or explored_found
*>     if explored_x(explored_idx) = curr_x
*>         and explored_y(explored_idx) = curr_y
*>         and explored_z(explored_idx) = curr_z
*>       set explored_found to true
*>     end-if
*>   end-perform
*>   .

check_exists_in_queue.
  set go_queue_not_found to true
  perform varying go_queue_idx from 1 by 1 until go_queue_idx > go_queue_cnt or go_queue_found
    if go_queue_x(go_queue_idx) = curr_x
        and go_queue_y(go_queue_idx) = curr_y
        and go_queue_z(go_queue_idx) = curr_z
      set go_queue_found to true
    end-if
  end-perform
  .

check_exists_in_cubes.
  set cube_not_found to true
  perform varying cube_idx from 1 by 1 until cube_idx > cube_cnt or cube_found
    if cube_x(cube_idx) = curr_x
        and cube_y(cube_idx) = curr_y
        and cube_z(cube_idx) = curr_z
      set cube_found to true
    end-if
  end-perform
  .

check_queue.
  *> display "CHECKING go_queue: " cube_idx " " temp_idx
  *> move 0 to is_neighbor
  set is_not_neighbor to true
  if cube_x(cube_idx) = go_queue_x(temp_idx)
    if cube_y(cube_idx) = go_queue_y(temp_idx)
      if cube_z(cube_idx) = go_queue_z(temp_idx) + 1
          or cube_z(cube_idx) = go_queue_z(temp_idx) - 1
        *> move 1 to is_neighbor
        set is_neighbor to true
      end-if
    else
      if cube_z(cube_idx) = go_queue_z(temp_idx)
          and (cube_y(cube_idx) = go_queue_y(temp_idx) + 1
            or cube_y(cube_idx) = go_queue_y(temp_idx) - 1)
        *> move 1 to is_neighbor
        set is_neighbor to true
      end-if
    end-if
  else
    if cube_y(cube_idx) = go_queue_y(temp_idx)
        and cube_z(cube_idx) = go_queue_z(temp_idx)
        and (cube_x(cube_idx) = go_queue_x(temp_idx) + 1
          or cube_x(cube_idx) = go_queue_x(temp_idx) - 1)
      *> move 1 to is_neighbor
      set is_neighbor to true
    end-if
  end-if
  .

*> check_explored.
*>   *> display "CHECKING EXPLORED: " cube_idx " " temp_idx
*>   *> move 0 to is_neighbor
*>   set is_not_neighbor to true
*>   if cube_x(cube_idx) = explored_x(temp_idx)
*>     if cube_y(cube_idx) = explored_y(temp_idx)
*>       if cube_z(cube_idx) = explored_z(temp_idx) + 1
*>           or cube_z(cube_idx) = explored_z(temp_idx) - 1
*>         *> move 1 to is_neighbor
*>         set is_neighbor to true
*>       end-if
*>     else
*>       if cube_z(cube_idx) = explored_z(temp_idx)
*>           and (cube_y(cube_idx) = explored_y(temp_idx) + 1
*>             or cube_y(cube_idx) = explored_y(temp_idx) - 1)
*>         *> move 1 to is_neighbor
*>         set is_neighbor to true
*>       end-if
*>     end-if
*>   else
*>     if cube_y(cube_idx) = explored_y(temp_idx)
*>         and cube_z(cube_idx) = explored_z(temp_idx)
*>         and (cube_x(cube_idx) = explored_x(temp_idx) + 1
*>           or cube_x(cube_idx) = explored_x(temp_idx) - 1)
*>       *> move 1 to is_neighbor
*>       set is_neighbor to true
*>     end-if
*>   end-if
*>   .

check_neighbor.
  *> display "CHECKING NEIGHBOR: " cube_idx " " temp_idx
  *> move 0 to is_neighbor
  set is_not_neighbor to true
  if cube_x(cube_idx) = cube_x(temp_idx)
    if cube_y(cube_idx) = cube_y(temp_idx)
      if cube_z(cube_idx) = cube_z(temp_idx) + 1
          or cube_z(cube_idx) = cube_z(temp_idx) - 1
        *> move 1 to is_neighbor
        set is_neighbor to true
      end-if
    else
      if cube_z(cube_idx) = cube_z(temp_idx)
          and (cube_y(cube_idx) = cube_y(temp_idx) + 1
            or cube_y(cube_idx) = cube_y(temp_idx) - 1)
        *> move 1 to is_neighbor
        set is_neighbor to true
      end-if
    end-if
  else
    if cube_y(cube_idx) = cube_y(temp_idx)
        and cube_z(cube_idx) = cube_z(temp_idx)
        and (cube_x(cube_idx) = cube_x(temp_idx) + 1
          or cube_x(cube_idx) = cube_x(temp_idx) - 1)
      *> move 1 to is_neighbor
      set is_neighbor to true
    end-if
  end-if
  .

check_limits.
  if cube_x(cube_idx) < low_x
    move cube_x(cube_idx) to low_x
  end-if
  if cube_x(cube_idx) > high_x
    move cube_x(cube_idx) to high_x
  end-if
  if cube_y(cube_idx) < low_y
    move cube_y(cube_idx) to low_y
  end-if
  if cube_y(cube_idx) > high_y
    move cube_y(cube_idx) to high_y
  end-if
  if cube_z(cube_idx) < low_z
    move cube_z(cube_idx) to low_z
  end-if
  if cube_z(cube_idx) > high_z
    move cube_z(cube_idx) to high_z
  end-if
  .

display_z_axis.
  *> Check out the situation by slicing on the Z axis.
  perform varying z_idx from low_z by 1 until z_idx > high_z
    display "Z INDEX: [" z_idx "]"
    display "===================="
    perform varying y_idx from low_y by 1 until y_idx > high_y
      perform varying x_idx from low_x by 1 until x_idx > high_x
        move x_idx to curr_x
        move y_idx to curr_y
        move z_idx to curr_z
        perform check_exists_in_cubes
        *> perform check_exists_in_explored
        perform check_exists_in_queue

        *> if cube_found and explored_found
        if cube_found and go_queue_found
          display ">>>>> ERROR: EXISTS IN BOTH CUBES AND EXPLORED. <<<<<"
        else
          if cube_found
            display '#' no advancing
          else
            *> if explored_found
            if go_queue_found
              display "@" no advancing
            else
              display '.' no advancing
            end-if
          end-if
        end-if
      end-perform
      display space
    end-perform
    display space
  end-perform
 .
