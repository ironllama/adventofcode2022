           >>source format free
identification division.
program-id. 16a.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_cnt pic s9(8) comp value 0.
      02 rf_row pic x(9999) occurs 0 to 9999 times
          depending on rf_cnt indexed by rf_idx.

    01 valves.
      02 starting_valve usage is index.
      02 valves_num pic 9(8) comp.
      02 valve occurs 62 times indexed by valve_idx.
        03 valve_name pic x(2).
        03 valve_flowrate pic 9(2).
        03 valve_neighbors_num pic 9.
        03 valve_neighbors occurs 5 times indexed by valve_neighbors_idx.
          04 valve_neighbor_name pic x(2).
          04 valve_neighbor_ptr usage is index.

    01 distances.
      02 distance_num pic 9(8) comp.  *> Larger size for lib-dikjstra.
      02 distances occurs 99 times indexed by active_valves_idx.
        03 distance_from usage is index.
        03 distance_to_num pic 9(2) comp.
        03 distance_to_targets occurs 99 times indexed by distance_to_idx.
          04 distance_to usage is index.
          04 distance_amt pic 9(2) comp.
    77 curr_active_valves_idx usage is index.

    01 temp_name pic x(2).
    01 temp_flowrate pic 9(2).
    01 temp_misc pic x(99).
    01 temp_done pic 9.
    01 temp_neighbors pic x(99).
    01 temp_neighbor pic x(2).
    01 temp_idx usage is index.
    01 temp_ptr usage is index.

    01 stack_checkout_options.
    02 stack_minute pic 9(2) comp.
    02 stack_num pic 9(4) comp.
    02 stack occurs 99 times.
      03 check_minute pic 9(2) comp.
      03 check_idx usage is index.
      03 check_score pic 9(8) comp.
      03 check_proj_score pic 9(8) comp.
      03 valves_visited_num pic 9(8) comp.
      03 valves_visited occurs 99 times indexed by valves_visited_idx.
        04 valves_visited_ptr usage is index.
        04 valves_visited_time pic 9(2) comp.
      03 valves_amt_venting pic 9(8) comp.
      *> 03 time_since_valve_on pic 9(2) comp.

    *> Convenience for pulling one item off the above stack.
    01 curr_valve.
      02 curr_minute pic 9(2) comp.
      02 curr_idx usage is index.
      02 curr_score pic 9(8) comp.
      02 curr_proj_score pic 9(8) comp.
      02 curr_valves_visited_num pic 9(8) comp.
      02 curr_valves_visited occurs 99 times indexed by curr_valves_visited_idx.
        03 curr_valves_visited_ptr usage is index.
        03 curr_valves_visited_time pic 9(2) comp.
      02 curr_valves_amt_venting pic 9(8) comp.
      *> 02 curr_time_since_valve_on pic 9(2) comp.

    77 check_if_valve_on_ptr usage is index.
    01 check_valves_visited_found pic x value 'N'.
      88 check_valve_on value 'Y'.
      88 check_valve_off value 'N'.

    01 add_to_stack_ind pic x value 'N'.
      88 add_to_stack value 'Y'.
      88 do_not_add_to_stack value 'N'.

    *> For lib-dijkstra.
    01 get_neighbors procedure-pointer.
    01 get_neighbors_stuff.
      02 current_ptr usage is index.
      02 curr_neighbors_num pic s9 comp.
      02 curr_neighbors occurs 5 times indexed by curr_neighbors_idx.
        03 curr_neighbor_ptr usage is index.
        03 curr_neighbor_dist pic s9.
    01 path.
      02 path_len pic s9(8) comp value 0.
      02 path_val usage is index occurs 0 to 99999 times
          depending on path_len indexed by path_idx.

    77 best_minute pic 9(2) comp.
    77 best_score pic 9(8) comp.


procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
*>   call 'lib-readdata' using function module-id ".da1" rf_all_lines

  move 0 to valves_num

  move rf_cnt to valves_num
  perform varying rf_idx from 1 by 1 until rf_idx > rf_cnt
    *> display "LINE: " function trim(rf_row(rf_idx))

    *> Get name of valve.
    move rf_row(rf_idx)(7:2) to valve_name(rf_idx)

    *> Note index if this is the starting valve.
    if valve_name(rf_idx) = "AA" move rf_idx to starting_valve end-if

    *> Get flowrate of valve.
    move rf_row(rf_idx)(24:2) to temp_flowrate
    if temp_flowrate(2:) = ";"
      move temp_flowrate(1:1) to valve_flowrate(rf_idx)
    else
      move temp_flowrate to valve_flowrate(rf_idx)
    end-if

    *> *> Keep track of all valves with positive flowrates, including the starting point AA.
    if valve_flowrate(rf_idx) > 0 or valve_name(rf_idx) = "AA"
      add 1 to distance_num
      move rf_idx to distance_from(distance_num)
    end-if

    *> Get all the neighbors of the valve.
    unstring function trim(rf_row(rf_idx)) delimited by "ves " or "valve "
      into temp_misc temp_neighbors
    end-unstring
    *> If it's a list, process. Otherwise, just assign the single valve.
    if temp_neighbors(3:1) = ","
      move 0 to temp_done
      set temp_ptr to 1
      perform until temp_done = 1
        *> move spaces to temp_neighbor  *> DANGER: This removes the last element. Needs +1 iteration to write last element.
        unstring function trim(temp_neighbors) delimited by ", "
          into temp_neighbor
          with pointer temp_ptr
          on overflow
            if temp_ptr > length of function trim(temp_neighbors)
              move 1 to temp_done
            end-if
            add 1 to valve_neighbors_num(rf_idx)
            move temp_neighbor to valve_neighbor_name(rf_idx valve_neighbors_num(rf_idx))
        end-unstring
      end-perform
    else
      add 1 to valve_neighbors_num(rf_idx)
      move function trim(temp_neighbors) to valve_neighbor_name(rf_idx valve_neighbors_num(rf_idx))
    end-if

    *> Display the processed lines.
    *> display "NEW: " valve_name(rf_idx) " " valve_flowrate(rf_idx) " NEIGHBORS: " no advancing
    *> perform varying valve_neighbors_idx from 1 by 1 until valve_neighbors_idx > valve_neighbors_num(rf_idx)
    *>   display valve_neighbor_name(rf_idx valve_neighbors_idx) no advancing
    *>   if valve_neighbors_idx < valve_neighbors_num(rf_idx) display ", " no advancing end-if
    *> end-perform
    *> display space
  end-perform

  *> Translate all the valve names to idx for easier use.
  perform varying valve_idx from 1 by 1 until valve_idx > valves_num
    *> display "VALVE: [" valve_idx " of " valves_num "] " valve_name(valve_idx) " " valve_flowrate(valve_idx) " NEIGHBORS: " no advancing
    perform varying valve_neighbors_idx from 1 by 1 until valve_neighbors_idx > valve_neighbors_num(valve_idx)
      move valve_neighbor_name(valve_idx valve_neighbors_idx) to temp_name
      perform get_idx_for_name
      if temp_done = 1
        move temp_idx to valve_neighbor_ptr(valve_idx valve_neighbors_idx)
        *> set valve_neighbor_ptr(valve_idx valve_neighbors_idx) to temp_ptr
        *> display temp_name ":" temp_idx no advancing
        *> if valve_neighbors_idx < valve_neighbors_num(valve_idx) display ", " no advancing end-if
      else
        display "TRANSLATE: VALVE " temp_name " NOT FOUND!"
      end-if
    end-perform
    *> display spaces
  end-perform


  *> display "NUM ACTIVE VALVES: " distance_num
  *> Get all the shortest distances as a table, using Dijkstra!
  perform varying active_valves_idx from 1 by 1 until active_valves_idx > distance_num
    perform varying curr_active_valves_idx from 1 by 1 until curr_active_valves_idx > distance_num
      if active_valves_idx <> curr_active_valves_idx
          and valve_name(distance_from(curr_active_valves_idx)) <> 'AA'  *> Skip 'AA' as a neighbor.
        initialize path
        set get_neighbors to entry "get_neighbors"
        *> call 'lib-dijkstra' using active_valves_idx curr_active_valves_idx distance_num path get_neighbors get_neighbors_stuff
        call 'lib-dijkstra' using distance_from(active_valves_idx) distance_from(curr_active_valves_idx) valves_num path get_neighbors get_neighbors_stuff

        add 1 to distance_to_num(active_valves_idx)
        move distance_from(curr_active_valves_idx) to distance_to(active_valves_idx distance_to_num(active_valves_idx))
        move path_len to distance_amt(active_valves_idx distance_to_num(active_valves_idx))

        *> display "DISTANCE: " valve_name(distance_from(active_valves_idx)) " to " valve_name(distance_from(curr_active_valves_idx)) ": " path_len " " no advancing
        *> perform varying path_idx from 1 by 1 until path_idx > path_len
        *>   display valve_name(path_val(path_idx)) " -> " no advancing
        *> end-perform
        *> display space
      end-if
    end-perform
  end-perform


  add 1 to stack_num
  move starting_valve to check_idx(stack_num)
  move 30 to check_minute(stack_num)

  perform checkout_options until stack_num < 1

  display "BEST: " best_score

  goback.


checkout_options.

  move stack(stack_num) to curr_valve
  subtract 1 from stack_num

  set add_to_stack to true

  *> display "CHECKOUT: [" curr_minute "] " valve_name(curr_idx) "(" curr_idx ") VALVES ON: " curr_valves_visited_num " AMT VENTING: " curr_valves_amt_venting " SCORE: " curr_score

  if add_to_stack and curr_minute > 1
    *> If it has a positive flowrate and is not yet on, turn it on.
    *> Be careful about changing curr_stack in this section, as it may need to stay intact for neighbors check.
    if add_to_stack and valve_flowrate(curr_idx) > 0
      move curr_idx to check_if_valve_on_ptr
      perform check_if_valve_on

      if check_valve_off
        add 1 to curr_valves_visited_num
        move curr_idx to curr_valves_visited_ptr(curr_valves_visited_num)
        move curr_minute to curr_valves_visited_time(curr_valves_visited_num)


        add valve_flowrate(curr_idx) to curr_valves_amt_venting
        *> move 1 to curr_time_since_valve_on
        compute curr_minute = curr_minute - 1
        compute curr_score = curr_score + curr_valves_amt_venting
        compute curr_proj_score = curr_score + (curr_valves_amt_venting * (curr_minute - 1))

        *> If all valves have been turned on, just fast foward without movement.
        *> if curr_valves_visited_num = (distance_num - 1)
        *>   move curr_proj_score to curr_score
        *>   move 1 to curr_minute

        *>   set do_not_add_to_stack to true
        *> end-if

        if curr_proj_score >= best_score
          move curr_proj_score to best_score
          move curr_minute to best_minute

          *> display "VALVES BEST: [" curr_valves_visited_num "]: SCORE: " curr_proj_score " " no advancing
          *> perform varying curr_valves_visited_idx from 1 by 1 until curr_valves_visited_idx > curr_valves_visited_num
          *>   display valve_name(curr_valves_visited_ptr(curr_valves_visited_idx)) ": " curr_valves_visited_time(curr_valves_visited_idx) ", " no advancing
          *> end-perform
          *> display space
        end-if
      end-if
    end-if


    if add_to_stack
      perform get_distance_from_idx
      if curr_active_valves_idx > 0
        *> display "GET DISTANCE: " valve_name(distance_from(curr_active_valves_idx)) " NEIGHBORS: " distance_to_num(curr_active_valves_idx)
        perform varying distance_to_idx from 1 by 1 until distance_to_idx > distance_to_num(curr_active_valves_idx)
              *> or curr_valves_visited_num = distance_num
          move distance_to(curr_active_valves_idx distance_to_idx) to check_if_valve_on_ptr
          perform check_if_valve_on

          if check_valve_off
              and distance_to(curr_active_valves_idx distance_to_idx) <> starting_valve  *> Skip 'AA'
              and distance_amt(curr_active_valves_idx distance_to_idx) < curr_minute
            add 1 to stack_num on size error display ">>>>> STACK OVERFLOW! <<<<<" end-add
            move curr_valve to stack(stack_num)

            move distance_to(curr_active_valves_idx distance_to_idx) to check_idx(stack_num)
            compute check_minute(stack_num) = curr_minute - distance_amt(curr_active_valves_idx distance_to_idx)
            compute check_score(stack_num) = curr_score + (curr_valves_amt_venting * distance_amt(curr_active_valves_idx distance_to_idx))
            *> add distance_amt(curr_active_valves_idx distance_to_idx) to time_since_valve_on(stack_num)

            compute check_proj_score(stack_num) = check_score(stack_num) + (curr_valves_amt_venting * (check_minute(stack_num) - 1))

            *> display "ADDED NEIGHBOR: [" check_minute(stack_num) "] " valve_name(check_idx(stack_num)) " " check_score(stack_num) " " valve_neighbors_idx
            *> display "STACK SIZE: " stack_num
          end-if
        end-perform
      end-if
    end-if

  end-if
  .


get_idx_for_name.
  move 0 to temp_done
  perform varying temp_idx from 1 by 1 until temp_idx > valves_num or temp_done = 1
    if valve_name(temp_idx) = temp_name
      move 1 to temp_done
    end-if
  end-perform
  *> Because of loop iter/check...
  subtract 1 from temp_idx
  if temp_done = 0 display "ERROR: NOT FOUND!" end-if
  .

check_if_valve_on.
  set check_valve_off to true
  perform varying curr_valves_visited_idx from 1 by 1 until curr_valves_visited_idx > curr_valves_visited_num
    if curr_valves_visited_ptr(curr_valves_visited_idx) = check_if_valve_on_ptr
      set check_valve_on to true
    end-if
  end-perform
  .

get_distance_from_idx.
  set curr_active_valves_idx to 0
  perform varying active_valves_idx from 1 by 1
      until active_valves_idx > distance_num or curr_active_valves_idx > 0
    if distance_from(active_valves_idx) = curr_idx
      set curr_active_valves_idx to active_valves_idx
    end-if
  end-perform
  .

*> For lib-dijkstra.
entry "get_neighbors"
  set curr_neighbors_num to 0

  perform varying valve_neighbors_idx from 1 by 1 until valve_neighbors_idx > valve_neighbors_num(current_ptr)
    *> display "GET NEIGHBORS FOR: [" valve_neighbors_idx " of " valve_neighbors_num(current_ptr) "] " valve_name(current_ptr) " -> " valve_name(valve_neighbor_ptr(current_ptr valve_neighbors_idx))
    add 1 to curr_neighbors_num
    move valve_neighbor_ptr(current_ptr valve_neighbors_idx) to curr_neighbor_ptr(curr_neighbors_num)
    move 1 to curr_neighbor_dist(curr_neighbors_num)
  end-perform
  .
