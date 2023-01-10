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



*> GRAVEYARD OF ABANDONED CODE
*> ============================================================================



    *> 01 valves_visited_tbl.
    *>   02 valves_visited pic 9(2) occurs 62 times indexed by valves_visited_idx.

    *> 01 path_options_queue.
    *>   02 po_high_valve_ptr usage is index.
    *>   02 po_high_step_num pic 9(2) comp.
    *>   02 po_high_score pic 9(8) comp.
    *>   02 po_head usage is index.
    *>   02 po_num pic 9(2).
    *>   02 po occurs 62 times indexed by po_idx.
    *>     03 po_valve_ptr usage is index.
    *>     03 po_step_num pic 9(2) comp.
    *>     03 po_score pic 9(8) comp.
    *>   02 po_found pic 9.

    *> 01 path_chosen.
    *>   02 pc_num pic 9(2).
    *>   02 pc occurs 62 times indexed by pc_idx.
    *>     03 pc_valve_ptr pic 9(2).
    *>     03 pc_step_num pic 9(2) comp.
    *>     03 pc_score pic 9(8) comp.


        *> 03 check_valve_idx usage is index.
        *> 03 check_options_num pic 9(2) comp.
        *> 03 check_options occurs 99 times indexed by check_valve_options_idx.
        *>   04 check_option_idx usage is index.*> display_neighbors.


    *> 01 buff_neighbors_list.
    *>   02 buff_neighbors_distance pic 9 comp.
    *>   02 buff_neighbors_highest_flow usage is index.
    *>   02 buff_neighbors_num pic 9(2) comp.
    *>   02 buff_neighbors occurs 99 times indexed by buff_neighbors_idx.
    *>     03 buff_neighbors_ptr usage is index.
    *>     03 buff_neighbors_totalflow pic 9(8) comp.
    *> 01 neighbors_list.
    *>   02 neighbors_distance pic 9 comp.
    *>   02 neighbors_highest_flow usage is index.
    *>   02 neighbors_num pic 9(2) comp.
    *>   02 neighbors occurs 99 times indexed by neighbors_idx.
    *>     03 neighbors_ptr usage is index.
    *>     03 neighbors_totalflow pic 9(8) comp.
    *> 01 curr_neighbor usage is index.
    *> 01 neighbors_found_ind pic x value 'N'.
    *>   88 neighbors_found value 'Y'.
    *>   88 neighbors_not_found value 'N'.
    *> 01 neighbor_exists_ind pic x value 'N'.
    *>   88 neighbor_exists value 'Y'.
    *>   88 neighbor_not_exists value 'N'.
    *> 01 check_neighbor_idx usage is index.

    *> 01 neighbor_ptr usage is index.

        *> 03 valves_on_num pic 9(8) comp.

    *> 01 states_seen_stuff.
    *>   02 state_history_num pic 9(8) comp.
    *>   02 state_history occurs 499999 times indexed by state_history_idx.
    *>     03 state_history_curr_minute pic 9(2) comp.
    *>     03 state_history_curr_idx usage is index.
    *>     03 state_history_valves_on_num pic 9(8) comp.
    *>     03 state_history_valves_visited_num pic 9(8) comp.
    *>     03 state_history_valves_visited occurs 99 times indexed by state_history_valves_visited_idx.
    *>       04 state_history_valves_visited_ptr usage is index.
    *>       04 state_history_valves_visited_time pic 9(2) comp.
    *>   02 state_exists_ind pic x value 'N'.
    *>     88 state_exists value 'Y'.
    *>     88 state_does_not_exist value 'N'.


*>   move 30 to curr_minute
*>   perform until curr_minute < 1

*>   end-perform
*>   add 1 to po_num
*>   move starting_valve to po_valve_ptr(po_num)
*>   move curr_minute to po_step_num(po_num)
*>   move 0 to po_score(po_num)

*>   move 1 to po_head
*>   perform with test after until po_head = po_num
*>     perform checkout_options
*>     display "HIGHEST: " po_high_valve_ptr " " valve_name(po_high_valve_ptr) " " po_high_step_num " " po_high_score
*>     *> add 1 to po_head
*>   end-perform


*>   display spaces
*>   display "CHECKOUT: " valve_name(po_valve_ptr(po_head)) " " po_valve_ptr(po_head)
*>   add 1 to stack_num

*>   if po_score(po_head) > po_high_score
*>     display "HIGHEST FOUND!"
*>     move po_valve_ptr(po_head) to po_high_valve_ptr
*>     move po_step_num(po_head) to po_high_step_num
*>     move po_score(po_head) to po_high_score
*>   end-if

  *> Get the next item off the stack.
*>   move check_idx(stack_num) to curr_idx
*>   move check_minute(stack_num) to curr_minute
*>   move check_score(stack_num) to curr_score


  *> Trying to stop circular paths
*>   if add_to_stack and curr_time_since_valve_on > 10
*>     set do_not_add_to_stack to true
*>     display "AROUND WE GO! AND STOP."
*>   end-if

  *> If there are more valves left than minutes left. Assuming you must open all valves.
*>   if ((distance_num - 1) - curr_valves_visited_num) >= curr_minute
*>     set do_not_add_to_stack to true
*>     *> display "CAN NOT COMPLETE. DEAD-END."
*>   end-if

*>   *> Just an end state, once the last minute is reached.
*>   if curr_minute = 1
*>       and curr_proj_score >= best_score(curr_minute)
*>     *> move curr_score to best_score(curr_minute)

*>     display "VALVES ORDER: [" curr_valves_visited_num "]: SCORE: " curr_proj_score " " no advancing
*>     perform varying curr_valves_visited_idx from 1 by 1 until curr_valves_visited_idx > curr_valves_visited_num
*>       display valve_name(curr_valves_visited_ptr(curr_valves_visited_idx)) ": " curr_valves_visited_time(curr_valves_visited_idx) ", " no advancing
*>     end-perform
*>     display space

*>     *> set do_not_add_to_stack to true
*>   end-if

*> *>   if curr_minute > 1 and curr_proj_score >= best_score(curr_minute)
*>     *> move curr_proj_score to best_score(curr_minute)
*>     *> display "TEST: " curr_proj_score " vs " best_score(1)
*>      if curr_proj_score > best_score(1)
*>         *>  and curr_minute <= best_minute(1)
*>     *>    display "HBEST"
*>        move curr_proj_score to best_score(1)
*>        move curr_minute to best_minute(1)
*>     *>  else
*>     *>    set do_not_add_to_stack to true
*>      end-if



    *> if curr_minute < 5 move curr_score to best_score(curr_minute) end-if
    *> move curr_score to best_score(curr_minute)



    *> perform varying state_history_idx from 1 by 1 until state_history_idx > state_history_num
    *>   if state_history(state_history_idx) = (100000 * curr_minute) + curr_score
    *>     set do_not_add_to_stack to true
    *>     *> display "STATE DUPE!"
    *>   end-if
    *> end-perform

    *> Mark as visited. The check if visted should have happened before adding to stack.
    *> Probably optional, given curr_valves_visited?
    *> add 1 to curr_valves_visited_num
    *> move curr_idx to curr_valves_visited(curr_valves_visited_num)
    *> set curr_valve_visted(curr_idx) to true


      *>   subtract 1 from curr_minute
      *>   set valve_on(curr_idx) to true
      *>   add valve_flowrate(curr_idx) to valves_amt_venting

      *>   move curr_idx to check_idx(stack_num)
      *>   move curr_score to check_score(stack_num)
        *> set check_valve_on to true  *> Unnecessary? Most likely.

        *> add 1 to stack_num on size error display ">>>>> STACK OVERFLOW! <<<<<" end-add
        *> move curr_valve to stack(stack_num)

        *> add 1 to valves_visited_num(stack_num)
        *> move curr_idx to valves_visited_ptr(stack_num valves_visited_num(stack_num))
        *> move curr_minute to valves_visited_time(stack_num valves_visited_num(stack_num))

        *> add 1 to valves_on_num(stack_num)

        *> add valve_flowrate(curr_idx) to valves_amt_venting(stack_num)
        *> move 1 to time_since_valve_on(stack_num)
        *> compute check_minute(stack_num) = curr_minute - 1
        *> compute check_score(stack_num) = curr_score + valves_amt_venting(stack_num)

        *> *> add 1 to state_history_num
        *> *> compute state_history(state_history_num) = (100000 * check_minute(stack_num)) + check_score(stack_num)
        *> *> display "TURNED ON VALVE: [" check_minute(stack_num) "]: IDX: " curr_idx " NAME: " valve_name(curr_idx) " FLOWRATE: " valves_amt_venting(stack_num) " SCORE: " check_score(stack_num)


        *> *> If all valves have been turned on, just fast foward without movement.
        *> *> add 1 to valves_currently_on
        *> if valves_on_num(stack_num) = distance_num
        *> *>   add 1 to stack_num on size error display ">>>>> STACK OVERFLOW! <<<<<" end-add
        *> *>   move stack(stack_num - 1) to stack(stack_num)

        *>   move 1 to check_minute(stack_num)
        *>   *> Only change score of next item on stack, not current one.
        *>   compute check_score(stack_num) = curr_score + (check_minute(stack_num) * valves_amt_venting(stack_num))

        *> *>   add 1 to state_history_num
        *> *>   compute state_history(state_history_num) = (100000 * check_minute(stack_num)) + check_score(stack_num)

        *>   *> display "ALL ON: FF: " check_score(stack_num)
        *> if curr_valves_visited_ptr(1) = 4
        *>     and curr_valves_visited_ptr(2) = 2
        *>   *>   and curr_minute = 25
        *>     and curr_valves_visited_ptr(3) = 10
        *>   *>   and curr_minute = 21
        *>     and curr_valves_visited_ptr(4) = 8
        *>   *>   and curr_minute = 13
        *>     and curr_valves_visited_ptr(5) = 5
        *>     and curr_minute = 7
        *>   *>   and curr_valves_visited_ptr(6) = 3
        *>   *>   and curr_minute = 6
        *>   display "VALVE PREP! CURR_IDX: " curr_idx " CURR_TIME_SINCE: " curr_time_since_valve_on " CURR_MINUTE: " curr_minute " distance_num: " distance_num " CURR_VALVES_VISITED: " curr_valves_visited_num " CURR_SCORE: " curr_score " BEST_SCORE: " best_score(curr_minute)
        *>   *> display "IN ELEMENT: " valve_name(distance_to(curr_active_valves_idx distance_to_idx)) " AMT: " distance_amt(curr_active_valves_idx distance_to_idx) " CURR_MINUTE: " curr_minute " CHECK_VALVE: " check_valves_visited_found
        *> end-if


*> if curr_proj_score > best_score(1)
*>     and (curr_minute <= best_minute(1) or best_minute(1) = 0)
*>   move curr_proj_score to best_score(1)
*>   move curr_minute to best_minute(1)
*> else
*>   set do_not_add_to_stack to true

*> if curr_proj_score < best_score(1)
*>     and curr_minute >= best_minute(1)
*> *>   set do_not_add_to_stack to true
*>   continue
*> end-if


        *> *> Just an end state, once the last minute is reached.
        *> if curr_minute = 1
        *>     and curr_proj_score >= best_score(curr_minute)
        *>   *> move curr_score to best_score(curr_minute)

        *>   display "VALVES ORDER: [" curr_valves_visited_num "]: SCORE: " curr_proj_score " " no advancing
        *>   perform varying curr_valves_visited_idx from 1 by 1 until curr_valves_visited_idx > curr_valves_visited_num
        *>     display valve_name(curr_valves_visited_ptr(curr_valves_visited_idx)) ": " curr_valves_visited_time(curr_valves_visited_idx) ", " no advancing
        *>   end-perform
        *>   display space

        *>   *> set do_not_add_to_stack to true
        *> end-if


    *> compute total_found = total_found + (valve_flowrate(curr_idx) * (30 - curr_minute - 1))

    *> if curr_minute > 1
      *> Get all the neighbors for comparisons.
      *> Scan per step away: BFS, stop when you find neighbors a certain distance away.
    *>   initialize neighbors_list
    *>   add 1 to neighbors_num
    *>   move curr_idx to neighbors_ptr(neighbors_num)


          *> if valves_visited_ptr(stack_num 1) = 4
          *>     and valves_visited_ptr(stack_num 2) = 2
          *>     and valves_visited_ptr(stack_num 3) = 9
          *>   *>   and valves_visited_ptr(4) = 8
          *>   *>   and valves_visited_ptr(5) = 5
          *>   *>   and valves_visited_ptr(6) = 3
          *>   display "BEFORE CHECK STATE HISTORY, THIS ADDED TO THE STACK! [" check_minute(stack_num) "] FROM: " valve_name(curr_idx) " TO: " valve_name(check_idx(stack_num))
          *> end-if

            *> if check_score(stack_num) > 0 and check_minute(stack_num) > 1
            *> *> and ((distance_num - 1) - curr_valves_visited_num) <= check_minute(stack_num)
            *>   perform check_state_history

            *>   if state_exists
            *>     *> or ((distance_num - 1) - curr_valves_visited_num) >= check_minute(stack_num)
            *>     *> if valves_visited_ptr(stack_num 1) = 4
            *>     *>     and valves_visited_ptr(stack_num 2) = 2
            *>     *>     and valves_visited_ptr(stack_num 3) = 9
            *>     *>   *>   and valves_visited_ptr(4) = 8
            *>     *>   *>   and valves_visited_ptr(5) = 5
            *>     *>   *>   and valves_visited_ptr(6) = 3
            *>     *>   display "REMOVED EXISTING: [" check_minute(stack_num) "] " valve_name(curr_idx) " TO: " valve_name(check_idx(stack_num))
            *>     *> end-if
            *>     subtract 1 from stack_num
            *>   else
            *>     *> if valves_visited_ptr(stack_num 1) = 4
            *>     *>     and valves_visited_ptr(stack_num 2) = 2
            *>     *>     and valves_visited_ptr(stack_num 3) = 9
            *>     *>   *>   and valves_visited_ptr(4) = 8
            *>     *>   *>   and valves_visited_ptr(5) = 5
            *>     *>   *>   and valves_visited_ptr(6) = 3
            *>     *>   display "ADDING HISTORY: [" check_minute(stack_num) "] " valve_name(curr_idx) " TO: " valve_name(check_idx(stack_num))
            *>     *> end-if

            *>     *>   if curr_valves_visited_ptr(1) = 4
            *>     *>     and curr_valves_visited_ptr(2) = 2
            *>     *>   *>   and curr_minute = 25
            *>     *>     and curr_valves_visited_ptr(3) = 10
            *>     *>   *>   and curr_minute = 21
            *>     *>     and curr_valves_visited_ptr(4) = 8
            *>     *>   *>   and curr_minute = 13
            *>     *>     and curr_valves_visited_ptr(5) = 5
            *>     *>     and curr_minute = 9
            *>     *>   *>   and curr_valves_visited_ptr(6) = 3
            *>     *>   *>   and curr_minute = 6
            *>     *> *>   display "IN ELEMENT: " valve_name(distance_to(curr_active_valves_idx distance_to_idx)) " AMT: " distance_amt(curr_active_valves_idx distance_to_idx) " CURR_MINUTE: " curr_minute " CHECK_VALVE: " check_valves_visited_found
            *>     *>   display "ADDING HISTORY: [" check_minute(stack_num) "] " valve_name(curr_idx) " TO: " valve_name(check_idx(stack_num))
            *>     *> end-if

            *>     *> display "HISTORY SIZE: " state_history_num

            *>     add 1 to state_history_num
            *>     *> compute state_history(state_history_num) = (100000 * check_minute(stack_num)) + check_score(stack_num)
            *>     move check_idx(stack_num) to state_history_curr_idx(state_history_num)
            *>     move check_minute(stack_num) to state_history_curr_minute(state_history_num)
            *>     perform varying valves_visited_idx from 1 by 1 until valves_visited_idx > valves_visited_num(stack_num)
            *>       add 1 to state_history_valves_visited_num(state_history_num)
            *>       move valves_visited_ptr(stack_num valves_visited_idx) to state_history_valves_visited_ptr(state_history_num state_history_valves_visited_num(state_history_num))
            *>       move valves_visited_time(stack_num valves_visited_idx) to state_history_valves_visited_time(state_history_num state_history_valves_visited_num(state_history_num))
            *>     end-perform
            *>     continue
            *>   end-if
            *> end-if


      *> set neighbors_not_found to true
      *> perform with test after until neighbors_found
      *>   perform get_neighbors_at_distance
      *> end-perform

      *> *> perform display_neighbors
      *> *> display "NEIGHBORS dist[" neighbors_distance "] BEST: " valve_name(neighbors_ptr(neighbors_highest_flow)) " >> " no advancing
      *> *> perform varying neighbors_idx from 1 by 1 until neighbors_idx > neighbors_num
      *> *>   display valve_name(neighbors_ptr(neighbors_idx)) ", " no advancing
      *> *> end-perform
      *> *> display space

      *> *> perform varying neighbors_idx from 1 by 1 until neighbors_idx > neighbors_num
      *> *>   add 1 to stack_num
      *> *>   move neighbors(neighbors_idx) to stack(stack_num)
      *> *> end-perform

      *> *> Go to the best neighbor.
      *> if neighbors_highest_flow <> 0
      *>   add 1 to stack_num
      *>   move neighbors_ptr(neighbors_highest_flow) to check_idx(stack_num)
      *>   compute check_minute(stack_num) = curr_minute - neighbors_distance
      *>   perform checkout_options
      *> end-if
    *> end-if


*> get_neighbors_at_distance.
*> *>   move neighbors_list to buff_neighbors_list
*>   *> display "GET NEIGHBORS: " no advancing
*>   *> perform display_neighbors
*>
*> *>   if neighbors_distance = 0
*> *>     add 1 to neighbors_distance
*>
*> *>     perform varying valve_neighbors_idx from 1 by 1 until valve_neighbors_idx > valve_neighbors_num(curr_idx)
*> *>       move valve_neighbor_ptr(curr_idx valve_neighbors_idx) to curr_neighbor
*>
*> *>       add 1 to neighbors_num
*> *>       move curr_neighbor to neighbors_ptr(neighbors_num)
*>
*> *>       compute neighbors_totalflow(neighbors_num) = valve_flowrate(curr_neighbor) * (curr_minute - neighbors_distance - 1)
*> *>       *> display "TOTALFLOW: [" valve_name(curr_neighbor) "]: " neighbors_totalflow(neighbors_num)
*> *>       display "VISTED?: " valve_name(curr_neighbor) " > " valve_visited_ind(curr_neighbor)
*> *>       if not valve_visted(curr_neighbor)
*> *>           and neighbors_totalflow(neighbors_num) > neighbors_totalflow(neighbors_highest_flow)
*> *>         set neighbors_highest_flow to neighbors_num
*> *>         set neighbors_found to true
*> *>       end-if
*> *>     end-perform
*> *>   else
*>     initialize buff_neighbors_list
*>     move neighbors_distance to buff_neighbors_distance
*>
*>     add 1 to buff_neighbors_distance
*>     perform varying neighbors_idx from 1 by 1 until neighbors_idx > neighbors_num
*>       perform varying valve_neighbors_idx from 1 by 1 until valve_neighbors_idx > valve_neighbors_num(neighbors_ptr(neighbors_idx))
*>         move valve_neighbor_ptr(neighbors_ptr(neighbors_idx) valve_neighbors_idx) to curr_neighbor
*>
*>         perform check_neighbor_exists
*>
*>         if neighbor_not_exists
*>           add 1 to buff_neighbors_num
*>           move curr_neighbor to buff_neighbors_ptr(buff_neighbors_num)
*>
*>           compute buff_neighbors_totalflow(buff_neighbors_num) = valve_flowrate(curr_neighbor) * (curr_minute - neighbors_distance - 1)
*>           *> compute neighbors_highest_flow = function max(neighbors_highest_flow, buff_neighbors_totalflow(buff_neighbors_num))
*>           *> display "TOTALFLOW: [" valve_name(curr_neighbor) "]: " buff_neighbors_totalflow(buff_neighbors_num)
*>           *> display "VISTED?: " valve_name(curr_neighbor) " > " valve_visited_ind(curr_neighbor)
*>
*>           perform check_neighbor_exists
*>
*>           if not valve_visted(curr_neighbor)
*>               and buff_neighbors_totalflow(buff_neighbors_num) > buff_neighbors_totalflow(buff_neighbors_highest_flow)
*>             set buff_neighbors_highest_flow to buff_neighbors_num
*>             set neighbors_found to true
*>           end-if
*>         end-if
*>       end-perform
*>     end-perform
*>     move buff_neighbors_list to neighbors_list
*> *>   end-if
*>
*>   *> display "END NEIGHBORS: " no advancing
*>   *> perform display_neighbors
*>     *> display "DISTANCE: " neighbors_distance
*>     if neighbors_distance > distance_num
*>       set neighbors_found to true
*>     end-if
*>   .

*> check_neighbor_exists.
*>   set neighbor_not_exists to true
*>   perform varying check_neighbor_idx from 1 by 1 until check_neighbor_idx > buff_neighbors_num or neighbor_exists
*>     if buff_neighbors_ptr(check_neighbor_idx) = curr_neighbor
*>       set neighbor_exists to true
*>     end-if
*>   end-perform
*>   .


*> check_state_history.
*>   set state_does_not_exist to true
*>   perform varying state_history_idx from 1 by 1
*>       until state_history_idx > state_history_num or state_exists
*>     *> if state_history(state_history_idx) = (100000 * check_minute(stack_num)) + check_score(stack_num)
*>     if state_history_curr_idx(state_history_idx) = check_idx(stack_num)
*>         and state_history_curr_minute(state_history_idx) = check_minute(stack_num)

*>       set state_exists to true
*>       *> display "ALREADY EXISTS: [" check_minute(stack_num) "] " valve_name(check_idx(stack_num))
*>       perform varying state_history_valves_visited_idx from 1 by 1
*>           until state_history_valves_visited_idx > state_history_valves_visited_num(state_history_idx)
*>             or state_does_not_exist
*>         if state_history_valves_visited_ptr(state_history_idx state_history_valves_visited_idx) <> valves_visited_ptr(stack_num state_history_valves_visited_idx)
*>             or state_history_valves_visited_time(state_history_idx state_history_valves_visited_idx) <> valves_visited_time(stack_num state_history_valves_visited_idx)
*>           set state_does_not_exist to true
*>         end-if
*>         *> if state_exists display "STATE DUPE!" end-if
*>       end-perform
*>     end-if
*>   end-perform
*>   .


*>   display "NEIGHBORS dist[" neighbors_distance "] BEST: " valve_name(neighbors_ptr(neighbors_highest_flow)) " >> " no advancing
*>   perform varying neighbors_idx from 1 by 1 until neighbors_idx > neighbors_num
*>     display valve_name(neighbors_ptr(neighbors_idx)) ", " no advancing
*>   end-perform
*>   display space
*>   .

  *> Add neighbors to queue, BFS.
*>   move temp_idx to check_valve_idx(stack_num)
*>   perform varying check_idx(stack_num) from 1 by 1 until check_idx(stack_num) > valve_neighbors_num(check_valve_idx)
*>   perform varying temp_idx from 1 by 1 until temp_idx > valve_neighbors_num(po_valve_ptr(po_head))
*>     display "CHECKING NEIGHBOR: " valve_neighbor_name(po_valve_ptr(po_head) temp_idx) " STEP: " po_step_num(po_head) " - 1"
*>     *> Check if neighbor is already in visit list.
*>     set po_found to 0
*>     perform varying po_idx from 1 by 1 until po_idx > po_num or po_found = 1
*>       if po_valve_ptr(po_idx) = valve_neighbor_ptr(po_valve_ptr(po_head) temp_idx)
*>         display "ALREADY IN QUEUE: " po_valve_ptr(po_idx) " " valve_neighbor_name(po_valve_ptr(po_head) temp_idx)
*>         move 1 to po_found
*>       end-if
*>     end-perform

*>     if po_found = 0
*>       add 1 to po_num
*>       move valve_neighbor_ptr(po_valve_ptr(po_head) temp_idx) to po_valve_ptr(po_num)
*>       compute po_step_num(po_num) = po_step_num(po_head) - 1
*>       compute po_score(po_num) = (po_step_num(po_num) - 1) * valve_flowrate(po_valve_ptr(po_num))
*>       display "ADDING TO QUEUE: " valve_neighbor_name(po_valve_ptr(po_head) temp_idx) " STEP: " po_step_num(po_num) " SCORE: " po_score(po_num)
*>       *> display "NEW SCORE: " po_score(po_num) " = (" po_step_num(po_num) " - 1) * " valve_flowrate(po_valve_ptr(po_num))
*>     end-if
*>   end-perform
