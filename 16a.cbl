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
      02 valves_num pic 9(2).
      02 valve occurs 62 times indexed by valve_idx.
        03 valve_name pic x(2).
        03 valve_flowrate pic 9(2).
        03 valve_neighbors_num pic 9.
        03 valve_neighbors occurs 5 times indexed by valve_neighbors_idx.
          04 valve_neighbor_name pic x(2).
          04 valve_neighbor_ptr pic 9(2).
        *>   04 valve_neighbor_ptr usage is pointer.
    
    01 active_valves.
      02 active_valve_num pic 9(2).
      02 active_valve pic s9(8) occurs 62 times indexed by active_valve_idx.
    77 active_valve_idx2 usage is index.

    01 valves_on_tbl.
      02 valves_on pic 9(2) occurs 62 times indexed by valves_on_idx.

    01 path_options_queue.
      02 po_high_valve_ptr usage is index.
      02 po_high_step_num pic 9(2) comp.
      02 po_high_score pic 9(8) comp.
      02 po_head usage is index.
      02 po_num pic 9(2).
      02 po occurs 62 times indexed by po_idx.
        03 po_valve_ptr usage is index.
        03 po_step_num pic 9(2) comp.
        03 po_score pic 9(8) comp.
      02 po_found pic 9.

    01 path_chosen.
      02 pc_num pic 9(2).
      02 pc occurs 62 times indexed by pc_idx.
        03 pc_valve_ptr pic 9(2).
        03 pc_step_num pic 9(2) comp.
        03 pc_score pic 9(8) comp.

    01 temp_name pic x(2).
    01 temp_flowrate pic 9(2).
    01 temp_misc pic x(99).
    01 temp_done pic 9.
    01 temp_neighbors pic x(99).
    01 temp_neighbor pic x(2).
    01 temp_idx usage is index.
    01 temp_ptr usage is index.

    01 checkout_stack.
      02 stack_num pic 9(4) comp.
      02 stack occurs 999 times.
        03 check_idx usage is index.
        03 check_valve_idx usage is index.

    77 curr_step pic 9(2) comp.
    77 total_found pic 9(8) comp.

procedure division.
  *> call 'lib-readdata' using function module-id ".dat" rf_all_lines
  call 'lib-readdata' using function module-id ".da1" rf_all_lines

  move 0 to total_found
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

    *> Keep track of all valves with positive flowrates
    if valve_flowrate(rf_idx) > 0
      add 1 to active_valve_num
      move rf_idx to active_valve(active_valve_num)
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
    display "VALVE: [" valve_idx " of " valves_num "] " valve_name(valve_idx) " " valve_flowrate(valve_idx) " NEIGHBORS: " no advancing
    perform varying valve_neighbors_idx from 1 by 1 until valve_neighbors_idx > valve_neighbors_num(valve_idx)
      move valve_neighbor_name(valve_idx valve_neighbors_idx) to temp_name
      perform get_idx_for_name
      if temp_done = 1
        move temp_idx to valve_neighbor_ptr(valve_idx valve_neighbors_idx)
        *> set valve_neighbor_ptr(valve_idx valve_neighbors_idx) to temp_ptr
        display temp_name ":" temp_idx no advancing
        if valve_neighbors_idx < valve_neighbors_num(valve_idx) display ", " no advancing end-if
      else
        display "TRANSLATE: VALVE " temp_name " NOT FOUND!"
      end-if
    end-perform
    display spaces
  end-perform

  *> Generate all the possibilties amongst the positive flowrate valves
  perform varying active_valve_idx from 1 by 1 until active_valve_idx > active_valve_num
    perform varying active_valve_idx2 from 1 by 1 until function mod(active_valve_idx2 active_valve_num) = active_valve_idx
      // Permutations!
    end-perform
  end-perform

  *> Find most beneficial path to next valve to open, and go there. Repeat.
  *> Most beneficial path: At each node, try to open valve (multiplied by steps taken to get there) and get score. Then traverse to next nodes. Repeat.

  *> Traverse paths BFS, if you find a node with a valve, record score with steps(minus 30?) and (valve * steps). Continue.
  *> Then find the highest total, teleport there, adopt steps(minus 30), record total flow so far, and set valve to 0. Repeat.


  move 30 to curr_step
*>   perform until curr_step < 1

*>   end-perform
  add 1 to po_num
  move starting_valve to po_valve_ptr(po_num)
  move curr_step to po_step_num(po_num)
  move 0 to po_score(po_num)

  move 1 to po_head
  perform with test after until po_head = po_num
    perform checkout_options
    display "HIGHEST: " po_high_valve_ptr " " valve_name(po_high_valve_ptr) " " po_high_step_num " " po_high_score
    add 1 to po_head
  end-perform

  display "FINAL: " total_found

  goback.


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

checkout_options.
  display spaces
  display "CHECKOUT: " valve_name(po_valve_ptr(po_head)) " " po_valve_ptr(po_head)
  add 1 to stack_num

  if po_score(po_head) > po_high_score
    display "HIGHEST FOUND!"
    move po_valve_ptr(po_head) to po_high_valve_ptr
    move po_step_num(po_head) to po_high_step_num
    move po_score(po_head) to po_high_score
  end-if

  *> Add neighbors to queue, BFS.
*>   move temp_idx to check_valve_idx(stack_num)
*>   perform varying check_idx(stack_num) from 1 by 1 until check_idx(stack_num) > valve_neighbors_num(check_valve_idx)
  perform varying temp_idx from 1 by 1 until temp_idx > valve_neighbors_num(po_valve_ptr(po_head))
    display "CHECKING NEIGHBOR: " valve_neighbor_name(po_valve_ptr(po_head) temp_idx) " STEP: " po_step_num(po_head) " - 1"
    *> Check if neighbor is already in visit list.
    set po_found to 0
    perform varying po_idx from 1 by 1 until po_idx > po_num or po_found = 1
      if po_valve_ptr(po_idx) = valve_neighbor_ptr(po_valve_ptr(po_head) temp_idx)
        display "ALREADY IN QUEUE: " po_valve_ptr(po_idx) " " valve_neighbor_name(po_valve_ptr(po_head) temp_idx)
        move 1 to po_found
      end-if
    end-perform

    if po_found = 0
      add 1 to po_num
      move valve_neighbor_ptr(po_valve_ptr(po_head) temp_idx) to po_valve_ptr(po_num)
      compute po_step_num(po_num) = po_step_num(po_head) - 1
      compute po_score(po_num) = (po_step_num(po_num) - 1) * valve_flowrate(po_valve_ptr(po_num))
      display "ADDING TO QUEUE: " valve_neighbor_name(po_valve_ptr(po_head) temp_idx) " STEP: " po_step_num(po_num) " SCORE: " po_score(po_num)
      *> display "NEW SCORE: " po_score(po_num) " = (" po_step_num(po_num) " - 1) * " valve_flowrate(po_valve_ptr(po_num))
    end-if
  end-perform
  .
