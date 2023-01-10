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

    01 distances_stuff.
      02 distance_num pic 9(8) comp.  *> Larger size for lib-dikjstra.
      02 distances occurs 99 times indexed by distances_idx.
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

    *> For lib-permutations.
    01 inputs.
      02 input_cnt pic s9(8) comp.  *> Must be provided. Num of possible items.
      02 input_head usage is index value 1. *> Internal use during recursion.

    01 permutations_stuff.
      02 permutations occurs 2 times indexed by permutations_idx.
        03 perm_len pic s9(8) comp.  *> Must be provided. Num of items per permutation.
        03 perm_list_cnt pic s9(8) comp.
        03 perm_list occurs 99999 times  *> Corruption on copying unboundeds to working/local storage.
            indexed by perm_list_idx.
          04 perm_cnt pic s9(8) comp.
          04 perm usage is index occurs 99 times
            indexed by perm_idx.

    77 your_paths_idx usage is index.
    77 your_perm_idx usage is index.
    77 your_path_score pic s9(8) comp.
    77 your_remainder pic 9 comp.
    77 elephant_paths_idx usage is index.
    77 elephant_perm_idx usage is index.
    77 elephant_path_score pic s9(8) comp.
    77 best_combo_score pic s9(8) comp.

    77 get_split_idx usage is index.
    77 get_split_inner_idx usage is index.
    77 get_split_who_idx usage is index.
    77 get_split_path_idx usage is index.
    77 get_split_perm_idx usage is index.

    01 same_perm_found_ind pic x value 'N'.
      88 same_perm_found value 'Y'.
      88 same_perm_not_found value 'N'.

    01 split_distances_stuff.
      02 split_distance_num pic 9(8) comp.  *> Larger size for lib-dikjstra.
      02 split_distances occurs 99 times indexed by split_distances_idx.
        03 split_distance_from usage is index.
        03 split_distance_to_num pic 9(2) comp.
        03 split_distance_to_targets occurs 99 times indexed by split_distance_to_idx.
          04 split_distance_to usage is index.
          04 split_distance_amt pic 9(2) comp.


procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
*>   call 'lib-readdata' using function module-id ".da1" rf_all_lines

  *> move 0 to valves_num
  add 1 to valves_num *> Reserve the top of the list for AA.
  add 1 to distance_num *> Reserve the top of the list for AA.

  perform varying rf_idx from 1 by 1 until rf_idx > rf_cnt
    *> display "LINE: " function trim(rf_row(rf_idx))

    *> Note index if this is the starting valve. Place at the top of the list.
    if rf_row(rf_idx)(7:2) = "AA"
      move 1 to valves_num
      move valves_num to starting_valve

      move valves_num to distance_from(1)
      *> subtract 1 from valves_num
    else
      add 1 to valves_num
    end-if

    *> Get name of valve.
    move rf_row(rf_idx)(7:2) to valve_name(valves_num)

    *> Get flowrate of valve.
    move rf_row(rf_idx)(24:2) to temp_flowrate
    if temp_flowrate(2:) = ";"
      move temp_flowrate(1:1) to valve_flowrate(valves_num)
    else
      move temp_flowrate to valve_flowrate(valves_num)
    end-if

    *> *> Keep track of all valves with positive flowrates, including the starting point AA.
    if valve_flowrate(valves_num) > 0
        *> or valve_name(valves_num) = "AA"
      add 1 to distance_num
      move valves_num to distance_from(distance_num)
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
            add 1 to valve_neighbors_num(valves_num)
            move temp_neighbor to valve_neighbor_name(valves_num valve_neighbors_num(valves_num))
        end-unstring
      end-perform
    else
      add 1 to valve_neighbors_num(valves_num)
      move function trim(temp_neighbors) to valve_neighbor_name(valves_num valve_neighbors_num(valves_num))
    end-if

    *> If we put AA at the top, make sure lines thereafter keep processing properly.
    if rf_row(rf_idx)(7:2) = "AA"
      compute valves_num = rf_idx
    end-if

    *> Display the processed lines.
    *> display "NEW: " valve_name(valves_num) " " valve_flowrate(valves_num) " NEIGHBORS: " no advancing
    *> perform varying valve_neighbors_idx from 1 by 1 until valve_neighbors_idx > valve_neighbors_num(valves_num)
    *>   display valve_neighbor_name(valves_num valve_neighbors_idx) no advancing
    *>   if valve_neighbors_idx < valve_neighbors_num(valves_num) display ", " no advancing end-if
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
  perform varying distances_idx from 1 by 1 until distances_idx > distance_num
    perform varying curr_active_valves_idx from 1 by 1 until curr_active_valves_idx > distance_num
      if distances_idx <> curr_active_valves_idx
          and valve_name(distance_from(curr_active_valves_idx)) <> 'AA'  *> Skip 'AA' as a neighbor.
        initialize path
        set get_neighbors to entry "get_neighbors"
        *> call 'lib-dijkstra' using distances_idx curr_active_valves_idx distance_num path get_neighbors get_neighbors_stuff
        call 'lib-dijkstra' using distance_from(distances_idx) distance_from(curr_active_valves_idx) valves_num path get_neighbors get_neighbors_stuff

        add 1 to distance_to_num(distances_idx)
        move distance_from(curr_active_valves_idx) to distance_to(distances_idx distance_to_num(distances_idx))
        move path_len to distance_amt(distances_idx distance_to_num(distances_idx))

        *> display "DISTANCE: " valve_name(distance_from(distances_idx)) " to " valve_name(distance_from(curr_active_valves_idx)) ": " path_len " " no advancing
        *> perform varying path_idx from 1 by 1 until path_idx > path_len
        *>   display valve_name(path_val(path_idx)) " -> " no advancing
        *> end-perform
        *> display space
      end-if
    end-perform
  end-perform


  *> NOTE: Unfortunately, test has an even number of input items and data have odd. GGAAAAAHHHH.
  compute input_cnt = distance_num - 1  *> Do not include 'AA'
  compute your_remainder = function mod(input_cnt 2)

  *> Give myself the larger num of perms if input_cnt is odd. Because how fast are elephants in a cave?!
  compute perm_len(1) = (input_cnt / 2) + your_remainder
  *> display "CALLING: " input_cnt perm_len(1)
  call 'lib-permutations' using inputs permutations(1)

  compute perm_len(2) = (input_cnt / 2)
  move 1 to input_head
  *> display "CALLING: " input_cnt perm_len(2)
  call 'lib-permutations' using inputs permutations(2)

  *> display "ALL PERMS: "
  *> perform varying permutations_idx from 1 by 1 until permutations_idx > 2
  *>   display "PERMS: [" perm_list_cnt(permutations_idx) "]"
  *>   perform varying perm_list_idx from 1 by 1 until perm_list_idx > perm_list_cnt(permutations_idx)
  *>     display "[ " no advancing
  *>     perform varying perm_idx from 1 by 1 until perm_idx > perm_cnt(permutations_idx perm_list_idx)
  *>       display valve_name(distance_from(perm(permutations_idx perm_list_idx perm_idx) + 1)) no advancing
  *>       if perm_idx < perm_cnt(permutations_idx perm_list_idx) display ", " no advancing end-if
  *>     end-perform
  *>     display " ]"
  *>   end-perform
  *> end-perform

  *> Compare every one of your possible paths with every one of the elephant's possible paths.
  *> Note that this assumes that AA is at the top of the split_distance lists!
  perform varying your_paths_idx from 1 by 1 until your_paths_idx > perm_list_cnt(1)
    *> Start by processing one of your path possibilities and getting the best score for that path.
    *> Create a filtered list of split_distances that only include the valves for this path.
    initialize split_distances_stuff
    move your_paths_idx to get_split_path_idx
    move 1 to get_split_who_idx
    set get_split_idx to starting_valve  *> Add the 'AA' (starting point) distances.
    perform get_split_distances_for_idx

    perform varying get_split_perm_idx from 1 by 1 until get_split_perm_idx > perm_cnt(1 your_paths_idx)
      compute get_split_idx = perm(1 your_paths_idx get_split_perm_idx) + 1  *> Since we skipped 'AA'
      perform get_split_distances_for_idx
    end-perform
    *> display "SPLITS: "
    *> perform varying split_distances_idx from 1 by 1 until split_distances_idx > split_distance_num
    *>   perform varying split_distance_to_idx from 1 by 1 until split_distance_to_idx > split_distance_to_num(split_distances_idx)
    *>     display valve_name(split_distance_from(split_distances_idx))
    *>         " -> " valve_name(split_distance_to(split_distances_idx split_distance_to_idx))
    *>         " [" split_distance_amt(split_distances_idx split_distance_to_idx)
    *>         "]"
    *>   end-perform
    *> end-perform

    *> Then, get the score for this path.
    perform find_best_score
    move best_score to your_path_score

    perform varying elephant_paths_idx from 1 by 1 until elephant_paths_idx > perm_list_cnt(2)
      *> Compare your path against the elephant's path.

      *> Skip any paths that have the same valve between the two (don't visit the same valves).
      set same_perm_not_found to true
      perform varying your_perm_idx from 1 by 1 until your_perm_idx > perm_cnt(1 your_paths_idx) or same_perm_found
        perform varying elephant_perm_idx from 1 by 1 until elephant_perm_idx > perm_cnt(2 elephant_paths_idx) or same_perm_found
          if perm(1 your_paths_idx your_perm_idx) = perm(2 elephant_paths_idx elephant_perm_idx)
            set same_perm_found to true
            *> display "SKIP"
          end-if
        end-perform
      end-perform

      if same_perm_not_found
        *> Create a filtered list of split_distances that only include the valves for this path.
        initialize split_distances_stuff
        move elephant_paths_idx to get_split_path_idx
        move 2 to get_split_who_idx
        set get_split_idx to starting_valve  *> Add the 'AA' (starting point) distances.
        perform get_split_distances_for_idx

        perform varying get_split_perm_idx from 1 by 1 until get_split_perm_idx > perm_cnt(2 elephant_paths_idx)
          compute get_split_idx = perm(2 elephant_paths_idx get_split_perm_idx) + 1  *> Since we skipped 'AA'
          perform get_split_distances_for_idx
        end-perform

        *> Then, get the score for this path.
        perform find_best_score
        move best_score to elephant_path_score

        compute best_combo_score = function max(best_combo_score, elephant_path_score + your_path_score)
      end-if
    end-perform
  end-perform

  display "BEST COMBO: " best_combo_score

*>   1310 L

  goback.


get_split_distances_for_idx.
  add 1 to split_distance_num
  set split_distance_from(split_distance_num) to distance_from(get_split_idx)
  *> display "GET NEIGHBORS FOR: " valve_name(distance_from(get_split_idx))

  *> Only add distance targets that exist in the original path list.
  *> perform varying get_split_inner_idx from 1 by 1 until get_split_inner_idx > perm_cnt(your_paths_idx)
  *> Using perm_len instead of perm_cnt(your_paths_idx) since all the paths should be the same length.
  perform varying get_split_inner_idx from 1 by 1 until get_split_inner_idx > perm_len(get_split_who_idx)
    *> display "INNER: " valve_name(distance_from(perm(your_paths_idx get_split_inner_idx)))
    if get_split_perm_idx = get_split_inner_idx
      set same_perm_found to true
    else
      set same_perm_not_found to true
    end-if

    perform varying distance_to_idx from 1 by 1 until distance_to_idx > distance_to_num(get_split_idx) or same_perm_found
      *> display "COMPARING: DIST:" valve_name(distance_to(get_split_idx distance_to_idx)) " vs PERM:" valve_name(distance_from(perm(your_paths_idx get_split_inner_idx)))
      if distance_to(get_split_idx distance_to_idx) = distance_from(perm(get_split_who_idx get_split_path_idx get_split_inner_idx) + 1)  *> +1 since we skipped 'AA'
        add 1 to split_distance_to_num(split_distance_num)
        move distance_to(get_split_idx distance_to_idx) to split_distance_to(split_distance_num split_distance_to_num(split_distance_num))
        move distance_amt(get_split_idx distance_to_idx) to split_distance_amt(split_distance_num split_distance_to_num(split_distance_num))
        set same_perm_found to true

        *> display "SPLIT: " valve_name(distance_from(get_split_idx))
        *>   " to " valve_name(distance_to(get_split_idx distance_to_idx))
        *>   ": " distance_amt(get_split_idx distance_to_idx)
      end-if
    end-perform
    if same_perm_not_found display "SPLIT: NOT FOUND: " valve_name(distance_from(get_split_idx)) end-if
  end-perform
  .

*> From part 1.
find_best_score.
  initialize stack_checkout_options
  initialize curr_valve
  move 0 to best_score
  move 0 to best_minute

  set stack_num to 1
  move starting_valve to check_idx(stack_num)
  move 26 to check_minute(stack_num)

  perform checkout_options until stack_num < 1
  *> display "BEST: " best_score
  .

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
        *> display "GET split_distANCE: " valve_name(distance_from(curr_active_valves_idx)) " NEIGHBORS: " split_distance_to_num(curr_active_valves_idx)
        perform varying split_distance_to_idx from 1 by 1 until split_distance_to_idx > split_distance_to_num(curr_active_valves_idx)
              *> or curr_valves_visited_num = split_distance_num
          move split_distance_to(curr_active_valves_idx split_distance_to_idx) to check_if_valve_on_ptr
          perform check_if_valve_on

          if check_valve_off
              and split_distance_to(curr_active_valves_idx split_distance_to_idx) <> starting_valve  *> Skip 'AA'
              and split_distance_amt(curr_active_valves_idx split_distance_to_idx) < curr_minute
            add 1 to stack_num on size error display ">>>>> STACK OVERFLOW! <<<<<" end-add
            move curr_valve to stack(stack_num)

            move split_distance_to(curr_active_valves_idx split_distance_to_idx) to check_idx(stack_num)
            compute check_minute(stack_num) = curr_minute - split_distance_amt(curr_active_valves_idx split_distance_to_idx)
            compute check_score(stack_num) = curr_score + (curr_valves_amt_venting * split_distance_amt(curr_active_valves_idx split_distance_to_idx))
            *> add split_distance_amt(curr_active_valves_idx split_distance_to_idx) to time_since_valve_on(stack_num)

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
  perform varying distances_idx from 1 by 1
      until distances_idx > split_distance_num or curr_active_valves_idx > 0
    if split_distance_from(distances_idx) = curr_idx
      set curr_active_valves_idx to distances_idx
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
