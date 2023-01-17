           >>source format free
identification division.
program-id. 12b.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_line_cnt pic s9(8) comp value 0.
      02 rf_line_row pic x(9999) occurs 0 to 9999 times
          depending on rf_line_cnt indexed by rf_line_idx.

    77 letters_per_line pic s9(2) comp.
    77 curr_letter_idx pic s9(2) comp.
    77 curr_letter pic x.

    77 alpha pic x(26).
    77 alpha_idx pic s9(2) comp.

    01 row_idx pic s9(4) comp.
    *> 01 heuristic_return pic s9(8) comp.

    *> 01 check_row_num pic s9(4) comp.
    *> 01 check_col_num pic s9(4) comp.
    *> 01 check_return pic s9(4) comp.

    01 all_as.
      02 as_table_len pic s9(4) comp value 0.
      02 as_table usage is index occurs 0 to 99999 times
          depending on as_table_len indexed by as_table_idx.
    77 lowest_as usage is index.

    01 shortest_path.
      02 shortest_path_len pic s9(8) comp value 0.
      02 shortest_path_val usage is index occurs 0 to 99999 times
          depending on shortest_path_len indexed by shortest_path_idx.

    01 total pic s9(8) comp value 0.


    01 nodes.
    *>   02 nodes_len pic s9(8) comp value 0.
      02 nodes_per_row pic s9(4) comp.
      02 nodes_row pic s9(4) comp value 0 occurs 0 to 99999 times
          depending on nodes_len indexed by nodes_idx.

    01 neighbor_ptr usage is index.
    01 current_rowpos usage is index.
    01 neighbor_rowpos usage is index.

    *> For lib-astar/dijkstra
    01 dijkstra_stuff.
      02 startPt usage is index.
      02 goalPt usage is index.
      02 nodes_len pic s9(8) comp.
      02 get_neighbors_stuff.
        03 get_neighbors procedure-pointer.
        03 current_ptr usage is index.
        03 curr_neighbors_num pic s9 comp.
        03 curr_neighbors occurs 9 times indexed by curr_neighbors_idx.
          04 curr_neighbor_ptr usage is index.
          04 curr_neighbor_dist pic s9.
      02 path.
        03 path_len pic s9(8) comp value 0.
        03 path_val usage is index occurs 0 to 99999 times
            depending on path_len indexed by path_idx.


procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
  *> call 'lib-readdata' using function module-id ".da1" rf_all_lines

  move length of function trim(rf_line_row(1)) to nodes_per_row
  *> display "LENGTH: " nodes_per_row

  move'zyxwvutsrqponmlkjihgfedcba' to alpha

  *> Find START and END
  set nodes_idx to 1
  perform varying rf_line_idx from 1 by 1 until rf_line_idx > rf_line_cnt
    *> display "LINE: " function trim(rf_line_row(rf_line_idx))
    perform varying row_idx from 1 by 1 until row_idx > nodes_per_row
      move rf_line_row(rf_line_idx)(row_idx:1) to curr_letter
      if curr_letter = "S"
        move nodes_idx to startPt
        *> move 0 to nodes_row(nodes_idx)
        move 26 to nodes_row(nodes_idx)
        *> display "START IDX: " nodes_idx
      else
        if curr_letter = "E"
          move nodes_idx to goalPt
          *> move 26 to nodes_row(nodes_idx)
          move 0 to nodes_row(nodes_idx)
          *> display "END IDX: " nodes_idx
        else
          *> Even though ASCII 'a' is 97, COBOL has 'a' at 98?!?!
          *> display rf_line_row(rf_line_idx)(row_idx:1) ": " function ord(rf_line_row(rf_line_idx)(row_idx:1))
          *> compute nodes_row(nodes_idx) = function ord(rf_line_row(rf_line_idx)(row_idx:1)) - 97

          *> Get values where z = 1 up to a = 26, using alpha lookup table.
          perform varying alpha_idx from 1 by 1 until alpha_idx > 26
            if alpha(alpha_idx:1) = rf_line_row(rf_line_idx)(row_idx:1)
              move alpha_idx to nodes_row(nodes_idx)
            end-if
          end-perform
          *> display rf_line_row(rf_line_idx)(row_idx:1) ": " nodes_row(nodes_idx)

          *> Also, keep track of all the 'a' values so we can use them as goals for a-star.
          if rf_line_row(rf_line_idx)(row_idx:1) = "a"
            add 1 to as_table_len
            move nodes_idx to as_table(as_table_len)
          end-if
        end-if
      end-if
      set nodes_idx up by 1
    end-perform
  end-perform

  compute nodes_len = nodes_idx - 1
  display "Loaded: " nodes_len " nodes and " nodes_per_row " per row."

  *> Display new grid of numbers.
  *> display "nodes: [" no advancing
  *> perform varying nodes_idx from 1 by 1 until nodes_idx > nodes_len
  *>   display nodes_row(nodes_idx) no advancing
  *>   if nodes_idx < nodes_len display ", " no advancing end-if
  *> end-perform
  *> display "]"

  *> Run a-star for each 'a' as a goal.
  set as_table_idx to 1
  move goalPt to startPt
  perform varying as_table_idx from 1 by 1 until as_table_idx > as_table_len
    *> display "GOAL: " goalPt " NEW END A: " as_table(as_table_idx)

    *> initialize curr_neighbor
    *> initialize current
    *> set heuristic to entry "heuristic"
    *> call 'lib-astar' using goalPt as_table(as_table_idx) nodes heuristic curr_neighbor current path

    initialize path
    set get_neighbors to entry "get_neighbors"
    *> call 'lib-dijkstra' using startPt goalPt nodes_len path get_neighbors get_neighbors_stuff
    *> call 'lib-dijkstra' using goalPt as_table(as_table_idx) nodes_len path get_neighbors_stuff
    move as_table(as_table_idx) to goalPt
    call 'lib-dijkstra' using dijkstra_stuff

    *> display "FINISHED: " path_len
    *> See if this path is the shortest path.
    if (path_len > 0 and path_len < lowest_as) or lowest_as = 0
      move path_len to lowest_as
      move path to shortest_path
    end-if
  end-perform

  *> Show the shortest path!
  *> display "shortest_path: [" no advancing
  *> perform varying shortest_path_idx from 1 by 1 until shortest_path_idx > shortest_path_len
  *>   display shortest_path_val(shortest_path_idx) no advancing
  *>   if shortest_path_idx < shortest_path_len
  *>     display ", " no advancing
  *>     compute total = total + nodes_row(shortest_path_val(shortest_path_idx))
  *>   end-if
  *> end-perform
  *> display "]"

  display "SIZE: " shortest_path_len

  goback.



entry "get_neighbors"
  set curr_neighbors_num to 0
  *> Up
  compute neighbor_ptr = current_ptr - nodes_per_row
  *> display "UP" neighbor_ptr ": " nodes_row(neighbor_ptr)
  if neighbor_ptr > 0 perform add_neighbor end-if

  *> Down
  compute neighbor_ptr = current_ptr + nodes_per_row
  *> display "DOWN" neighbor_ptr ": " nodes_row(neighbor_ptr)
  if neighbor_ptr <= nodes_len perform add_neighbor end-if

  *> Left
  compute neighbor_ptr = current_ptr - 1
  *> Subtraction in mod(a - 1, b) because 0 vs 1 indexed collection.
  compute neighbor_rowpos = function mod(neighbor_ptr - 1, nodes_per_row)
  compute current_rowpos = function mod(current_ptr - 1, nodes_per_row)
  *> display "LEFT" neighbor_rowpos ": " nodes_row(neighbor_rowpos)
  *> display "LEFT" neighbor_ptr ": " nodes_row(neighbor_ptr)
  if neighbor_rowpos < current_rowpos and neighbor_rowpos >= 0 perform add_neighbor end-if

  *> Right
  compute neighbor_ptr = current_ptr + 1
  *> Subtraction in mod(a - 1, b) because 0 vs 1 indexed collection.
  compute neighbor_rowpos = function mod(neighbor_ptr - 1, nodes_per_row)
  compute current_rowpos = function mod(current_ptr - 1, nodes_per_row)
  *> display "RIGHT" neighbor_rowpos ": " nodes_row(neighbor_rowpos)
  *> display "RIGHT" neighbor_ptr ": " nodes_row(neighbor_ptr)
  if neighbor_rowpos > current_rowpos perform add_neighbor end-if
  .

add_neighbor.
  if nodes_row(neighbor_ptr) - nodes_row(current_ptr) <= 1
    add 1 to curr_neighbors_num
    move neighbor_ptr to curr_neighbor_ptr(curr_neighbors_num)
    move 1 to curr_neighbor_dist(curr_neighbors_num)
  end-if
  .


*> Compute the heuristic values for a-star.
*> entry "heuristic"
*> Worthless heuristic -- should be using something like Manhatttan distance at least!
*> *>   move nodes_row(curr_neighbor) to heuristic_return
*>   compute heuristic_return = nodes_row(curr_neighbor) - nodes_row(current)
*> *>   compute heuristic_return = nodes_row(current) - nodes_row(curr_neighbor)
*>   *> display "HEURISTIC!" curr_neighbor nodes_row(curr_neighbor) heuristic_return
*>   goback returning heuristic_return.

*> Originally, I had a-star callback here to check if a node was 'a' to see if it was time
*> to stop on the way to an arbitrary goal (node 1). But reverted to just assigning a new 'a' as a goal, above.
*> entry "check_original"
*>   compute check_row_num = current / nodes_per_row
*>   compute check_col_num = function mod(current nodes_per_row)
*>   move function ord(rf_line_row(check_row_num)(check_col_num:1)) to check_return
*> *>   display "RETURNING: " check_return " FOR LETTER " rf_line_row(check_row_num)(check_col_num:1) " IDX: " current
*>   goback returning check_return.
