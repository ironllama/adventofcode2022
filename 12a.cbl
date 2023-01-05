           >>source format free
identification division.
program-id. 12a.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_line_cnt pic s9(8) comp value 0.
      02 rf_line_row pic x(9999) occurs 0 to 9999 times
          depending on rf_line_cnt indexed by rf_line_idx.

    77 curr_letter pic x.
    77 row_idx pic s9(4) comp.
    *> 77 heuristic_return pic s9(8) comp.

    77 check_row_num pic s9(4) comp.
    77 check_col_num pic s9(4) comp.
    77 check_return pic x.

    01 nodes.
    *>   02 nodes_len pic s9(8) comp value 0.
      02 nodes_per_row pic s9(4) comp.
      02 nodes_row pic s9(4) comp value 0 occurs 0 to 99999 times
          depending on nodes_len indexed by nodes_idx.

    01 neighbor_ptr usage is index.
    01 current_rowpos usage is index.
    01 neighbor_rowpos usage is index.

    *> For lib-astar/dijkstra
    01 startPt usage is index.
    01 goalPt usage is index.
    01 nodes_len pic s9(8) comp.

    01 get_neighbors procedure-pointer.
    01 get_neighbors_stuff.
      02 current_ptr usage is index.
      02 curr_neighbors_num pic s9 comp.
      02 curr_neighbors occurs 4 times indexed by curr_neighbors_idx.
        03 curr_neighbor_ptr usage is index.
        03 curr_neighbor_dist pic s9.
    01 path.
      02 path_len pic s9(8) comp value 0.
      02 path_val usage is index occurs 0 to 99999 times
          depending on path_len indexed by path_idx.


procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
*>   call 'lib-readdata' using function module-id ".da1" rf_all_lines

  move length of function trim(rf_line_row(1)) to nodes_per_row
  *> display "LENGTH: " nodes_per_row

  *> Find START and END, and populate the grid as numbers for a-star.
  set nodes_idx to 1
  perform varying rf_line_idx from 1 by 1 until rf_line_idx > rf_line_cnt
    *> display "LINE: " function trim(rf_line_row(rf_line_idx))
    perform varying row_idx from 1 by 1 until row_idx > nodes_per_row
      move rf_line_row(rf_line_idx)(row_idx:1) to curr_letter
      if curr_letter = "S"
        move nodes_idx to startPt
        move 0 to nodes_row(nodes_idx)
        *> display "START IDX: " nodes_idx
      else
        if curr_letter = "E"
          move nodes_idx to goalPt
          move 26 to nodes_row(nodes_idx)
          *> display "END IDX: " nodes_idx
        else
          *> Even though ASCII 'a' is 97, COBOL has 'a' at 98?!?!
          *> display rf_line_row(rf_line_idx)(row_idx:1) ": " function ord(rf_line_row(rf_line_idx)(row_idx:1))
          compute nodes_row(nodes_idx) = function ord(rf_line_row(rf_line_idx)(row_idx:1)) - 97
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

*>   set heuristic to entry "heuristic"
*>   call 'lib-astar' using startPt goalPt nodes heuristic curr_neighbor current path

  set get_neighbors to entry "get_neighbors"
  call 'lib-dijkstra' using startPt goalPt nodes_len path get_neighbors get_neighbors_stuff

*>   *> Show the shortest path values!
*>   display "path: [" no advancing
*>   perform varying path_idx from 1 by 1 until path_idx > path_len
*>     display path_val(path_idx) no advancing
*>     if path_idx < path_len
*>       display ", " no advancing
*>     end-if
*>   end-perform
*>   display "]"
*>   *> Show the shortest path!
*>   perform varying rf_line_idx from 1 by 1 until rf_line_idx > rf_line_cnt
*>     perform varying row_idx from 1 by 1 until row_idx > nodes_per_row
*>       set path_idx to 1
*>       search path_val varying path_idx
*>         at end display "." no advancing
*>         when path_val(path_idx) = ((rf_line_idx - 1) * nodes_per_row) + row_idx
*>         *>   display "#" no advancing
*>           if path_val(path_idx) = path_val(path_idx - 1) - 1
*>             display ">" no advancing
*>           else
*>             if path_val(path_idx) = path_val(path_idx - 1) + 1
*>              display "<" no advancing
*>            else
*>              if path_val(path_idx) < (rf_line_cnt - 1) * nodes_per_row and path_val(path_idx) = path_val(path_idx - 1) - nodes_per_row
*>                display "v" no advancing
*>              else
*>                if path_val(path_idx) > nodes_per_row and path_val(path_idx) = path_val(path_idx - 1) + nodes_per_row
*>                  display "^" no advancing
*>                else
*>                  display "?" no advancing
*>                end-if
*>              end-if
*>             end-if
*>           end-if
*>       end-search
*>     end-perform
*>     display space
*>   end-perform

  display "SIZE: " path_len

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
  *> compute heuristic_return = nodes_row(curr_neighbor) - nodes_row(current)
  *> *> display "HEURISTIC!" curr_neighbor nodes_row(curr_neighbor) heuristic_return
  *> goback returning heuristic_return.
