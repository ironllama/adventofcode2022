           >>source format free
identification division.
program-id. lib-astar is initial.
*> The 'is initial' sets everything back to initial state per call.

data division.
  *> local-storage should also get reset per call, but doesn't seem to work?
  local-storage section.
    01 minmax pic x(3).
    01 oper pic x(6).
    01 openSet.
      02 openSet_cnt pic s9(8) comp value 0.
      02 openSet_item occurs 1 to 999999
          depending on openSet_cnt indexed by openSet_idx.
        03 openSet_item_key pic s9(8) comp.
        03 openSet_item_val pic s9(8) comp.
    01 openSet_new_item.
      02 openSet_new_key pic s9(8) comp.
      02 openSet_new_val pic s9(8) comp.

    01 gScore.
      02 gScore_len pic s9(8) comp.
      02 gScore_val pic s9(8) comp value 99999999
          occurs 0 to 999999 times
          depending on gScore_len indexed by gScore_idx.
    01 fScore.
      02 fScore_len pic s9(8) comp.
      02 fScore_val pic s9(8) comp value 99999999
          occurs 0 to 999999 times
          depending on fScore_len indexed by fScore_idx.

    01 current pic s9(8) comp.
    01 current_rowpos pic s9(8) comp.
    01 neighbor_rowpos pic s9(8) comp.
    01 temp_gScore pic s9(8) comp.
    01 temp_fScore pic s9(8) comp.

    01 heuristic pic s9(8) comp.
    01 original_val pic s9(4) comp.

    *> Might be a waste of space as a sparse array, rather than map.
    01 cameFrom.
      02 cameFrom_len pic s9(8) comp.
      02 cameFrom_val pic s9(8) comp value 0
          occurs 0 to 999999 times depending on cameFrom_len
          indexed by cameFrom_idx.
    01 nextFound pic s9(8) comp.

  linkage section.
    01 ln-startPt pic s9(8) comp.
    01 ln-goalPt pic s9(8) comp.
    01 ln-nodes.
      02 ln-nodes_len pic s9(8) comp value 0.
      02 ln-nodes_per_row pic s9(4) comp.
      02 ln-nodes_row pic s9(4) comp value 0 occurs 0 to unbounded
          depending on ln-nodes_len indexed by ln-nodes_idx.
    01 ln-heuristic procedure-pointer.
    01 ln-curr_neighbor pic s9(8) comp.
    01 ln-current pic s9(8) comp.
    01 ln-path.
      02 ln-path_len pic s9(8) comp value 0.
      02 ln-path_val pic s9(8) comp value 0 occurs 0 to unbounded
          depending on ln-path_len indexed by ln-path_idx.


procedure division using ln-startPt ln-goalPt ln-nodes ln-heuristic ln-curr_neighbor ln-current ln-path.
  *> display "LIB-ASTAR!"
  move 'min' to minmax
  move 'insert' to oper
  move 1 to openSet_new_key
  move ln-startPt to openSet_new_val
  call 'lib-heap' using minmax oper openSet openSet_new_item

  set gScore_idx to 1
  perform varying gScore_idx from 1 by 1 until gScore_idx > ln-nodes_len
    move 99999999 to gScore_val(gScore_idx)
  end-perform
  move 0 to gScore_val(1)

  set fScore_idx to 1
  perform varying fScore_idx from 1 by 1 until fScore_idx > ln-nodes_len
    move 99999999 to fScore_val(fScore_idx)
  end-perform
  move 0 to fScore_val(1)

  perform until openSet_cnt < 1
    *> display "OPENSET: [" no advancing
    *> perform varying openSet_idx from 1 by 1 until openSet_idx > openSet_cnt
    *>   display openSet_item_key(openSet_idx) ":" openSet_item_val(openSet_idx) no advancing
    *>   if openSet_idx < openSet_cnt
    *>     display ", " no advancing
    *> end-perform
    *> display "]"

    move 'next' to oper
    call 'lib-heap' using minmax oper openSet openSet_new_item

    move openSet_new_val to current

    if current = ln-goalPt
      perform reconstruct_path
      goback
    end-if

    *> display ">>>>> CHECKING: KEY:" current
    *> Up
    compute ln-curr_neighbor = current - ln-nodes_per_row
    *> display "UP" ln-curr_neighbor ": " ln-nodes_row(ln-curr_neighbor)
    if ln-curr_neighbor > 0 perform check_neighbor end-if

    *> Down
    compute ln-curr_neighbor = current + ln-nodes_per_row
    *> display "DOWN" ln-curr_neighbor ": " ln-nodes_row(ln-curr_neighbor)
    if ln-curr_neighbor <= ln-nodes_len perform check_neighbor end-if

    *> Left
    compute ln-curr_neighbor = current - 1
    *> Subtraction in mod(a - 1, b) because 0 vs 1 indexed collection.
    compute neighbor_rowpos = function mod(ln-curr_neighbor - 1, ln-nodes_per_row)
    compute current_rowpos = function mod(current - 1, ln-nodes_per_row)
    *> display "LEFT" neighbor_rowpos ": " ln-nodes_row(neighbor_rowpos)
    *> display "LEFT" ln-curr_neighbor ": " ln-nodes_row(ln-curr_neighbor)
    if neighbor_rowpos < current_rowpos and neighbor_rowpos >= 0 perform check_neighbor end-if

    *> Right
    compute ln-curr_neighbor = current + 1
    *> Subtraction in mod(a - 1, b) because 0 vs 1 indexed collection.
    compute neighbor_rowpos = function mod(ln-curr_neighbor - 1, ln-nodes_per_row)
    compute current_rowpos = function mod(current - 1, ln-nodes_per_row)
    *> display "RIGHT" neighbor_rowpos ": " ln-nodes_row(neighbor_rowpos)
    *> display "RIGHT" ln-curr_neighbor ": " ln-nodes_row(ln-curr_neighbor)
    if neighbor_rowpos > current_rowpos perform check_neighbor end-if

  end-perform

  goback.

check_neighbor.
  *> display "CHECKING: " ln-nodes_row(ln-curr_neighbor) " - " ln-nodes_row(current)
  *> This if-then-end-if only exists for AoC2022-Day12.
  if ln-nodes_row(ln-curr_neighbor) - ln-nodes_row(current) <= 1
   compute temp_gScore = gScore_val(current) + ln-nodes_row(ln-curr_neighbor)  *> See above.
   *> display "GSCORE: [" ln-curr_neighbor "]: " temp_gScore " < " gScore_val(ln-curr_neighbor) " " gScore_val(current) " " ln-nodes_row(ln-curr_neighbor)
   if temp_gScore < gScore_val(ln-curr_neighbor)
     *> display "NEW LOW"
     compute cameFrom_len = cameFrom_len + 1
     move current to cameFrom_val(ln-curr_neighbor)

     move temp_gScore to gScore_val(ln-curr_neighbor)
     *> compute heuristic = (ln-nodes_per_row
     *>     - function mod(ln-goalPt, ln-nodes_per_row)
     *>     - function mod(ln-curr_neighbor, ln-nodes_per_row))
     *>     + (ln-goalPt / ln-nodes_per_row) - (ln-curr_neighbor / ln-nodes_per_row)
     move current to ln-current
     call ln-heuristic returning heuristic

     compute fScore_val(ln-curr_neighbor) = temp_gScore + heuristic

     move 'search' to oper
     move -1 to openSet_new_key
     move ln-curr_neighbor to openSet_new_val
     call 'lib-heap' using minmax oper openSet openSet_new_item

     if openSet_new_key = 0
       *> display "NEW LOW INSERTING: " fScore_val(ln-curr_neighbor) ln-curr_neighbor
       move 'insert' to oper
       move fScore_val(ln-curr_neighbor) to openSet_new_key
       move ln-curr_neighbor to openSet_new_val
       call 'lib-heap' using minmax oper openSet openSet_new_item
     end-if
   end-if
  end-if
  .

reconstruct_path.
  move current to nextFound
*>   display "RECONSTRUCT: [" no advancing
*>   perform varying cameFrom_idx from 1 by 1 until cameFrom_idx > cameFrom_len
*>     display cameFrom_val(cameFrom_idx) ", " no advancing
*>   end-perform
*>   display "]"
*>   display "LENGTH: " cameFrom_len

  perform until nextFound = ln-startPt or ln-path_len > cameFrom_len
    compute ln-path_len = ln-path_len + 1
    move nextFound to ln-path_val(ln-path_len)
    move cameFrom_val(nextFound) to nextFound
    *> display "NEXT FOUND: " nextFound
  end-perform
  .
