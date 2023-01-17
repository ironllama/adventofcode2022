           >>source format free
identification division.
program-id. lib-dijkstra is initial.
author. alexoh@wcoding.

data division.
  *> local-storage should also get reset per call, but doesn't seem to work?
  local-storage section.
    77 minmax pic x(3).
    77 oper pic x(6).
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

    *> 01 current_rowpos pic s9(8) comp.
    *> 01 neighbor_rowpos pic s9(8) comp.

    *> Might be a waste of space as a sparse array, rather than map.
    01 cameFrom.
      02 cameFrom_len pic s9(8) comp.
      02 cameFrom_val usage is index
          occurs 0 to 999999 times depending on cameFrom_len
          indexed by cameFrom_idx.
    77 nextFound usage is index.

    77 temp_gScore pic s9(8) comp.
    77 temp_fScore pic s9(8) comp.

    77 current usage is index.
    77 neighbor usage is index.
    77 neighbor_dist pic s9.

  linkage section.
    01 ln-dijkstra_stuff.
      02 ln-startPt usage is index.
      02 ln-goalPt usage is index.
      02 ln-nodes_len pic s9(8) comp.
      *> 01 ln-nodes.
      *>   02 ln-nodes_len pic s9(8) comp value 0.
      *>   02 ln-nodes_per_row pic s9(4) comp.
      *>   02 ln-nodes_row pic s9(4) comp value 0 occurs 0 to unbounded
      *>       depending on ln-nodes_len indexed by ln-nodes_idx.
      *> 01 ln-current usage is index.
      02 ln-get_neighbors_stuff.
        03 ln-get_neighbors procedure-pointer.
        03 ln-current_ptr usage is index.
        03 ln-curr_neighbors_num pic s9 comp.
        03 ln-curr_neighbors occurs 9 times
            indexed by ln-curr_neighbors_idx.
          04 ln-curr_neighbor_ptr usage is index.
          04 ln-curr_neighbor_dist pic s9.
      02 ln-path.
        03 ln-path_len pic s9(8) comp value 0.
        03 ln-path_val usage is index occurs 0 to unbounded
            depending on ln-path_len indexed by ln-path_idx.


procedure division using ln-dijkstra_stuff.
  *> display "DIJKSTRA!"
  move 'min' to minmax
  move 'insert' to oper
  move 1 to openSet_new_key
  move ln-startPt to openSet_new_val
  call 'lib-heap' using minmax oper openSet openSet_new_item

  set gScore_idx to 1
  perform varying gScore_idx from 1 by 1 until gScore_idx > ln-nodes_len
    move 99999999 to gScore_val(gScore_idx)
  end-perform
  move 0 to gScore_val(ln-startPt)

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

    move openSet_new_val to ln-current_ptr
    move ln-current_ptr to current

    if current = ln-goalPt
      perform reconstruct_path
      goback
    end-if

    *> display ">>>>> CHECKING: KEY:" current
    call ln-get_neighbors

    perform varying ln-curr_neighbors_idx from 1 by 1 until ln-curr_neighbors_idx > ln-curr_neighbors_num
      move ln-curr_neighbor_ptr(ln-curr_neighbors_idx) to neighbor
      move ln-curr_neighbor_dist(ln-curr_neighbors_idx) to neighbor_dist
      perform check_neighbor
    end-perform

  end-perform

  goback.

check_neighbor.
  compute temp_gScore = gScore_val(current) + neighbor_dist
  *> display "GSCORE: [" neighbor "]: " temp_gScore " < " gScore_val(neighbor) " -- " gScore_val(current) " + " neighbor_dist
  if temp_gScore < gScore_val(neighbor)
    *> display "NEW LOW"
    add 1 to cameFrom_len  *> Is this even needed?
    move current to cameFrom_val(neighbor)

    move temp_gScore to gScore_val(neighbor)

    *> See if node is already in the heap. Should probably combine with an insert in future?
    move 'search' to oper
    move -1 to openSet_new_key
    move neighbor to openSet_new_val
    call 'lib-heap' using minmax oper openSet openSet_new_item

    *> display "TEST NEIGHBOR: " neighbor " " openSet_new_key
    *> If not already in the priority queue, add it.
    if openSet_new_key = 0
      *> display "NEW LOW INSERTING: " gScore_val(neighbor) neighbor
      move 'insert' to oper
      move temp_gScore to openSet_new_key
      move neighbor to openSet_new_val
      call 'lib-heap' using minmax oper openSet openSet_new_item
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
    add 1 to ln-path_len
    move nextFound to ln-path_val(ln-path_len)
    move cameFrom_val(nextFound) to nextFound
    *> display "NEXT FOUND: " nextFound
  end-perform
  .
