           >>source format free
identification division.
program-id. 24a.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_cnt pic s9(8) comp value 0.
      02 rf_row pic x(9999) occurs 0 to 9999 times
          depending on rf_cnt indexed by rf_i.
    77 rf_row_len pic 9(8) comp.
    77 rf_col_i usage is index.

    01 borderless.
      02 bl_r_cnt pic 9(8) comp.
      02 bl_r occurs 25 times indexed by bl_r_i.
        03 bl_c_cnt pic 9(8) comp.
        03 bl_c pic x occurs 120 times indexed by bl_c_i.

    77 c_x pic 9(3) comp.
    77 c_y pic 9(3) comp.
    01 n_val.
      02 n_y pic 9(3) comp.
      02 n_x pic 9(3) comp.
      02 n_t pic 9(3) comp.

    01 gfs_stuff.
      02 gfs_m pic 9(8) comp.
      02 gfs_g pic 9(8) comp.
      02 gfs_w pic 9(8).
      02 gfs_ws redefines gfs_w.
        03 filler pic 9(7).
        03 gfs_w1 pic 9.
      02 gfs_d pic x.
      02 gfs_t1 pic 9(8) comp.
      02 gfs_t2 pic 9(8) comp.

    77 total_found pic s9(8) comp.

    *> 01 t_stuff.
    *>   02 t_head usage is index.
    *>   02 t_num pic 9(8) comp.
    *>   02 t_val occurs 9999999 times indexed by t_idx.
    *>     03 t_x pic 9(3) comp.
    *>     03 t_y pic 9(3) comp.
    *>     03 t_t pic 9(3) comp.
    *>   02 t_found_ind pic x value 'N'.
    *>     88 t_found value 'Y'.
    *>     88 t_not_found value 'N'.

    01 heap_stuff.
      02 minmax pic x(3).
      02 oper pic x(6).
      02 h_new_item.
        03 h_new_key pic s9(8) comp.
        03 h_new_val.
          04 h_new_y pic s9(3) comp.
          04 h_new_x pic s9(3) comp.
          04 h_new_t pic s9(3) comp.
      02 openSet.
        03 h_cnt pic s9(8) comp value 0.
        03 h_item occurs 999999 indexed by h_idx.
          04 h_item_key pic s9(8) comp.
          04 h_item_val.
            05 h_item_y pic s9(3) comp.
            05 h_item_x pic s9(3) comp.
            05 h_item_t pic s9(3) comp.


procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
*>   call 'lib-readdata' using function module-id ".da1" rf_all_lines
*>   call 'lib-readdata' using function module-id ".da2" rf_all_lines

  move 0 to total_found
  move function length(function trim(rf_row(1))) to rf_row_len

  *> New map w/out the borders. Assuming all lines are the same length.
  perform varying rf_i from 2 by 1 until rf_i > (rf_cnt - 1)
    add 1 to bl_r_cnt
    perform varying rf_col_i from 2 by 1
        until rf_col_i > (rf_row_len - 1)
      add 1 to bl_c_cnt(bl_r_cnt)
      move rf_row(rf_i)(rf_col_i:1) to bl_c(bl_r_cnt bl_c_cnt(bl_r_cnt))
    end-perform
  end-perform

  *> *> Add the starting node -- Queue
  *> add 1 to t_num
  *> move 0 to t_y(t_num)
  *> move 0 to t_x(t_num)
  *> move 0 to t_t(t_num)
  *> set t_head to 0

  *> perform until t_head > t_num
  *>   add 1 to t_head

  *>   *> display "STACK: [" t_head "]: " t_y(t_head) " " t_x(t_head) " " t_t(t_head) " REM: " t_num
  *>   move t_y(t_head) to c_y
  *>   move t_x(t_head) to c_x
  *>   compute n_t = t_t(t_head) + 1

  *> Add the starting node -- Min-Heap
  move 'min' to minmax
  move 'insert' to oper
  move 1 to h_new_key
  move 0 to h_new_y
  move 0 to h_new_x
  move 0 to h_new_t
  call 'lib-heap-time' using heap_stuff

  perform until h_cnt < 1
    move 'next' to oper
    call 'lib-heap-time' using heap_stuff

    *> display "HEAP: [" h_new_key "]: " h_new_y " " h_new_x " " h_new_t " REM: " h_cnt
    move h_new_y to c_y
    move h_new_x to c_x
    compute n_t = h_new_t + 1


    if c_y = bl_r_cnt and c_x = bl_c_cnt(1)
      *> display "END: " n_t " STACK: " t_num
      *> display "END: " n_t " HEAP: " h_cnt

      move n_t to total_found
      display "FINAL: " total_found
      goback  *> Stop the loop. Eww?
    else
      *> Wait until opening, and then require to go. Otherwise, you'll wait forever?
      if c_y = 0 and c_x = 0
        move 1 to n_y
        move 1 to n_x

        perform get_future_state_of_spot
        if gfs_w = 0
          *> display "START: " n_t
          perform add_neighbor
        else
          move c_x to n_x
          move c_y to n_y
          perform add_neighbor
        end-if
      else

        *> Right
        if c_x < bl_c_cnt(1)
          move c_y to n_y
          add 1 to c_x giving n_x
          perform add_neighbor
        end-if

        *> Down
        if c_y < bl_r_cnt
          add 1 to c_y giving n_y
          move c_x to n_x
          perform add_neighbor
        end-if

        *> Up - Do not go back up from the first position. No point -- could have just waited
        if c_y > 1
          subtract 1 from c_y giving n_y
          move c_x to n_x
          perform add_neighbor
        end-if

        *> Left
        if c_x > 1
          move c_y to n_y
          subtract 1 from c_x giving n_x
          perform add_neighbor
        end-if

        *> Wait
        *> move current_ptr to neighbor_ptr
        if c_x > 0 and c_y > 0
          move c_x to n_x
          move c_y to n_y
          perform add_neighbor
        end-if
      end-if
    end-if
  end-perform

  goback.


add_neighbor.
  if n_y >= 0 and n_y <= bl_r_cnt and n_x >= 0 and n_x <= bl_c_cnt(1)
    perform get_future_state_of_spot
    *> display "CHECKING: Y: " n_y " X: " n_x " T: " n_t " GFS_W: " gfs_w
    if gfs_w = 0
      *> *> Skip if this has already been visited.
      *> *> 1:35
      *> set t_not_found to true
      *> perform varying t_idx from 1 by 1 until t_idx > t_num or t_found
      *>   if t_val(t_idx) = n_val
      *>     set t_found to true
      *> end-perform
      *> *> 0:40
      *> set t_not_found to true
      *> perform varying t_idx from t_num by -1 until t_idx < 1 or t_found
      *>   if t_val(t_idx) = n_val
      *>     set t_found to true
      *> end-perform
      *> *> ???
      *> set t_idx to 1
      *> search t_val
      *>   at end set t_not_found to true
      *>   when t_val(t_idx) = n_val
      *>     set t_found to true
      *> end-search

      *> if t_not_found
      *>   add 1 to t_num
      *>   move n_val to t_val(t_num)
      *> end-if

      *> Skip if this has already been visited.
      *> 0:03
      move 'search' to oper
      move -1 to h_new_key
      move n_val to h_new_val
      call 'lib-heap-time' using heap_stuff

      if h_new_key = 0
        move 'insert' to oper
        *> compute h_new_key = (bl_c_cnt(1) - n_x) + (bl_r_cnt - n_y)  *> Closer is better?
        move n_t to h_new_key  *> Basically queue, but with heap.
        move n_val to h_new_val
        *> display "NEW STATE INSERTING: h_ret: " h_ret " PTR: " n_y " " n_x " TIME: " n_t
        call 'lib-heap-time' using heap_stuff
      end-if
   end-if
  end-if
  .

get_future_state_of_spot.
  move 0 to gfs_w
  move space to gfs_d

  *> Check n_t to the left to see if any ">"
  move function mod(n_t bl_c_cnt(1)) to gfs_m
  compute gfs_t1 = bl_c_cnt(1) - (gfs_m - n_x)
  compute gfs_t2 = n_x - gfs_m
  if (gfs_m >= n_x and bl_c(n_y gfs_t1) = ">")
      or (gfs_m < n_x and bl_c(n_y gfs_t2) = ">")
    add 1 to gfs_w
    move ">" to gfs_d
  end-if

  *> Check n_t to the right to see if any "<"
  move function mod(n_t bl_c_cnt(1)) to gfs_m
  compute gfs_g = bl_c_cnt(1) - n_x
  compute gfs_t1 = gfs_m - gfs_g
  compute gfs_t2 = n_x + gfs_m
  if (gfs_m > gfs_g and bl_c(n_y gfs_t1) = "<")
      or (gfs_m <= gfs_g and bl_c(n_y gfs_t2) = "<")
    add 1 to gfs_w
    move "<" to gfs_d
  end-if

  *> Assuming y value goes up as it goes cardinally south.
  *> Check n_t to the top to see if any "v"
  move function mod(n_t bl_r_cnt) to gfs_m
  compute gfs_t1 = bl_r_cnt - (gfs_m - n_y)
  compute gfs_t2 = n_y - gfs_m
  if (gfs_m >= n_y and bl_c(gfs_t1 n_x) = "v")
      or (gfs_m < n_y and bl_c(gfs_t2 n_x) = "v")
    add 1 to gfs_w
    move "v" to gfs_d
  end-if

  *> Check n_t to the bottom to see if any "^"
  move function mod(n_t bl_r_cnt) to gfs_m
  compute gfs_g = bl_r_cnt - n_y
  compute gfs_t1 = gfs_m - gfs_g
  compute gfs_t2 = n_y + gfs_m
  if (gfs_m > gfs_g and bl_c(gfs_t1 n_x) = "^")
      or (gfs_m <= gfs_g and bl_c(gfs_t2 n_x) = "^")
    add 1 to gfs_w
    move "^" to gfs_d
  end-if
  .



*> >>>>>>>>>>>>>>>>>>>>>>>> CODE GRAVEYARD <<<<<<<<<<<<<<<<<<<<<<<<<<<<<

  *> display "BORDERLESS:"
  *> perform varying bl_r_i from 1 by 1 until bl_r_i > bl_r_cnt
  *>   perform varying bl_c_i from 1 by 1
  *>       until bl_c_i > bl_c_cnt(bl_r_i)
  *>     display bl_c(bl_r_i bl_c_i) no advancing
  *>   end-perform
  *>   display space
  *> end-perform
  *> display space

*>   Testing against sample.
*>   perform varying n_t from 0 by 1 until n_t > 18
*>     display "T+ " n_t ": "
*>     perform varying bl_r_i from 1 by 1 until bl_r_i > bl_r_cnt
*>       perform varying bl_c_i from 1 by 1
*>           until bl_c_i > bl_c_cnt(bl_r_i)
*>         move bl_r_i to c_y
*>         move bl_c_i to c_x
*>         perform get_future_state_of_spot
*>         evaluate gfs_w
*>           when 0 display "." no advancing
*>           when 1 display gfs_d no advancing
*>           when other display gfs_w1 no advancing
*>         end-evaluate
*>         *> display bl_c(bl_r_i bl_c_i) no advancing
*>       end-perform
*>       display space
*>     end-perform
*>     display space
*>   end-perform


    *> Print current state of map
    *> if current_ptr = nodes_len move bl_r_cnt to c_y
    *> else compute c_y = ((current_ptr - 1) / bl_c_cnt(1)) + 1
    *> end-if
    *> compute c_x = function mod(current_ptr, bl_c_cnt(1))
    *> if c_x = 0 and current_ptr <> 0 move bl_c_cnt(1) to c_x
    *> end-if
    *> compute n_t = current_time
    *> perform print_grid


    *> 01 print_stuff.
    *>   02 p_x pic 9(3) comp.
    *>   02 p_y pic 9(3) comp.
    *>   02 p_t pic 9(8) comp.

*> print_grid.
*>   compute p_t = function mod(n_t, f_lcm)
*>   if p_t = 0
*>     move f_lcm to p_t
*>   end-if
*>   display "PRINT: " p_t
*>   perform varying bl_r_i from 1 by 1 until bl_r_i > bl_r_cnt
*>     perform varying bl_c_i from 1 by 1
*>         until bl_c_i > bl_c_cnt(bl_r_i)
*>       if bl_r_i = c_y and bl_c_i = c_x
*>         display "E" no advancing
*>       else
*>         display f_col(p_t, bl_r_i, bl_c_i) no advancing
*>       end-if
*>     end-perform
*>     display space
*>   end-perform
*>   .


    *> 01 future_maps.
    *>   02 f_lcm pic 9(8) comp.
    *>   02 f_lcm_i pic 9(8) comp.
    *>   02 f_num pic 9(8) comp.
    *>   02 f_map occurs 600 times indexed by f_map_i.
    *>     03 f_row_cnt pic 9(8) comp.
    *>     03 f_row occurs 25 times indexed by f_row_i.
    *>       04 f_col_cnt pic 9(8) comp.
    *>       04 f_col pic x occurs 120 times indexed by f_col_i.

    *> *> For lib-dijkstra-time/lib-astar-time
    *> *> 01 dijkstra_stuff.
    *> 01 astar_stuff.
    *>   02 startPt usage is index.
    *>   02 goalPt usage is index.
    *>   02 nodes_len pic s9(8) comp.
    *>   02 heuristic procedure-pointer.
    *>   02 h_ptr pic s9(4) comp.
    *>   02 h_ret pic s9(8) comp.
    *>   02 get_neighbors_stuff.
    *>     03 get_neighbors procedure-pointer.
    *>     03 current_ptr pic s9(4) comp.
    *>     03 current_time pic s9(4) comp.
    *>     03 curr_neighbors_num pic s9 comp.
    *>     03 curr_neighbors occurs 9 times
    *>         indexed by curr_neighbors_idx.
    *>       04 curr_neighbor pic s9(8) comp.
    *>       04 curr_neighbor_stuff redefines curr_neighbor.
    *>         05 curr_neighbor_ptr pic s9(4) comp.
    *>         05 curr_neighbor_time pic s9(4) comp.
    *>       04 curr_neighbor_cost pic s9.
    *>   02 path.
    *>     03 path_len pic s9(8) comp value 0.
    *>     03 path_val pic s9(4) occurs 9999 times
    *>         indexed by path_idx.

    *> 01 nodes_len pic s9(8) comp.
    *> 77 neighbor_rowpos usage is index.
    *> 77 current_rowpos usage is index.


  *> LCM using the Greatest Common Factor (GCF)
*>   perform varying f_lcm_i from 2 by 1 until f_lcm > 0
*>     if function mod(bl_r_cnt f_lcm_i) = 0
*>         and function mod(bl_c_cnt(1) f_lcm_i) = 0
*>       compute f_lcm = (bl_r_cnt * bl_c_cnt(1)) / f_lcm_i
*>   end-perform
*>   display "LCM: " f_lcm

  *> Generate all possible maps
*>   perform varying n_t from 1 by 1 until n_t > f_lcm
*>     perform varying bl_r_i from 1 by 1 until bl_r_i > bl_r_cnt
*>       *> display "ROW: " bl_r_i " COL_CNT: " bl_c_cnt(bl_r_i)
*>       perform varying bl_c_i from 1 by 1
*>           until bl_c_i > bl_c_cnt(bl_r_i)
*>         move bl_r_i to c_y
*>         move bl_c_i to c_x
*>         perform get_future_state_of_spot
*>         evaluate gfs_w
*>           when 0 move '.' to f_col(n_t bl_r_i bl_c_i)
*>           when 1 move gfs_d to f_col(n_t bl_r_i bl_c_i)
*>           when other move gfs_w1 to f_col(n_t bl_r_i bl_c_i)
*>         end-evaluate
*>         *> display "ADDED: " f_col(n_t bl_r_i bl_c_i) " n_t: " n_t " row: " bl_r_i " col: " bl_c_i
*>       end-perform
*>     end-perform
*>   end-perform


*> compute nodes_len = bl_r_cnt * bl_c_cnt(1)


*>   initialize path
*>   move 0 to startPt
*>   compute goalPt = bl_r_cnt * bl_c_cnt(1)
*>   move goalPt to nodes_len
*>   set get_neighbors to entry "get_neighbors"
*> *>   call 'lib-dijkstra-time' using dijkstra_stuff
*>   set heuristic to entry "heuristic"
*>   call 'lib-astar-time' using astar_stuff

*>   perform varying path_idx from path_len by -1 until path_idx < 0
*>     compute tmp_y = ((path_val(path_idx) - 1) / bl_c_cnt(1)) + 1
*>     compute tmp_x = function mod(path_val(path_idx) bl_c_cnt(1))
*>     if tmp_x = 0
*>       move bl_c_cnt(1) to tmp_x
*>     end-if

*>     compute tmp_t = path_len - path_idx + 1
*>     display "T+ " tmp_t ": " path_val(path_idx) " Y: " tmp_y " X: " tmp_x

*>     perform varying bl_r_i from 1 by 1 until bl_r_i > bl_r_cnt
*>       perform varying bl_c_i from 1 by 1
*>           until bl_c_i > bl_c_cnt(bl_r_i)
*>         if bl_r_i = tmp_y and bl_c_i = tmp_x
*>           display "E" no advancing
*>         else
*>           move bl_r_i to c_y
*>           move bl_c_i to c_x
*>           move tmp_t to n_t
*>           perform get_future_state_of_spot
*>           evaluate gfs_w
*>             when 0 display "." no advancing
*>             when 1 display gfs_d no advancing
*>             when other display gfs_w1 no advancing
*>           end-evaluate
*>           *> display bl_c(bl_r_i bl_c_i) no advancing
*>         end-if
*>       end-perform
*>       display space
*>     end-perform
*>     display space
*>   end-perform

*>   display "TIME: " current_time " PATH_LEN: " path_len
*>   compute total_found = current_time + 1
*>   display "FINAL: " total_found


        *> Position (neighbor_ptr) here based on a FLATTENED matrix.
        *> Right
        *> compute current_rowpos = function mod(current_ptr bl_c_cnt(1))
        *> if current_rowpos = 0
        *>   move bl_c_cnt(1) to current_rowpos
        *> end-if
        *> if current_rowpos <> bl_c_cnt(1) and current_rowpos <> 0
        *> if current_rowpos <> 0
        *>   compute neighbor_ptr = current_ptr + 1

        *> Down
        *> compute neighbor_ptr = current_ptr + bl_c_cnt(1)
        *> if neighbor_ptr <= nodes_len
        *>   compute neighbor_ptr = current_ptr + bl_c_cnt(1)

        *> Up - Do not go back up from the first position. No point -- could have just waited
        *> if current_ptr <> 1
        *>   compute neighbor_ptr = current_ptr - bl_c_cnt(1)
        *>   if neighbor_ptr > 0 perform add_neighbor end-if

        *> Left
        *> compute current_rowpos = function mod(current_ptr bl_c_cnt(1))
        *> if current_rowpos = 0
        *>   move bl_c_cnt(1) to current_rowpos
        *> end-if
        *> if current_rowpos > 1 perform add_neighbor end-if
        *> if current_rowpos <> 1
        *>   compute neighbor_ptr = current_ptr - 1


*> *> Compute the heuristic values for a-star.
*> entry "heuristic"
*>   compute tmp_y = ((h_ptr - 1) / bl_c_cnt(1)) + 1
*>   compute tmp_x = function mod(h_ptr bl_c_cnt(1))
*>   if tmp_x = 0 and h_ptr <> 0
*>     move bl_c_cnt(1) to tmp_x
*>   end-if

*>   compute h_ret = (bl_c_cnt(1) - tmp_x) + (bl_r_cnt - tmp_y)
*>   display "HEURISTIC!" h_ret " PTR: " h_ptr
*>   goback
*>   .

*> *> For lib-dijkstra.
*> entry "get_neighbors"
*>   *> display "GET NEIGHBORS for C_PTR: " current_ptr
*>   set curr_neighbors_num to 0

*>   if current_ptr = 0
*>     *> Wait until opening, and then require to go. Otherwise, you'll wait forever?
*>     move 1 to c_y
*>     move 1 to c_x
*>     compute n_t = current_time + 1
*>     perform get_future_state_of_spot

*>     if gfs_w = 0
*>       move 1 to neighbor_ptr
*>       perform add_neighbor
*>     else
*>       move current_ptr to neighbor_ptr
*>       perform add_neighbor
*>     end-if
*>   else
*>     *> Position (neighbor_ptr) here based on a FLATTENED matrix.

*>     *> Right
*>     compute neighbor_ptr = current_ptr + 1
*>     compute current_rowpos = function mod(current_ptr bl_c_cnt(1))
*>     *> if current_rowpos = 0  *> Since modding row's max pos will produce 0
*>     *>   compute neighbor_ptr = neighbor_ptr - bl_c_cnt(1)
*>     *> end-if
*>     *> *> Subtraction in mod(a - 1, b) because 0 vs 1 indexed collection.
*>     *> *> compute neighbor_rowpos = function mod(neighbor_ptr - 1, bl_c_cnt(1))
*>     *> *> compute current_rowpos = function mod(current_ptr - 1, bl_c_cnt(1))
*>     *> *> display "RIGHT" neighbor_rowpos ": " nodes_row(neighbor_rowpos)
*>     *> *> display "RIGHT" neighbor_ptr ": " nodes_row(neighbor_ptr)
*>     *> if neighbor_ptr > nodes_len
*>     *>   move 1 to neighbor_ptr
*>     *> end-if
*>     *> perform add_neighbor
*>     if current_rowpos < bl_c_cnt(1) perform add_neighbor end-if

*>     *> Down
*>     compute neighbor_ptr = current_ptr + bl_c_cnt(1)
*>     *> display "DOWN" neighbor_ptr ": " nodes_row(neighbor_ptr)
*>     *> If you can cycle.
*>     *> if neighbor_ptr > nodes_len
*>       *> compute neighbor_ptr = bl_c_cnt(1) - (nodes_len - current_ptr)
*>     *> end-if
*>     *> perform add_neighbor
*>     if neighbor_ptr <= nodes_len perform add_neighbor end-if

*>     *> Up - Do not go back up from the first position. No point -- could have just waited
*>     if current_ptr <> 1
*>       compute neighbor_ptr = current_ptr - bl_c_cnt(1)
*>       *> If you can cycle.
*>       *> if neighbor_ptr < 1
*>       *>   compute neighbor_ptr = nodes_len - (bl_c_cnt(1) - current_ptr)
*>       *> end-if
*>       *> perform add_neighbor
*>       *> display "UP" neighbor_ptr ": " nodes_row(neighbor_ptr)
*>       if neighbor_ptr > 0 perform add_neighbor end-if
*>     end-if

*>     *> Left
*>     compute neighbor_ptr = current_ptr - 1
*>     compute current_rowpos = function mod(current_ptr bl_c_cnt(1))
*>     *> if current_rowpos = 1
*>     *>   compute neighbor_ptr = neighbor_ptr + bl_c_cnt(1)
*>     *> end-if
*>     *> *> Subtraction in mod(a - 1, b) because 0 vs 1 indexed collection.
*>     *> *> compute neighbor_rowpos = function mod(neighbor_ptr - 1, bl_c_cnt(1))
*>     *> *> display "LEFT" neighbor_rowpos ": " nodes_row(neighbor_rowpos)
*>     *> *> display "LEFT" neighbor_ptr ": " nodes_row(neighbor_ptr)
*>     *> perform add_neighbor
*>     if current_rowpos > 1 perform add_neighbor end-if

*>     *> Wait
*>     move current_ptr to neighbor_ptr
*>     perform add_neighbor
*>   end-if

*>   goback
*>   .


*> add_neighbor.
  *> display "ADD_NEIGHBOR_START"
  *> get_future_state_of_spot expects X, Y
  *> TODO: If it's the last element, no need to check -- but is the if too freq overall?
*>   if neighbor_ptr = nodes_len
*>     move bl_r_cnt to c_y
*>   else
    *> Subtraction in (a - 1 / b) because last item in each row would
    *> otherwise produce a whole number of the next int value
*>     compute c_y = ((neighbor_ptr - 1) / bl_c_cnt(1)) + 1
*> *>   end-if

*>   compute c_x = function mod(neighbor_ptr, bl_c_cnt(1))
*>   if c_x = 0 and neighbor_ptr <> 0
*>     move bl_c_cnt(1) to c_x
*>   end-if
  *> compute n_t = current_time + 1

*>   compute p_t = function mod(n_t, f_lcm)
*>   if p_t = 0
*>     move f_lcm to p_t
*>   end-if
  *> display "CHECKING: Y: " c_y " X: " c_x " T: " n_t " p_t " p_t " f_col: " f_col(p_t, c_y, c_x) " NEIGH_PTR: " neighbor_ptr
  *>   *> display "CHECKING: Y: " c_y " X: " c_x " T: " n_t " GFS_W: " gfs_w " NEIGH_PTR: " neighbor_ptr
    *> display "CHECKING: Y: " n_y " X: " n_x " T: " n_t " GFS_W: " gfs_w
  *>   if f_col(p_t, c_y, c_x) = '.' or neighbor_ptr = 0
      *> display "CHECKING: Y: " c_y " X: " c_x " T: " n_t " GFS_W: " gfs_w " NEIGH_PTR: " neighbor_ptr
      *> display "CHECKING: Y: " c_y " X: " c_x " T: " n_t " p_t " p_t " f_col: " f_col(p_t, c_y, c_x) " NEIGH_PTR: " neighbor_ptr
      *> add 1 to curr_neighbors_num
      *> move neighbor_ptr to curr_neighbor_ptr(curr_neighbors_num)
      *> *> compute neighbor_time = current_time + 1
      *> add 1 to current_time giving curr_neighbor_time(curr_neighbors_num)
      *> if current_ptr <> neighbor_ptr
      *>   move 1 to curr_neighbor_cost(curr_neighbors_num)
      *> else
      *>   move 0 to curr_neighbor_cost(curr_neighbors_num)
      *> end-if

      *> Skip if this has already been visited.
      *> move 'search' to oper
      *> move -1 to h_new_key
      *> move neighbor_ptr to h_new_ptr
      *> move n_t to h_new_time
      *> call 'lib-heap-time' using heap_stuff

      *> display "TEST NEIGHBOR: " neighbor " " h_new_key
      *> If not already in the priority queue, add it.
      *> if h_new_key = 0
      *>   move 0 to h_ret
      *>   if neighbor_ptr > 0
      *>     perform get_heuristic
      *>   end-if
      *>   *> display "NEW STATE INSERTING: h_ret: " h_ret " PTR: " neighbor_ptr " TIME: " n_t
      *>   move 'insert' to oper
      *>   *> move fScore_val(neighbor_ptr) to h_new_key
      *>   move h_ret to h_new_key
      *>   *> move neighbor to h_new_val
      *>   move neighbor_ptr to h_new_ptr
      *>   move n_t to h_new_time
      *>   call 'lib-heap-time' using heap_stuff
      *> end-if
*>    end-if
*>   end-if
*>     *> display "ADD_NEIGHBOR_END"
*>   .


    *> 01 heuristics_stuff.
    *>   02 tmp_x usage is index.
    *>   02 tmp_y usage is index.
    *>   02 tmp_t pic 9(8) comp.
    *>   02 h_ret pic s9(8) comp.

*> get_heuristic.
*>   compute tmp_y = ((neighbor_ptr - 1) / bl_c_cnt(1)) + 1
*>   compute tmp_x = function mod(neighbor_ptr bl_c_cnt(1))
*>   if tmp_x = 0 and neighbor_ptr <> 0
*>     move bl_c_cnt(1) to tmp_x
*>   end-if

*>   compute h_ret = (bl_c_cnt(1) - tmp_x) + (bl_r_cnt - tmp_y)
*>   *> display "HEURISTIC!" h_ret " PTR: " neighbor_ptr
*>   .
