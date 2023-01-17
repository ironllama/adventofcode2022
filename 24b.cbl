           >>source format free
identification division.
program-id. 24b.

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

    01 heap_stuff.
      02 minmax pic x(3).
      02 oper pic x(6).
      02 h_new_item.
        03 h_new_key pic s9(8) comp.
        03 h_new_val.
          04 h_new_y pic s9(3) comp.
          04 h_new_x pic s9(3) comp.
          04 h_new_t pic s9(3) comp.
      02 h_val.
        03 h_cnt pic s9(8) comp value 0.
        03 h_item occurs 999999 indexed by h_idx.
          04 h_item_key pic s9(8) comp.
          04 h_item_val.
            05 h_item_y pic s9(3) comp.
            05 h_item_x pic s9(3) comp.
            05 h_item_t pic s9(3) comp.

    77 s_x pic 9(3) comp.
    77 s_y pic 9(3) comp.
    77 s_t pic 9(3) comp.
    77 g_x pic 9(3) comp.
    77 g_y pic 9(3) comp.


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

  *> Add the starting node -- Min-Heap
  move 0 to s_y
  move 0 to s_x
  move 0 to s_t
  move bl_r_cnt to g_y
  move bl_c_cnt(1) to g_x
  perform trek_through
  display "ARRIVED: " g_y " " g_x " at " n_t

  move bl_r_cnt to s_y
  move bl_c_cnt(1) to s_x
  move n_t to s_t
  move 1 to g_y
  move 1 to g_x
  perform trek_through
  display "ARRIVED: " g_y " " g_x " at " n_t

  move 0 to s_y
  move 0 to s_x
  move n_t to s_t
  move bl_r_cnt to g_y
  move bl_c_cnt(1) to g_x
  perform trek_through
  display "ARRIVED: " g_y " " g_x " at " n_t

  move n_t to total_found
  display "FINAL: " total_found
  goback.

trek_through.
  *> Add the starting node -- Min-Heap
  move 'min' to minmax
  move 'insert' to oper
  move 1 to h_new_key
  move s_y to h_new_y
  move s_x to h_new_x
  move s_t to h_new_t
  call 'lib-heap-time' using heap_stuff

  perform until h_cnt < 1
    move 'next' to oper
    call 'lib-heap-time' using heap_stuff

    *> display "HEAP: [" h_new_key "]: " h_new_y " " h_new_x " " h_new_t " REM: " h_cnt
    move h_new_y to c_y
    move h_new_x to c_x
    compute n_t = h_new_t + 1

    if c_y = g_y and c_x = g_x
      *> display "END: " n_t " STACK: " t_num
      *> display "END: " n_t " HEAP: " h_cnt
      move 0 to h_cnt  *> Stop loop.
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
      if c_y <> s_y and c_x <> s_x
        move c_y to n_y
        move c_x to n_x
        perform add_neighbor
      end-if
    end-if
  end-perform
  .

add_neighbor.
  if n_y >= 0 and n_y <= bl_r_cnt and n_x >= 0 and n_x <= bl_c_cnt(1)
    perform get_future_state_of_spot
    *> display "CHECKING: Y: " n_y " X: " n_x " T: " n_t " GFS_W: " gfs_w
    if gfs_w = 0
      *> Skip if this has already been visited.
      move 'search' to oper
      move -1 to h_new_key
      move n_val to h_new_val
      call 'lib-heap-time' using heap_stuff

      if h_new_key = 0
        move 'insert' to oper
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
