           >> source format free
identification division.
program-id. lib-heapsort is initial.

data division.
  local-storage section.
    77 boundary usage is index.
    77 child usage is index.
    77 parent usage is index.
    77 sibling usage is index.

    77 swap-a usage is index.
    77 swap-b usage is index.
    01 swap-temp pic x(999).

    01 array_end pic s9(4) comp.
    01 halt-sw pic x(3).

    01 left_num pic 9(4) comp.
    01 right_num pic 9(4) comp.

  linkage section.
    01 ln-in_array.
      02 ln-array_cnt pic s9(4) comp.
      02 ln-array_item pic x(999) occurs 0 to unbounded
          depending on ln-array_cnt indexed by ln-array_idx.
    01 ln-compare.
      02 ln-compare_func procedure-pointer.
      02 ln-compare_one pic x(999).
      02 ln-compare_two pic x(999).
      02 ln-compare_res pic 9 value 0.

procedure division using ln-in_array ln-compare.
*> procedure division using ln-in_array.

    *> display "min_heapify [ " ln-inStartPos "][" with no advancing
    *> perform varying idx from 1 by 1 until idx > ln-array_cnt
    *>   display ln-array_item(idx) " " with no advancing
    *> end-perform
    *> display "]"

  *> Just bring the largest element to the top. ARRAY NOT SORTED.
  perform varying ln-array_idx from 2 by 1 until ln-array_idx > ln-array_cnt
    perform percolate-up
  end-perform

  *> Now we sort the rest.
  perform varying ln-array_idx from ln-array_cnt by -1 until ln-array_idx < 2
    set swap-a to ln-array_idx
    set swap-b to 1
    perform swap-two-bars

    compute boundary = ln-array_idx - 1
    perform percolate-down
  end-perform

  goback.


percolate-up.
  set child to ln-array_idx
  move "OFF" to halt-sw
  perform until ((child = 1) or (halt-sw = "ON"))
    compute parent = child / 2

    *> if ln-array_item(child) > ln-array_item(parent)
    *> move function trim(ln-array_item(parent)) to left_num
    *> move function trim(ln-array_item(child)) to right_num
    *> if left_num < right_num
    move spaces to ln-compare_one
    move spaces to ln-compare_two
    move ln-array_item(parent) to ln-compare_one
    move ln-array_item(child) to ln-compare_two
    move 0 to ln-compare_res
    call ln-compare_func
    if ln-compare_res = 2
      set swap-a to parent
      set swap-b to child
      perform swap-two-bars

      set child to parent
    else
      move "ON" to halt-sw
    end-if
  end-perform
  .


percolate-down.
  set parent to 1
  move "OFF" to halt-sw
  perform until halt-sw = "ON"
    compute child = 2 * parent
    if child > boundary
      move "ON" to halt-sw
    else
      compute sibling = child + 1

      *> Find the child with the higher value.
      if sibling not > boundary
        *> if ln-array_item(sibling) > ln-array_item(child)
        *> move function trim(ln-array_item(child)) to left_num
        *> move function trim(ln-array_item(sibling)) to right_num
        *> if right_num > left_num
        move spaces to ln-compare_one
        move spaces to ln-compare_two
        move ln-array_item(child) to ln-compare_one
        move ln-array_item(sibling) to ln-compare_two
        move 0 to ln-compare_res
        call ln-compare_func
        if ln-compare_res = 2
          *> compute child = child + 1
          set child to sibling
        end-if
      end-if

    *>   if ln-array_item(parent) < ln-array_item(child)
    *>   move function trim(ln-array_item(parent)) to left_num
    *>   move function trim(ln-array_item(child)) to right_num
    *>   if left_num < right_num
      move spaces to ln-compare_one
      move spaces to ln-compare_two
      move ln-array_item(parent) to ln-compare_one
      move ln-array_item(child) to ln-compare_two
      move 0 to ln-compare_res
      call ln-compare_func
      if ln-compare_res = 2
        set swap-a to parent
        set swap-b to child
        perform swap-two-bars

        set parent to child
      else
        move "ON" to halt-sw
      end-if
    end-if
  end-perform
  .

swap-two-bars.
  move spaces to swap-temp
  move ln-array_item(swap-a) to swap-temp
  move ln-array_item(swap-b) to ln-array_item(swap-a)
  move swap-temp to ln-array_item(swap-b)
  .
