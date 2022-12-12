           >>source format free
identification division.
program-id. lib-heap is initial.
*> Adapted from https://www.digitalocean.com/community/tutorials/min-heap-binary-tree

data division.
  local-storage section.
    77 parent usage is index.
    77 child usage is index.
    77 sibling usage is index.

    01 swap-temp.
      02 swap_temp_key pic s9(8) comp.
      02 swap_temp_val pic s9(8) comp.

    01 halt_sw pic x(3).

  linkage section.
    01 ln-type pic x(3).
    01 ln-oper pic x(6).
    01 ln-in_heap.
      02 ln-heap_cnt pic s9(8) comp value 0.
      02 ln-heap_item occurs 1 to unbounded
          depending on ln-heap_cnt indexed by ln-heap_idx.
        03 ln-heap_item_key pic s9(8) comp.
        03 ln-heap_item_val pic s9(8) comp.
    01 ln-heap_new_item.
      02 ln-heap_new_key pic s9(8) comp.
      02 ln-heap_new_val pic s9(8) comp.

procedure division using ln-type ln-oper ln-in_heap ln-heap_new_item.
  initialize swap-temp
  move 0 to halt_sw

  if ln-oper = "insert"
    perform insert_heap
  end-if
  if ln-oper = "next"
    perform delete_heap
  end-if
  if ln-oper = "search"
    perform search_heap
  end-if

  goback.


insert_heap.
  compute ln-heap_cnt = ln-heap_cnt + 1
  move ln-heap_new_item to ln-heap_item(ln-heap_cnt)

  set child to ln-heap_cnt
  compute parent = child / 2
  perform until child < 2
      or (ln-type = "max" and ln-heap_item_key(parent) >= ln-heap_item_key(child))
      or (ln-type = "min" and ln-heap_item_key(parent) <= ln-heap_item_key(child))
    perform swap_nodes

    set child to parent
    compute parent = child / 2
  end-perform
  .


delete_heap.
  if ln-heap_cnt > 0
    move ln-heap_item(1) to ln-heap_new_item

    set parent to 1
    set child to ln-heap_cnt
    perform swap_nodes

    compute ln-heap_cnt = ln-heap_cnt - 1

    perform heapify
  end-if
  .


heapify.
  compute child = 2 * parent
  move "OFF" to halt_sw
  perform until halt_sw = "ON"
    if child > ln-heap_cnt
      move "ON" to halt_sw
    else
      compute sibling = child + 1

    *> Find the child with the lower value.
    if sibling <= ln-heap_cnt
      if (ln-type = "max" and ln-heap_item_key(sibling) > ln-heap_item_key(child))
          or (ln-type = "min" and ln-heap_item_key(sibling) < ln-heap_item_key(child))
        set child to sibling
      end-if
    end-if

    if (ln-type = "max" and ln-heap_item_key(child) > ln-heap_item_key(parent))
        or (ln-type = "min" and ln-heap_item_key(child) < ln-heap_item_key(parent))
      perform swap_nodes
      set parent to child
    else
      move "ON" to halt_sw
    end-if

    compute child = 2 * parent
  end-perform
  .


swap_nodes.
  move ln-heap_item(parent) to swap-temp
  move ln-heap_item(child) to ln-heap_item(parent)
  move swap-temp to ln-heap_item(child)
  .


search_heap.
  set ln-heap_idx to 1
  search ln-heap_item
    at end move 0 to ln-heap_new_key
    when ln-heap_item_val(ln-heap_idx) = ln-heap_new_val
      move ln-heap_item_key(ln-heap_idx) to ln-heap_new_key
  end-search
  .
