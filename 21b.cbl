           >>source format free
identification division.
program-id. 21b.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_cnt pic s9(8) comp value 0.
      02 rf_row pic x(9999) occurs 0 to 9999 times
          depending on rf_cnt indexed by rf_idx.

    01 instructions.
      02 inst_cnt pic s9(4) comp.
      02 inst_found pic 9.
      02 all_inst occurs 3000 times indexed by inst_idx.
        03 inst_name pic x(4).
        03 inst_left pic x(4).
        03 inst_left_num pic s9(18) comp.
        03 inst_oper pic x.
        03 inst_right pic x(4).
        03 inst_right_num pic s9(18) comp.
        03 inst_num pic s9(18) comp.
        03 inst_human pic x value 'N'.
          88 inst_has_human value 'Y'.
    01 root_idx usage is index.
    01 root_left_val pic s9(18) comp.
    01 root_right_val pic s9(18) comp.

    01 stack_stuff.
      02 stack_target pic x(4).
      02 stack_target_idx usage is index.
      02 stack_left_idx usage is index.
      02 stack_value pic s9(18) comp.
      02 stack_cnt pic s9(4) comp.
      02 stack occurs 9999 times indexed by stack_idx.
        03 stack_inst_idx usage is index.

    77 total_found pic s9(18) comp.

procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
*>   call 'lib-readdata' using function module-id ".da1" rf_all_lines

  move 0 to total_found
  perform varying rf_idx from 1 by 1 until rf_idx > rf_cnt
    *> display "LINE: " function trim(rf_row(rf_idx))
    add 1 to inst_cnt
    move rf_row(rf_idx)(1:4) to inst_name(rf_idx)
    if length of function trim(rf_row(rf_idx)) > 9
      move rf_row(rf_idx)(7:4) to inst_left(rf_idx)
      move rf_row(rf_idx)(12:1) to inst_oper(rf_idx)
      move rf_row(rf_idx)(14:4) to inst_right(rf_idx)
    else
      move rf_row(rf_idx)(7:) to inst_num(rf_idx)
    end-if

    if inst_name(rf_idx) = "root" set root_idx to rf_idx end-if
  end-perform
  *> display "=== INST ==="
  *> perform varying inst_idx from 1 by 1 until inst_idx > inst_cnt
  *>   if inst_oper(inst_idx) <> space
  *>     display inst_name(inst_idx) ": " inst_left(inst_idx) " " inst_oper(inst_idx) " " inst_right(inst_idx)
  *>   else
  *>     display inst_name(inst_idx) ": " inst_num(inst_idx)
  *>   end-if
  *> end-perform
  *> display "============"

  move "root" to stack_target
  perform get_value


  move stack_value to total_found
  *> display "FIRST PART: " total_found

  *> display "GOING IN REVERSE!"

  move inst_left(root_idx) to stack_target
  perform find_inst
  move inst_num(stack_target_idx) to root_left_val
  *> display "LEFT: " inst_left(root_idx) " VAL: " root_left_val " HUM: " inst_human(stack_target_idx)

  move inst_right(root_idx) to stack_target
  perform find_inst
  move inst_num(stack_target_idx) to root_right_val
  *> display "RIGHT: " inst_right(root_idx) " VAL: " root_right_val " HUM: " inst_human(stack_target_idx)

  if inst_has_human(stack_target_idx)
    move inst_right(root_idx) to stack_target
    compute total_found = inst_num(root_idx) - root_right_val
  else
    move inst_left(root_idx) to stack_target
    compute total_found = inst_num(root_idx) - root_left_val
  end-if
  perform get_human

  display "FINISHED: " total_found

  goback.

get_human.
  *> Make sure to set the stack-target before calling get_human!
  add 1 to stack_cnt on size error display ">>>>> STACK OVERFLOW! <<<<<" end-add

  *> Look for instruction given as stack_target in the list.
  perform find_inst
  move stack_target_idx to stack_inst_idx(stack_cnt)
  *> display "FOUND: " inst_found " " stack_inst_idx(stack_cnt) " " inst_name(stack_inst_idx(stack_cnt)) " " inst_left(stack_inst_idx(stack_cnt)) " " inst_num(stack_inst_idx(stack_cnt))

  *> Special end-condition.
*>   if stack_target = "humn"
*>     display ">>>>> HOOOOMAN! <<<<<" total_found
*>   else
  if stack_target <> "humn"
    move inst_left(stack_inst_idx(stack_cnt)) to stack_target
    perform find_inst
    move stack_target_idx to stack_left_idx
    if inst_has_human(stack_left_idx)
      evaluate inst_oper(stack_inst_idx(stack_cnt))
        when "+" compute total_found = total_found - inst_right_num(stack_inst_idx(stack_cnt)) on size error display ">>>>> COMPUTE + OVERFLOW!!! <<<<<" end-compute
        when "-" compute total_found = total_found + inst_right_num(stack_inst_idx(stack_cnt)) on size error display ">>>>> COMPUTE - OVERFLOW!!! <<<<<" end-compute
        when "*" compute total_found = total_found / inst_right_num(stack_inst_idx(stack_cnt)) on size error display ">>>>> COMPUTE * OVERFLOW!!! <<<<<" end-compute
        when "/" compute total_found = total_found * inst_right_num(stack_inst_idx(stack_cnt)) on size error display ">>>>> COMPUTE / OVERFLOW!!! <<<<<" end-compute
      end-evaluate

      *> display "LEFT: [" stack_cnt "]: " inst_name(stack_inst_idx(stack_cnt)) " L: " inst_left(stack_inst_idx(stack_cnt)) " " inst_oper(stack_inst_idx(stack_cnt)) " " inst_right(stack_inst_idx(stack_cnt)) " SO FAR: " total_found
      move inst_left(stack_inst_idx(stack_cnt)) to stack_target
      perform get_human
    else
      *> Safe to assume the right has the human if the left does not?
      *> Having the operand on the left makes for tricky math. Beware!
      evaluate inst_oper(stack_inst_idx(stack_cnt))
        when "+" compute total_found = total_found - inst_left_num(stack_inst_idx(stack_cnt)) on size error display ">>>>> COMPUTE + OVERFLOW!!! <<<<<" end-compute
        *> when "-" compute total_found = total_found + inst_left_num(stack_inst_idx(stack_cnt)) on size error display ">>>>> COMPUTE - OVERFLOW!!! <<<<<" end-compute
        when "-" compute total_found = inst_left_num(stack_inst_idx(stack_cnt)) - total_found on size error display ">>>>> COMPUTE - OVERFLOW!!! <<<<<" end-compute
        when "*" compute total_found = total_found / inst_left_num(stack_inst_idx(stack_cnt)) on size error display ">>>>> COMPUTE * OVERFLOW!!! <<<<<" end-compute
        *> when "/" compute total_found = total_found * inst_left_num(stack_inst_idx(stack_cnt)) on size error display ">>>>> COMPUTE / OVERFLOW!!! <<<<<" end-compute
        when "/" compute total_found = inst_left_num(stack_inst_idx(stack_cnt)) / total_found on size error display ">>>>> COMPUTE / OVERFLOW!!! <<<<<" end-compute
      end-evaluate

      *> display "RIGHT: [" stack_cnt "]: " inst_name(stack_inst_idx(stack_cnt)) " L: " inst_left(stack_inst_idx(stack_cnt)) " " inst_oper(stack_inst_idx(stack_cnt)) " " inst_right(stack_inst_idx(stack_cnt)) " SO FAR: " total_found
      move inst_right(stack_inst_idx(stack_cnt)) to stack_target
      perform get_human
    end-if
  end-if

  subtract 1 from stack_cnt
  .

get_value.
  *> Make sure to set the stack-target before calling get_value!

  add 1 to stack_cnt on size error display ">>>>> STACK OVERFLOW! <<<<<" end-add

  *> Look for instruction given as stack_target in the list.
  perform find_inst
  move stack_target_idx to stack_inst_idx(stack_cnt)
  *> display "FOUND: " inst_found " " stack_inst_idx(stack_cnt) " " inst_name(stack_inst_idx(stack_cnt)) " " inst_left(stack_inst_idx(stack_cnt)) " " inst_num(stack_inst_idx(stack_cnt))

  *> Special end-condition.
  if stack_target = "humn"
    set inst_has_human(stack_inst_idx(stack_cnt)) to true
    *> display ">>>>> HOOOOMAN! <<<<<" stack_value
  end-if

  *> If no number value has been yet set, it is not a number but an operation to do.
  if inst_num(stack_inst_idx(stack_cnt)) = 0
    move inst_left(stack_inst_idx(stack_cnt)) to stack_target
    perform get_value
    move stack_value to inst_left_num(stack_inst_idx(stack_cnt))
    if stack_cnt > 0 and inst_has_human(stack_inst_idx(stack_cnt + 1))
      *> display "CHILD WITH HOOMAN! (LEFT) "
      move inst_human(stack_inst_idx(stack_cnt + 1)) to inst_human(stack_inst_idx(stack_cnt))
    end-if

    move inst_right(stack_inst_idx(stack_cnt)) to stack_target
    perform get_value
    move stack_value to inst_right_num(stack_inst_idx(stack_cnt))
    if stack_cnt > 0 and inst_has_human(stack_inst_idx(stack_cnt + 1))
      *> display "CHILD WITH HOOMAN! (RIGHT)"
      move inst_human(stack_inst_idx(stack_cnt + 1)) to inst_human(stack_inst_idx(stack_cnt))
    end-if

    evaluate inst_oper(stack_inst_idx(stack_cnt))
      when "+" compute inst_num(stack_inst_idx(stack_cnt)) = inst_left_num(stack_inst_idx(stack_cnt)) + inst_right_num(stack_inst_idx(stack_cnt)) on size error display ">>>>> COMPUTE + OVERFLOW!!! <<<<<" end-compute
      when "-" compute inst_num(stack_inst_idx(stack_cnt)) = inst_left_num(stack_inst_idx(stack_cnt)) - inst_right_num(stack_inst_idx(stack_cnt)) on size error display ">>>>> COMPUTE - OVERFLOW!!! <<<<<" end-compute
      when "*" compute inst_num(stack_inst_idx(stack_cnt)) = inst_left_num(stack_inst_idx(stack_cnt)) * inst_right_num(stack_inst_idx(stack_cnt)) on size error display ">>>>> COMPUTE * OVERFLOW!!! <<<<<" end-compute
      when "/" compute inst_num(stack_inst_idx(stack_cnt)) = inst_left_num(stack_inst_idx(stack_cnt)) / inst_right_num(stack_inst_idx(stack_cnt)) on size error display ">>>>> COMPUTE / OVERFLOW!!! <<<<<" end-compute
    end-evaluate
  end-if

  *> display "CURR: [" stack_cnt "]: " inst_name(stack_inst_idx(stack_cnt)) " L: " inst_left(stack_inst_idx(stack_cnt)) " " inst_oper(stack_inst_idx(stack_cnt)) " " inst_right(stack_inst_idx(stack_cnt)) " SO FAR: " inst_num(stack_inst_idx(stack_cnt))
  move inst_num(stack_inst_idx(stack_cnt)) to stack_value
  subtract 1 from stack_cnt
  .

find_inst.
  *> Look for instruction given as stack_target in the list.
  move 0 to inst_found
  perform varying inst_idx from 1 by 1 until inst_idx > inst_cnt or inst_found = 1
    if inst_name(inst_idx) = stack_target
      move inst_idx to stack_target_idx
      move 1 to inst_found
    end-if
  end-perform
  .



*> The boneyard of abandoned attempts.

*>    01 root_value pic s9(18) comp.
*>    01 human_idx usage is index.

*>        03 stack_parent pic x(4).
*>        03 stack_human pic x value 'N'.
*>          88 stack_has_human value 'Y'.
*>          88 stack_has_no_human value 'N'.
*>      02 stack_inst_dir pic x value 'F'.
*>        88 go_backwards value 'B'.

*>      *> Special handling of the root, since we branch between the two "equal" values.
*>    if stack_parent(stack_cnt) = "root"
*>      move inst_idx to root_idx
*>      display "ROOT: " inst_found " " stack_inst_idx(stack_cnt) " " inst_name(stack_inst_idx(stack_cnt)) " " inst_left(stack_inst_idx(stack_cnt)) " " inst_num(stack_inst_idx(stack_cnt))
*>    end-if
*>
*>    move inst_left(stack_inst_idx(stack_cnt)) to stack_target
*>    perform get_value
*>    move stack_value to inst_left_num(stack_inst_idx(stack_cnt))
*>    if stack_cnt > 0 move stack_has_human(stack_cnt) to stack_has_human(stack_cnt - 1) end-if
*>    *> If this root operand doesn't involve a human, save its value for the human side.
*>    *> Otherwise, we can ignore this stack_value.
*>    if stack_parent(stack_cnt) = "root" and not stack_has_human
*>      move stack_value to root_value
*>    end-if
*>
*>    move inst_right(stack_inst_idx(stack_cnt)) to stack_target
*>    *> Assuming if the first root operand doesn't involve a human, this one will.
*>    if stack_parent(stack_cnt) = "root" and not stack_has_human
*>      set go_backwards to true
*>      *> move root_value to stack_value  *> Already should be set from first operand.
*>    end-if
*>    perform get_value
*>    if stack_parent(stack_cnt) = "root"
*>      *> If the first operand didn't involve a human, this one did.
*>      if not go_backwards
*>        *> Otherwise, we have to redo the first operand assuming it was human, with the new root_value.
*>        *> move stack_value to root_value  *> More for symmetry. Doesn't really do much in operation.
*>
*>        set go_backwards to true
*>        move inst_left(stack_inst_idx(stack_cnt)) to stack_target
*>        perform get_value
*>        move root_value to inst_right_num(stack_inst_idx(stack_cnt))
*>      end-if
*>    else
*>      move stack_value to inst_right_num(stack_inst_idx(stack_cnt))
*>      if stack_cnt > 0 move stack_has_human(stack_cnt) to stack_has_human(stack_cnt - 1) end-if
*>    end-if
*>
*>    if go_backwards perform process_backwards
*>    else perform process_forwards
*>    end-if
*>
