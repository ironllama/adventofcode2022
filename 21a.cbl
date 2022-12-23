           >>source format free
identification division.
program-id. 21a.

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

    01 stack_stuff.
      02 stack_target pic x(4).
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
  display "FINAL: " total_found

  goback.


get_value.
  add 1 to stack_cnt on size error display ">>>>> STACK OVERFLOW! <<<<<" end-add
  *> display "get_value: " stack_target

  move 0 to inst_found
  perform varying inst_idx from 1 by 1 until inst_idx > inst_cnt or inst_found = 1
    if inst_name(inst_idx) = stack_target
      move inst_idx to stack_inst_idx(stack_cnt)
      move 1 to inst_found
    end-if
  end-perform
  *> display "FOUND: " inst_found " " stack_inst_idx(stack_cnt) " " inst_name(stack_inst_idx(stack_cnt)) " " inst_left(stack_inst_idx(stack_cnt)) " " inst_num(stack_inst_idx(stack_cnt))

  if inst_num(stack_inst_idx(stack_cnt)) = 0
    move inst_left(stack_inst_idx(stack_cnt)) to stack_target
    perform get_value
    move stack_value to inst_left_num(stack_inst_idx(stack_cnt))

    move inst_right(stack_inst_idx(stack_cnt)) to stack_target
    perform get_value
    move stack_value to inst_right_num(stack_inst_idx(stack_cnt))

    evaluate inst_oper(stack_inst_idx(stack_cnt))
      when "+" compute inst_num(stack_inst_idx(stack_cnt)) = inst_left_num(stack_inst_idx(stack_cnt)) + inst_right_num(stack_inst_idx(stack_cnt)) on size error display ">>>>> COMPUTE + OVERFLOW!!! <<<<<" end-compute
      when "-" compute inst_num(stack_inst_idx(stack_cnt)) = inst_left_num(stack_inst_idx(stack_cnt)) - inst_right_num(stack_inst_idx(stack_cnt)) on size error display ">>>>> COMPUTE - OVERFLOW!!! <<<<<" end-compute
      when "*" compute inst_num(stack_inst_idx(stack_cnt)) = inst_left_num(stack_inst_idx(stack_cnt)) * inst_right_num(stack_inst_idx(stack_cnt)) on size error display ">>>>> COMPUTE * OVERFLOW!!! <<<<<" end-compute
      when "/" compute inst_num(stack_inst_idx(stack_cnt)) = inst_left_num(stack_inst_idx(stack_cnt)) / inst_right_num(stack_inst_idx(stack_cnt)) on size error display ">>>>> COMPUTE / OVERFLOW!!! <<<<<" end-compute
    end-evaluate
  end-if

  move inst_num(stack_inst_idx(stack_cnt)) to stack_value
  subtract 1 from stack_cnt
  .
