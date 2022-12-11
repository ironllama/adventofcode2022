           >>source format free
identification division.
program-id. 11a.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_line_cnt pic s9(8) comp value 0.
      02 rf_line_row pic x(9999) occurs 0 to 9999 times
          depending on rf_line_cnt indexed by rf_line_idx.

    77 read_filler1 pic x(99).
    77 read_filler2 pic x(99).
    77 read_monkey_num pic 9.
    77 read_items_done pic 9.
    77 read_items_ptr pic s9(4) comp.
    77 read_item_num pic s9(2) comp.

    77 num_monkeys pic 9.
    01 all_monkeys.
      02 monkey occurs 8 times indexed by monkey_idx.
        03 monkey_items_head pic s9(18) comp value 1.
        03 monkey_items_num pic s9(18) comp value 0.
        03 monkey_items occurs 999999 times
      *>    depending on monkey_items_num
          indexed by monkey_items_idx.
          04 monkey_items_val pic s9(18) comp.
          04 monkey_items_id pic s9(2) comp.
        *> 03 starting_monkey_items_num pic s9(18) comp value 0.
        *> 03 starting_monkey_items occurs 999999 times
      *>    indexed by starting_monkey_items_idx.
      *>    04 starting_monkey_items_val pic s9(18) comp.
      *>    04 starting_monkey_items_id pic s9(2) comp.
        03 monkey_operator pic x.
        03 monkey_operand pic x(18).
        03 monkey_test pic s9(2) comp.
        03 monkey_test_t pic 9.
        03 monkey_test_f pic 9.
        03 monkey_num_inspects pic s9(8) comp.

    77 starting_id_num pic s9(2) comp.
    *> 77 starting_found pic 9.

    77 all_tests pic s9(8) comp value 1.

    77 curr_operand pic s9(18) comp.
    01 curr_worry.
      02 curr_worry_val pic s9(18) comp.
      02 curr_worry_id pic s9(2) comp. 

    77 round_idx pic s9(5) comp.
    77 highest_1 pic s9(18) comp.
    77 highest_2 pic s9(18) comp.

    77 total_found pic s9(18) comp.

procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
  move 8 to num_monkeys
  *> call 'lib-readdata' using function module-id ".da1" rf_all_lines
  *> move 4 to num_monkeys

  move 0 to total_found
  move 0 to starting_id_num 
  perform varying rf_line_idx from 1 by 1 until rf_line_idx > rf_line_cnt
    *> display "LINE: " function trim(rf_line_row(rf_line_idx))
    if length of function trim(rf_line_row(rf_line_idx)) > 1
      if rf_line_row(rf_line_idx)(1:1) = "M"
        initialize read_filler1
        unstring rf_line_row(rf_line_idx) delimited by space
          into read_filler1 read_monkey_num
        end-unstring
        add 1 to read_monkey_num
        *> display "MONKEY " read_monkey_num
      else
        if rf_line_row(rf_line_idx)(1:1) = "S"
          initialize read_filler1
          initialize read_filler2
          unstring rf_line_row(rf_line_idx) delimited by ": "
            into read_filler1 read_filler2
          end-unstring

          move 0 to read_items_done
          move 1 to read_items_ptr
          perform until read_items_done = 1
            unstring read_filler2 delimited by ", "
              into read_item_num
              with pointer read_items_ptr
              on overflow
                if read_items_ptr > length of function trim(read_filler2)
                  move 1 to read_items_done
                end-if
                add 1 to monkey_items_num(read_monkey_num)
                add 1 to starting_id_num
                move starting_id_num to monkey_items_id(read_monkey_num monkey_items_num(read_monkey_num))
                move read_item_num to monkey_items_val(read_monkey_num monkey_items_num(read_monkey_num))

      *>          add 1 to starting_monkey_items_num(read_monkey_num)
      *>          move starting_id_num to starting_monkey_items_id(read_monkey_num monkey_items_num(read_monkey_num))
      *>          move read_item_num to starting_monkey_items_val(read_monkey_num monkey_items_num(read_monkey_num))
            end-unstring
            *> display "ADDED ITEM: " read_item_num
          end-perform
        else
          if rf_line_row(rf_line_idx)(1:1) = "O"
            initialize read_filler1
            initialize read_filler2
            unstring rf_line_row(rf_line_idx) delimited by "= old "
              into read_filler1 read_filler2
            end-unstring

            unstring function trim(read_filler2) delimited by all spaces
              into monkey_operator(read_monkey_num) monkey_operand(read_monkey_num)
            end-unstring
            *> display "OPERATOR: " monkey_operator(read_monkey_num) " " monkey_operand(read_monkey_num)
          else
            if rf_line_row(rf_line_idx)(1:1) = "T"
              initialize read_filler1
              unstring rf_line_row(rf_line_idx) delimited by "by "
                into read_filler1 monkey_test(read_monkey_num)
              end-unstring

              compute all_tests = all_tests * monkey_test(read_monkey_num)

            else
              if rf_line_row(rf_line_idx)(4:1) = "t"
                unstring rf_line_row(rf_line_idx) delimited by "key "
                  into read_filler1 monkey_test_t(read_monkey_num)
                end-unstring
                add 1 to monkey_test_t(read_monkey_num)
              else
                if rf_line_row(rf_line_idx)(4:1) = "f"
                  unstring rf_line_row(rf_line_idx) delimited by "key "
                    into read_filler1 monkey_test_f(read_monkey_num)
                  end-unstring
                  add 1 to monkey_test_f(read_monkey_num)
                end-if
              end-if
            end-if
          end-if
        end-if
      end-if
    end-if
  end-perform

  perform varying monkey_idx from 1 by 1 until monkey_idx > num_monkeys
    display "MONKEY " monkey_idx ": oper: " monkey_operator(monkey_idx) " " monkey_operand(monkey_idx) " div by: " monkey_test(monkey_idx) " t: " monkey_test_t(monkey_idx) " f: " monkey_test_f(monkey_idx) " items: " no advancing
     set monkey_items_idx to 1
     perform varying monkey_items_idx from 1 by 1 until monkey_items_idx > monkey_items_num(monkey_idx)
      *> display monkey_items_val(monkey_idx monkey_items_idx) ", " no advancing
       display "[" monkey_items_id(monkey_idx monkey_items_idx) "]" monkey_items_val(monkey_idx monkey_items_idx) ", " no advancing
     end-perform
     display space
  end-perform
  *> perform varying monkey_idx from 1 by 1 until monkey_idx > num_monkeys
    *> display "STARTING " monkey_idx ": " no advancing
     *> set starting_monkey_items_idx to 1
     *> perform varying starting_monkey_items_idx from 1 by 1 until starting_monkey_items_idx > starting_monkey_items_num(monkey_idx)
      *> display "[" starting_monkey_items_id(monkey_idx starting_monkey_items_idx) "]" monkey_items_val(monkey_idx starting_monkey_items_idx) ", " no advancing
     *> end-perform
     *> display space
  *> end-perform

  display "ALL TESTS: " all_tests

  perform varying round_idx from 1 by 1 until round_idx > 10000 
  *> perform varying round_idx from 1 by 1 until round_idx > 20 
    perform varying monkey_idx from 1 by 1 until monkey_idx > num_monkeys
       initialize curr_worry
       set monkey_items_idx to 1
       perform varying monkey_items_idx from monkey_items_head(monkey_idx) by 1 until monkey_items_idx > monkey_items_num(monkey_idx)
         add 1 to monkey_num_inspects(monkey_idx)

         move monkey_items(monkey_idx monkey_items_idx) to curr_worry
      *>   display "CURR_WORRY: MONKEY " monkey_idx ": [" curr_worry_id "]: " curr_worry_val

      *>   move 0 to starting_found
      *>   perform varying starting_monkey_items_idx from 1 by 1 until starting_monkey_items_idx > starting_monkey_items_num(monkey_idx) or starting_found = 1
      *>     if starting_monkey_items_id(monkey_idx starting_monkey_items_idx) = curr_worry_id
             *> *> move monkey_items_val(monkey_idx monkey_items_idx) to curr_worry_val
      *>       move starting_monkey_items_val(monkey_idx starting_monkey_items_idx) to curr_worry_val
             *> *> move starting_monkey_items_val(monkey_idx starting_monkey_items_idx) to monkey_items_val(monkey_idx monkey_items_idx)
      *>       move 1 to starting_found
      *>     end-if
      *>   end-perform
      *>   if starting_found = 0
      *>     add 1 to starting_monkey_items_num(monkey_idx)
      *>     move curr_worry to starting_monkey_items(monkey_idx starting_monkey_items_num(monkey_idx))
      *>     display "ADDED: MONKEY: " monkey_idx " id: " curr_worry_id " val: " curr_worry_val
      *>   end-if

         if monkey_operand(monkey_idx) = "old"
      *>     move monkey_items_val(monkey_idx monkey_items_idx) to curr_operand
           move curr_worry_val to curr_operand
      *>     move 1 to curr_operand
         else
           move monkey_operand(monkey_idx) to curr_operand
         end-if
        *> display "CURR_OPERAND: " curr_operand

         if monkey_operator(monkey_idx) = "+"
      *>     compute curr_worry_val = monkey_items_val(monkey_idx monkey_items_idx) + curr_operand
           compute curr_worry_val = curr_worry_val + curr_operand
         end-if
         if monkey_operator(monkey_idx) = "*"
           compute curr_worry_val = curr_worry_val * curr_operand
         end-if
       
      *>   compute curr_worry_val = curr_worry_val / 3
         compute curr_worry_val = function mod (curr_worry_val all_tests)
        *> display "CURR_WORRY: MONKEY " monkey_idx ": [" curr_worry_id "]: " monkey_items_val(monkey_idx monkey_items_idx) " " curr_operand " = " curr_worry_val

         if function mod(curr_worry_val monkey_test(monkey_idx)) = 0
           add 1 to monkey_items_num(monkey_test_t(monkey_idx))
        *>   move monkey_items(monkey_idx monkey_items_idx) to monkey_items(monkey_test_t(monkey_idx) monkey_items_num(monkey_test_t(monkey_idx)))
      *>     move curr_worry to monkey_items_val(monkey_test_t(monkey_idx) monkey_items_num(monkey_test_t(monkey_idx)))
      *>     move monkey_test(monkey_idx) to curr_worry_val
           move curr_worry to monkey_items(monkey_test_t(monkey_idx) monkey_items_num(monkey_test_t(monkey_idx)))
      *>     move monkey_test(monkey_idx) to monkey_items(monkey_test_t(monkey_idx) monkey_items_num(monkey_test_t(monkey_idx)))
      *>     display "PRIME!"
         else
           add 1 to monkey_items_num(monkey_test_f(monkey_idx))
        *>   move monkey_items(monkey_idx monkey_items_idx) to monkey_items(monkey_test_f(monkey_idx) monkey_items_num(monkey_test_f(monkey_idx)))
      *>     move curr_worry to monkey_items_val(monkey_test_f(monkey_idx) monkey_items_num(monkey_test_f(monkey_idx)))
           move curr_worry to monkey_items(monkey_test_f(monkey_idx) monkey_items_num(monkey_test_f(monkey_idx)))
      *>     move monkey_test(monkey_idx) to monkey_items(monkey_test_f(monkey_idx) monkey_items_num(monkey_test_f(monkey_idx)))
         end-if
         add 1 to monkey_items_head(monkey_idx)
       end-perform
       *> display space
    end-perform

    *> display "ROUND: " round_idx
    *> perform varying monkey_idx from 1 by 1 until monkey_idx > num_monkeys
      *> display "MONKEY " monkey_idx ": " no advancing
      *> set monkey_items_idx to 1
      *> perform varying monkey_items_idx from monkey_items_head(monkey_idx) by 1 until monkey_items_idx > monkey_items_num(monkey_idx)
        *> display "[" monkey_items_id(monkey_idx monkey_items_idx) "]" monkey_items_val(monkey_idx monkey_items_idx) ", " no advancing
      *> end-perform
      *> display space
    *> end-perform
  end-perform

  display space
  perform varying monkey_idx from 1 by 1 until monkey_idx > num_monkeys
    display "MONKEY " monkey_idx ": " monkey_num_inspects(monkey_idx)
    if monkey_num_inspects(monkey_idx) > highest_1
      move highest_1 to highest_2
      move monkey_num_inspects(monkey_idx) to highest_1
    else
      if monkey_num_inspects(monkey_idx) > highest_2
        move monkey_num_inspects(monkey_idx) to highest_2
      end-if
    end-if
  end-perform

  compute total_found = highest_1 * highest_2

  display "FINAL: " total_found

  goback.