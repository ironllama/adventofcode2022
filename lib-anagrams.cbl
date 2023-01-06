           >>source format free
identification division.
program-id. lib-anagrams is recursive.

data division.
  local-storage section.
    01 curr_char pic x(2).

    01 local_inputs.
      02 local_input_cnt pic s9(8) comp.
      02 local_input_item pic x(2) occurs 999 times
        indexed by local_input_idx.
    77 local_inputs_start usage is index.

    01 temp_perm_stuff.
      02 temp_perm_cnt pic s9(8) comp.
      02 temp_perm pic x(2) occurs 999 times
        indexed by temp_perm_idx.

    01 local_permutations.
      02 local_perm_len pic s9(8) comp.  *> Must be provided. Num of items per permutation.
      02 local_perm_list_cnt pic s9(8) comp.
      02 local_perm_list occurs 999 times
          indexed by local_perm_list_idx.
        03 local_perm_cnt pic s9(8) comp.
        03 local_perm pic x(2) occurs 999 times
          indexed by local_perm_idx.

  linkage section.
    01 inputs.
      02 input_cnt pic s9(8) comp.
      02 input_item pic x(2) occurs 0 to unbounded times
        depending on input_cnt indexed by input_idx.

    01 permutations.
      02 perm_len pic s9(8) comp.  *> Must be provided. Num of items per permutation.
      02 perm_list_cnt pic s9(8) comp.
      02 perm_list occurs 999 times  *> Corruption on copying unboundeds to working/local storage.
          indexed by perm_list_idx.
        03 perm_cnt pic s9(8) comp.
        03 perm pic x(2) occurs 999 times
          indexed by perm_idx.


procedure division using inputs permutations.
  display "ANAGRAMS: " input_cnt perm_len perm_cnt(1)
  if perm_len > 0
    perform varying input_idx from 1 by 1 until input_idx > (input_cnt - perm_len + 1)
      display "OUTER: " input_idx input_cnt perm_len

      initialize local_inputs
      initialize local_permutations

      *> Copy of inputs for recursion.
      compute local_inputs_start = input_idx + 1
      perform varying local_input_idx from local_inputs_start by 1 until local_input_idx > input_cnt
        add 1 to local_input_cnt
        move input_item(local_input_idx) to local_input_item(local_input_cnt)
      end-perform
      *> Copy of permutations for recursion.
      *> move permutations to local_permutations
      set local_perm_len to perm_len
      subtract 1 from local_perm_len

      display "LOCAL INPUTS: [ " no advancing
      perform varying local_input_idx from 1 by 1 until local_input_idx > local_input_cnt
        display local_input_item(local_input_idx) no advancing
        if local_input_idx < local_input_cnt display ", " no advancing end-if
      end-perform
      display " ]" perm_cnt(1) local_perm_cnt(1)

      display "RECURSING: " local_input_cnt local_perm_len
      call 'lib-anagrams' using local_inputs local_permutations

      display "LOCAL PERMS: [" input_idx "] " no advancing
      perform varying local_perm_list_idx from 1 by 1 until local_perm_list_idx > local_perm_list_cnt
        display "[ " no advancing
        perform varying local_perm_idx from 1 by 1 until local_perm_idx > local_perm_cnt(local_perm_list_idx)
          display local_perm(local_perm_list_idx local_perm_idx) no advancing
          if local_perm_idx < local_perm_cnt(local_perm_list_idx) display ", " no advancing end-if
        end-perform
        display " ]" no advancing
      end-perform
      display space

        perform with test after varying local_perm_list_idx from 1 by 1 until local_perm_list_idx > local_perm_list_cnt
          add 1 to perm_list_cnt
          *> Manually add the first item from inputs.
          add 1 to perm_cnt(perm_list_cnt)
          move input_item(input_idx) to perm(perm_list_cnt perm_cnt(perm_list_cnt))

          *> Add the rest from the recursion.
          perform varying local_perm_idx from 1 by 1 until local_perm_idx > local_perm_cnt(local_perm_list_idx)
            add 1 to perm_cnt(perm_list_cnt)
            move local_perm(local_perm_list_cnt local_perm_list_idx) to perm(perm_list_cnt perm_cnt(perm_list_cnt))
          end-perform
        end-perform
    end-perform
    *> for (let i = 0; i <= arr.length - len; i++) {
    *>     const sub_result = combinations(arr.slice(i + 1), len - 1);
    *>     for (const combination of sub_result) {
    *>         result.push([arr[i], ...combination]);
    *>     }
    *> }
    *> return result;

    display "END: " input_cnt perm_len perm_list_cnt perm_cnt(1)
    display "PERMS: " no advancing
    perform varying perm_list_idx from 1 by 1 until perm_list_idx > perm_list_cnt
      display "[ " no advancing
      perform varying perm_idx from 1 by 1 until perm_idx > perm_cnt(perm_list_idx)
        display perm(perm_list_idx perm_idx) no advancing
        if perm_idx < perm_cnt(perm_list_idx) display ", " no advancing end-if
      end-perform
      display " ]" no advancing
    end-perform
    display space

  end-if

*>     if input_cnt = 1
*>       add 1 to perm_list_cnt
*>       move inputs to perm_list(perm_list_cnt)
*>     else
*>       perform varying input_idx from 1 by 1 until input_idx > input_cnt
*>         move input_item(input_cnt) to local_input_item
*>         compute local_input_cnt = input_cnt - 1
*>         call 'lib-anagrams' using local_input local_perm
*>         subtract 1 from stack_cnt

*>         curr_char = input_item[0];
*>         for (var j = 0; j <= stack[stack_cnt + 1].perm.length; j++) {
*>             // Add char to beginning of list.
*>             if (stack[stack_cnt + 1].perm[j]) {
*>                 temp_perm = [];
*>                 temp_perm.push(curr_char);
*>                 for (var k = 0; k <= stack[stack_cnt + 1].perm[j].length; k++) {
*>                     if (stack[stack_cnt + 1].perm[j][k])
*>                         temp_perm.push(stack[stack_cnt + 1].perm[j][k]);
*>                 }

*>                 perm_list_cnt += 1;
*>                 perm_list(perm_list_cnt) = temp_perm;
*>             }
*>         }

*>         // Add char to the end of the list.
*>         input_item = input_item.slice(1, input_item.length);
*>         input_item[input_item.length] = curr_char;
*>       end-perform
*>   end-if

  goback.
