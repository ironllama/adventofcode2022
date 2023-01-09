           >>source format free
identification division.
program-id. lib-permutations is recursive.
author. alexoh@wcoding.

data division.
  local-storage section.
    77 curr_loop_idx usage is index.
    77 curr_input_head usage is index.

    01 local_permutations.
      02 local_perm_len pic s9(8) comp.
      02 local_perm_list_cnt pic s9(8) comp.
      02 local_perm_list occurs 999 times
          indexed by local_perm_list_idx.
        03 local_perm_cnt pic s9(8) comp.
        03 local_perm usage is index occurs 999 times
          indexed by local_perm_idx.

  linkage section.
    01 inputs.
      02 input_cnt pic s9(8) comp.
      02 input_head usage is index value 1. *> Internal use during recursion.
      *> 02 input_item pic x(2) occurs 0 to unbounded times
      *> 02 input_item usage is index occurs 0 to unbounded times
      *>   depending on input_cnt indexed by input_idx.

    01 permutations.
      02 perm_len pic s9(8) comp.  *> Must be provided. Num of items per permutation.
      02 perm_list_cnt pic s9(8) comp.
      02 perm_list occurs 999 times  *> Corruption on copying unboundeds to working/local storage.
          indexed by perm_list_idx.
        03 perm_cnt pic s9(8) comp.
        *> 03 perm pic x(2) occurs 999 times
        03 perm usage is index occurs 999 times
          indexed by perm_idx.


procedure division using inputs permutations.
  *> Keep local copy of the head pointer to mutate, but keep local to this recursion instance..
  move input_head to curr_input_head

  if perm_len > 0
    perform varying curr_loop_idx from curr_input_head by 1 until curr_loop_idx > (input_cnt - perm_len + 1)
      initialize local_permutations

      add 1 to curr_input_head
      set local_perm_len to perm_len
      subtract 1 from local_perm_len

      *> display "LOCAL INPUTS: [ " no advancing
      *> perform varying input_idx from curr_loop_idx by 1 until input_idx > input_cnt
      *>   display input_item(input_idx) no advancing
      *>   if input_idx < input_cnt display ", " no advancing end-if
      *> end-perform
      *> display " ]" perm_cnt(1) local_perm_cnt(1)

      *> Put mutated head pointer into payload.
      move curr_input_head to input_head

      call 'lib-anagrams' using inputs local_permutations

      *> display "LOCAL PERMS:  " no advancing
      *> perform varying local_perm_list_idx from 1 by 1 until local_perm_list_idx > local_perm_list_cnt
      *>   display "[ " no advancing
      *>   perform varying local_perm_idx from 1 by 1 until local_perm_idx > local_perm_cnt(local_perm_list_idx)
      *>     display local_perm(local_perm_list_idx local_perm_idx) no advancing
      *>     if local_perm_idx < local_perm_cnt(local_perm_list_idx) display ", " no advancing end-if
      *>   end-perform
      *>   display " ]" no advancing
      *> end-perform
      *> display space

      perform with test after varying local_perm_list_idx from 1 by 1 until local_perm_list_idx >= local_perm_list_cnt
        add 1 to perm_list_cnt
        *> Manually add the first item from inputs.
        add 1 to perm_cnt(perm_list_cnt)
        move curr_loop_idx to perm(perm_list_cnt perm_cnt(perm_list_cnt))

        *> Add the rest from the recursion.
        perform varying local_perm_idx from 1 by 1 until local_perm_idx > local_perm_cnt(local_perm_list_idx)
          add 1 to perm_cnt(perm_list_cnt)
          move local_perm(local_perm_list_idx local_perm_idx) to perm(perm_list_cnt perm_cnt(perm_list_cnt))
        end-perform
      end-perform
    end-perform

    *> display "PERMS: " no advancing
    *> perform varying perm_list_idx from 1 by 1 until perm_list_idx > perm_list_cnt
    *>   display "[ " no advancing
    *>   perform varying perm_idx from 1 by 1 until perm_idx > perm_cnt(perm_list_idx)
    *>     display perm(perm_list_idx perm_idx) no advancing
    *>     if perm_idx < perm_cnt(perm_list_idx) display ", " no advancing end-if
    *>   end-perform
    *>   display " ]" no advancing
    *> end-perform
    *> display space

  end-if

  goback
  .
