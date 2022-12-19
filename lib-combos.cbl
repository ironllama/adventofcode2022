           >>source format free
identification division.
program-id. combos.

data division.
  working-storage section.

  linkage section.
    01 input_tbl_stuff.
      02 input_tbl_len pic s9(8).
      02 input_tbl pic s9(8) occurs 0 to unbounded
          depending on input_tbl_len indexed by input_tbl_idx.
    
    01 output_tbl_stuff.
      02 output_tbl_len pic s9(8).
      02 output_tbl occurs 0 to unbounded
          depending on output_tbl_len indexed by output_tbl_idx.
        03 output_combo_len pic s9(8).
        *> 03 output_combo pic s9(8) occurs 0 to unbounded
        *>    depending on output_combo_len indexed by output_combo_idx.
        03 output_combo pic s9(8) occurs 99 times.

procedure division.
  display function factorial(4) 

  goback.

*> combine.
  *> if idx < input_tbl_len
    *> add 1 to idx
    *> perform combine

    *> add 1 to output_tbl(idx)
    *> move input_tbl(idx) to output_tbl
    *> perform combine
  *> end-if
  *> .
end program combos.
