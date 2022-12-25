           >>source format free
identification division.
program-id. 25a.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_cnt pic s9(8) comp value 0.
      02 rf_row pic x(9999) occurs 0 to 9999 times
          depending on rf_cnt indexed by rf_idx.
    01 rf_char_idx usage is index.
    01 rf_line_len pic s9(2) comp.

    01 pos_place pic 9(18) comp.
    01 line_total pic s9(18) comp.
    77 total_found pic s9(18) comp.

    01 largest_power pic s9(18) comp.
    01 digits_cnt pic 9(2) comp.
    01 digits_str pic x(26).
    01 digits_num pic s9(18) comp.
    01 digits_running pic s9(18) comp.

    01 gen_idx usage is index.
    01 gen_end pic 9(18) comp.
    01 gen_buffer pic x(18).
    01 gen_num pic 9.
    01 gen_carry pic 9.
    01 gen_char pic x.

    77 total_found_snafu pic s9(18) comp.

procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
  *> call 'lib-readdata' using function module-id ".da1" rf_all_lines

  *> Process each line.
  move 0 to total_found
  perform varying rf_idx from 1 by 1 until rf_idx > rf_cnt
    *> display "LINE: " function trim(rf_row(rf_idx))
    move length of function trim(rf_row(rf_idx)) to rf_line_len
    move 0 to line_total
    perform varying rf_char_idx from rf_line_len by -1 until rf_char_idx < 1
      *> 1s place.
      if rf_char_idx = rf_line_len
        move 1 to pos_place
      else
        compute pos_place = 5 ** (rf_line_len - rf_char_idx)
      end-if
      *> display "pos_place: " pos_place

      evaluate rf_row(rf_idx)(rf_char_idx:1)
        when '0'
          compute line_total = line_total + 0
        when '1'
          compute line_total = line_total + pos_place
        when '2'
          compute line_total = line_total + (pos_place * 2)
        when '-'
          compute line_total = line_total + (pos_place * -1)
        when '='
          compute line_total = line_total + (pos_place * -2)
      end-evaluate
    end-perform
    compute total_found = total_found + line_total
    *> display "LINE [" rf_idx "]: " line_total " TOTAL: " total_found
  end-perform
  *> display "TOTAL_FOUND: " total_found

  move total_found to digits_running
  perform base5_convert
  *> display "AFTER BASE5: " digits_str
  perform snafu_convert
  display "AFTER SNAFU: " digits_str

  goback.

base5_convert.
  compute largest_power = function log(digits_running) / function log(5)
  *> display "LARGEST POWER OF " digits_running " is: " largest_power

  move spaces to digits_str
  perform varying gen_idx from largest_power by -1 until gen_idx < 0
    compute digits_num = digits_running / (5 ** gen_idx)
    compute digits_running = digits_running - (digits_num * (5 ** gen_idx))
    add 1 to digits_cnt
    move digits_num to gen_buffer
    move gen_buffer(length of gen_buffer:1) to digits_str(digits_cnt:1)
    *> display "digits_num: " digits_num " digits_str: " digits_str
  end-perform
  .

snafu_convert.
  move length of function trim(digits_str) to gen_end
  perform varying gen_idx from gen_end by -1 until gen_idx < 1
    move digits_str(gen_idx:1) to gen_num
    if gen_carry = 1
      compute gen_num = gen_num + 1
      move 0 to gen_carry
    end-if
    evaluate gen_num
      when 5
        move "0" to digits_str(gen_idx:1)
        move 1 to gen_carry
      when 4
        move "-" to digits_str(gen_idx:1)
        move 1 to gen_carry
      when 3
        move "=" to digits_str(gen_idx:1)
        move 1 to gen_carry
      when other
        move gen_num to digits_str(gen_idx:1)
    end-evaluate
  end-perform
  *> If there is a carry at the highest place, requiring a new digit in front.
  if gen_carry >= 1
    move gen_carry to gen_char
    display "CURR: [" digits_str "] char: [" gen_char "]"
    string gen_char digits_str delimited by size into gen_buffer end-string
    move gen_buffer to digits_str
  end-if
  .



  *> Other garbage from failed or abandoned attempts...


  *> 01 curr_range pic s9(18) comp.

  *> compute largest_power = function log(total_found) / function log(5)
  *> display "LARGEST POWER OF " total_found " is: " largest_power
  *> move spaces to digits_str
  *> perform varying gen_idx from largest_power by -1 until gen_idx < 0
    *> if gen_idx = 0
      *> if total_found < 3 and total_found >= 0
        *> add 1 to digits_cnt
        *> display "LAST DIGIT A: " digits_str
        *> move total_found to gen_buffer
        *> display "LAST DIGIT B: " gen_buffer(18:1)
        *> move gen_buffer(18:1) to digits_str(digits_cnt:1)
        *> display "LAST DIGIT: " total_found " " digits_str
      *> else
        *> display "TOO MUCH OR TOO LITTLE LEFT: " total_found
      *> end-if
    *> else
      *> compute curr_range = (5 ** gen_idx)

      *> if total_found >= 0
        *> if total_found > curr_range * 3
      *>    add 1 to digits_cnt
      *>    move '1' to digits_str(digits_cnt:1)
      *>    compute total_found = total_found - (5 ** (gen_idx + 1))
        *> else
      *>    if total_found > curr_range * 2
      *>      add 1 to digits_cnt
      *>      move '2' to digits_str(digits_cnt:1)
      *>      compute total_found = total_found - (curr_range * 2)
      *>    else
      *>      if total_found > curr_range
      *>        add 1 to digits_cnt
      *>        move '1' to digits_str(digits_cnt:1)
      *>        compute total_found = total_found - curr_range
      *>      else
      *>        add 1 to digits_cnt
      *>        move '0' to digits_str(digits_cnt:1)
      *>      end-if
      *>    end-if
        *> end-if
      *> else
        *> if total_found < (curr_range * -2)
      *>    display "ISSUES"
        *> else
      *>    if total_found < (curr_range * -1)
      *>      add 1 to digits_cnt
      *>      move '=' to digits_str(digits_cnt:1)
      *>      compute total_found = total_found - (curr_range * -2)
      *>    else
      *>      add 1 to digits_cnt
      *>      move '-' to digits_str(digits_cnt:1)
      *>      compute total_found = total_found - (curr_range * -1)
      *>    end-if
        *> end-if
      *> end-if
    *> end-if
    *> display "GEN: " gen_idx " digits " digits_str
  *> end-perform
