           >>source format free
identification division.
program-id. lib-readdata.
author. alexoh@wcoding.

environment division.
  input-output section.
    file-control.
      select ifile assign to filename
        organization is line sequential
        file status is filestat.

data division.
  file section.
    fd ifile.
    *>   record is varying in size from 9 to 9999
    *>   depending on ln-max_line_len.
    *> May have to change the size of the line per puzzle input!
    01 fileline pic x(9999).

  working-storage section.
    01 filename pic x(10).
    01 filestat pic xx.
    77 eof pic x.

  linkage section.
    01 ln-filename pic x(10).
    01 ln-filesuffix pic x(4).
    *> 01 ln-max_line_len pic s9(8) comp.
    01 ln-all_lines.
     02 line_cnt pic s9(8) comp value 0.
     *> May have to change the size of the line per puzzle input!
     02 line_row pic x(9999) occurs 0 to unbounded
         depending on line_cnt indexed by line_idx.

procedure division using ln-filename ln-filesuffix ln-all_lines.
  *> To test with a local file
  *> string function module-id delimited by size
  *>   ".txt"
  *>   into filename
  *> end-string.
  *> display filename.

  string ln-filename(1:2) delimited by spaces
    ln-filesuffix
    into filename
  end-string.

  display "readdata: Opening file. filename: " filename
  open input ifile.

  if filestat = "35" or filestat = "05" then
    display "File does not exist: " filename
    goback
  end-if.

  move 0 to line_cnt
  perform until eof = 'Y'
    read ifile at end move 'Y' to eof
      not at end
        add 1 to line_cnt
        *> display ">>" function trim(fileline)
        *> move function trim(fileline) to line_row(line_cnt)
        move fileline to line_row(line_cnt)
    end-read
  end-perform.

  close ifile.

*>   display function trim(line_row(line_cnt))
*>   perform varying line_idx from 1 by 1 until line_idx > line_cnt
*>     display function trim(line_row(line_idx))
*>   end-perform.

  display "readdata: filename: " function trim(filename) " line_cnt:" line_cnt " char_cnt(1): " length of function trim(line_row(1))
  if length of function trim(line_row(1)) = length of fileline
    display "readdata: WARNING: You may be OVERFLOWING on your read!"
  end-if
  goback.
