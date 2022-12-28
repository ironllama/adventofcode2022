           >>source format free
identification division.
program-id. 19b.

data division.
  working-storage section.
    *> For lib-readfile
    01 rf_all_lines.
      02 rf_cnt pic s9(8) comp value 0.
      02 rf_row pic x(9999) occurs 0 to 9999 times
          depending on rf_cnt indexed by rf_idx.

    01 bp_stuff.
      02 bp_cnt pic 9(2) comp.
      02 bp_bots occurs 30 times indexed by bp_idx.
        03 ore_bot_ore pic 9(2) comp.
        03 clay_bot_ore pic 9(2) comp.
        03 obsidian_bot_ore pic 9(2) comp.
        03 obsidian_bot_clay pic 9(2) comp.
        03 geode_bot_ore pic 9(2) comp.
        03 geode_bot_obsidian pic 9(2) comp.

    77 gen_buffer pic x(4).

    01 all_states.
      *> 02 high_state usage is index.
      02 create_state_ind pic x value 'N'.
        88 create_state value 'Y'.
        88 skip_state value 'N'.
      02 states_head usage is index.
      02 states_cnt pic 9(8) comp.
      02 states occurs 999999 times indexed by states_idx.
        03 minute pic 9(2) comp value 1.
        03 prior_state usage is index.
        03 minerals.
          05 ore_num pic 9(2) comp value 0.
          05 clay_num pic 9(2) comp value 0.
          05 obsidian_num pic 9(2) comp value 0.
          05 geode_num pic 9(2) comp value 0.
        03 robots.
          05 num_ore_bots pic 9(2) comp value 0.
          05 num_clay_bots pic 9(2) comp value 0.
          05 num_obsidian_bots pic 9(2) comp value 0.
          05 num_geode_bots pic 9(2) comp value 0.
        03 state_score pic 9(18).
        03 geode_bot_built_ind pic x value 'N'.
          88 geode_bot_built value 'Y'.
          88 geode_bot_not_built value 'N'.
        03 obsidian_bot_allowed_ind pic x value 'N'.
          88 obsidian_bot_allowed value 'Y'.
          88 obsidian_bot_not_allowed value 'N'.
        03 clay_bot_allowed_ind pic x value 'N'.
          88 clay_bot_allowed value 'Y'.
          88 clay_bot_not_allowed value 'N'.
        03 ore_bot_allowed_ind pic x value 'N'.
          88 ore_bot_allowed value 'Y'.
          88 ore_bot_not_allowed value 'N'.
        03 something_built_ind pic x value 'N'.
          88 something_built value 'Y'.
          88 nothing_built value 'N'.

    77 max_ore_total pic 9(2).
    77 max_ore_bots pic 9(2).
    77 max_clay_bots pic 9(2).
    77 max_obsidian_bots pic 9(2).

    01 curr_state.
      03 curr_minute pic 9(2) comp value 1.
      03 curr_prior_state usage is index.
      03 curr_minerals.
        05 curr_ore_num pic 9(2) comp value 0.
        05 curr_clay_num pic 9(2) comp value 0.
        05 curr_obsidian_num pic 9(2) comp value 0.
        05 curr_geode_num pic 9(2) comp value 0.
      03 curr_robots.
        05 curr_num_ore_bots pic 9(2) comp value 0.
        05 curr_num_clay_bots pic 9(2) comp value 0.
        05 curr_num_obsidian_bots pic 9(2) comp value 0.
        05 curr_num_geode_bots pic 9(2) comp value 0.
      03 curr_state_score pic 9(18).
      03 curr_geode_bot_built_ind pic x value 'N'.
        88 curr_geode_bot_built value 'Y'.
        88 curr_geode_bot_not_built value 'N'.
      03 curr_obsidian_bot_allowed_ind pic x value 'N'.
        88 curr_obsidian_bot_allowed value 'Y'.
        88 curr_obsidian_bot_not_allowed value 'N'.
      03 curr_clay_bot_allowed_ind pic x value 'N'.
        88 curr_clay_bot_allowed value 'Y'.
        88 curr_clay_bot_not_allowed value 'N'.
      03 curr_ore_bot_allowed_ind pic x value 'N'.
        88 curr_ore_bot_allowed value 'Y'.
        88 curr_ore_bot_not_allowed value 'N'.
      03 curr_something_built_ind pic x value 'N'.
        88 curr_something_built value 'Y'.
        88 curr_nothing_built value 'N'.
      *> 03 state_score_str pic x(18).

    77 orig_ore_num pic 9(2) comp.
    77 orig_clay_num pic 9(2) comp.
    77 orig_obsidian_num pic 9(2) comp.
    77 orig_geode_num pic 9(2) comp.

    01 best_minutes_stuff.
      02 best_minute occurs 32 times.
        03 best_geode_num pic 9(2) comp.

    77 print_idx usage is index.

    77 total_found pic s9(8) comp.

    77 max_minutes pic 9(2) comp.

procedure division.
  call 'lib-readdata' using function module-id ".dat" rf_all_lines
*>   call 'lib-readdata' using function module-id ".da1" rf_all_lines

*>   move 0 to total_found
  move 1 to total_found

  perform varying rf_idx from 1 by 1 until rf_idx > rf_cnt
    *> display "LINE: " function trim(rf_row(rf_idx))
    add 1 to bp_cnt
    move rf_row(rf_idx)(35:2) to ore_bot_ore(bp_cnt)
    move rf_row(rf_idx)(64:2) to clay_bot_ore(bp_cnt)

    move rf_row(rf_idx)(97:2) to obsidian_bot_ore(bp_cnt)
    move rf_row(rf_idx)(107:3) to gen_buffer
    if gen_buffer(3:1) = "c" move space to gen_buffer(3:1) end-if
    move function trim(gen_buffer) to obsidian_bot_clay(bp_cnt)

    move rf_row(rf_idx)(138:3) to gen_buffer
    if gen_buffer(1:1) = "s" move space to gen_buffer(1:1) end-if
    if gen_buffer(3:1) = "o" move space to gen_buffer(3:1) end-if
    move function trim(gen_buffer) to geode_bot_ore(bp_cnt)
    move rf_row(rf_idx)(148:4) to gen_buffer
    if gen_buffer(1:1) = "d" move space to gen_buffer(1:1) end-if
    if gen_buffer(4:1) = "o" move space to gen_buffer(4:1) end-if
    move function trim(gen_buffer) to geode_bot_obsidian(bp_cnt)
  end-perform
  *> Display results of parsing.
  *> perform varying bp_idx from 1 by 1 until bp_idx > bp_cnt
  *>   display "BOT[" bp_idx "]: ORE: " ore_bot_ore(bp_idx) "  CLY: " clay_bot_ore(bp_idx) "  OBS: " obsidian_bot_ore(bp_idx) " + " obsidian_bot_clay(bp_idx) "  GEO: " geode_bot_ore(bp_idx) " + " geode_bot_obsidian(bp_idx)
  *> end-perform

  move 32 to max_minutes

*>   perform varying bp_idx from 1 by 1 until bp_idx > bp_cnt
  perform varying bp_idx from 1 by 1 until bp_idx > 3

    initialize all_states
    initialize curr_state
    *> initialize high_state
    initialize best_minutes_stuff

    add 1 to states_cnt
    move 0 to minute(states_cnt)
    move 1 to num_ore_bots(states_cnt)
    move 0 to prior_state(states_cnt)
    set something_built(states_cnt) to true

    compute max_ore_bots = ore_bot_ore(bp_idx) + clay_bot_ore(bp_idx) + obsidian_bot_ore(bp_idx) + geode_bot_ore(bp_idx)
    compute max_ore_total = ore_bot_ore(bp_idx) + clay_bot_ore(bp_idx) + obsidian_bot_ore(bp_idx) + geode_bot_ore(bp_idx)
    move obsidian_bot_clay(bp_idx) to max_clay_bots
    move geode_bot_obsidian(bp_idx) to max_obsidian_bots

    *> display "BOT MAXES: ORE: " max_ore_bots " CLY: " max_clay_bots " OBS: " max_obsidian_bots

    *> BFS
    *> move 0 to states_head
    *> perform process_state until states_head > states_cnt
    *> perform process_state until states_head > states_cnt or curr_minute = 25

    *> DFS
    perform process_state until states_cnt = 0

    *> display "MIN[" high_minute "][" high_state_score "]: ORE " high_ore_num " CLY: " high_clay_num " OBS: " high_obsidian_num " GEO: " high_geode_num " === BOTS: ORE: " high_num_ore_bots " CLY: " high_num_clay_bots " OBS: " high_num_obsidian_bots " GEO: " high_num_geode_bots
    *> display "HIGH BP_IDX: [" bp_idx "] GEODE_NUM: " high_geode_num
    *> compute total_found = total_found + (bp_idx * high_geode_num)

    display "BEST BP_IDX: [" bp_idx "] GEODE_NUM: " best_geode_num(max_minutes)
    *> compute total_found = total_found + (bp_idx * best_geode_num(max_minutes))
    compute total_found = total_found * best_geode_num(max_minutes)
  end-perform

  display "FINAL: " total_found

  goback.


process_state.
  *> add 1 to states_head  *> Next in "queue". (BFS)
  *> Make a copy for convenience. Also, to avoid modifying state (if reusing as visited states)
  *> move states(states_head) to curr_state

  move states(states_cnt) to curr_state  *> DFS
  subtract 1 from states_cnt  *> Remove from "stack"

  add 1 to curr_minute

  *> move states_cnt to print_idx
  *> display "START [" states_cnt "] " no advancing
  *> perform print_state

  *> Store original amounts to determine bot creation options, which use amounts prior to collection.
  move curr_ore_num to orig_ore_num
  move curr_clay_num to orig_clay_num
  move curr_obsidian_num to orig_obsidian_num
  move curr_geode_num to orig_geode_num

  *> Mineral collection.
  add curr_num_ore_bots to curr_ore_num
  if curr_num_clay_bots > 0
    add curr_num_clay_bots to curr_clay_num
  end-if
  if curr_num_obsidian_bots > 0
    add curr_num_obsidian_bots to curr_obsidian_num
  end-if
  if curr_num_geode_bots > 0
    add curr_num_geode_bots to curr_geode_num
  end-if

  *> display "curr_something_built: " curr_something_built_ind " OR: " curr_ore_bot_allowed_ind " CL: " curr_clay_bot_allowed_ind " OB: " curr_obsidian_bot_allowed_ind " GE: " curr_geode_bot_built_ind

  *> Building new bots and adding new decision branches.
  set curr_geode_bot_not_built to true
  if orig_obsidian_num >= geode_bot_obsidian(bp_idx) and orig_ore_num >= geode_bot_ore(bp_idx)
    subtract geode_bot_obsidian(bp_idx) from curr_obsidian_num
    subtract geode_bot_ore(bp_idx) from curr_ore_num
    add 1 to curr_num_geode_bots
    set curr_geode_bot_built to true
    set curr_something_built to true
    perform add_branch

    *> Gross. But without local variables... meh.
    subtract 1 from curr_num_geode_bots
    add geode_bot_obsidian(bp_idx) to curr_obsidian_num
    add geode_bot_ore(bp_idx) to curr_ore_num
  end-if

  if curr_geode_bot_not_built
      and (curr_something_built or (curr_nothing_built and curr_obsidian_bot_allowed))
      and orig_clay_num >= obsidian_bot_clay(bp_idx) and orig_ore_num >= obsidian_bot_ore(bp_idx)
      and curr_num_obsidian_bots < max_obsidian_bots
    subtract obsidian_bot_clay(bp_idx) from curr_clay_num
    subtract obsidian_bot_ore(bp_idx) from curr_ore_num
    add 1 to curr_num_obsidian_bots
    set curr_obsidian_bot_not_allowed to true
    set curr_something_built to true
    perform add_branch

    *> Gross. But without local variables... meh.
    subtract 1 from curr_num_obsidian_bots
    add obsidian_bot_clay(bp_idx) to curr_clay_num
    add obsidian_bot_ore(bp_idx) to curr_ore_num
  else
    set curr_obsidian_bot_allowed to true
  end-if

  if curr_geode_bot_not_built
      and (curr_something_built or (curr_nothing_built and curr_clay_bot_allowed))
      and orig_ore_num >= clay_bot_ore(bp_idx) and curr_num_clay_bots < max_clay_bots
    subtract clay_bot_ore(bp_idx) from curr_ore_num
    add 1 to curr_num_clay_bots
    set curr_clay_bot_not_allowed to true
    set curr_something_built to true
    perform add_branch

    *> Gross. But without local variables... meh.
    subtract 1 from curr_num_clay_bots
    add clay_bot_ore(bp_idx) to curr_ore_num
  else
    set curr_clay_bot_allowed to true
  end-if

  if curr_geode_bot_not_built
      and (curr_something_built or (curr_nothing_built and curr_ore_bot_allowed))
      and orig_ore_num >= ore_bot_ore(bp_idx) and curr_num_ore_bots < max_ore_bots
    subtract ore_bot_ore(bp_idx) from curr_ore_num
    add 1 to curr_num_ore_bots
    set curr_ore_bot_not_allowed to true
    set curr_something_built to true
    perform add_branch

    *> Gross. But without local variables... meh.
    subtract 1 from curr_num_ore_bots
    add ore_bot_ore(bp_idx) to curr_ore_num
  else
    set curr_ore_bot_allowed to true
  end-if

  *> Sometimes, you do nothing. Not sure if this is helpful, though, as it creates lots of uninteresting states?
  if curr_geode_bot_not_built
    set curr_nothing_built to true
    perform add_branch
  end-if

  *> move states_head to print_idx
  *> display "END " no advancing
  *> perform print_state
  .

add_branch.
  *> Prevent duplicate states.
  set create_state to true

  *> Pruning. Test for highest is at the end of each round, while adding happens in the middle.
  if curr_minute >= max_minutes  *> Limit of puzzle.
      or curr_ore_num > max_ore_total  *> Stop branches with ore hogs.
    set skip_state to true
  end-if

  *> Prune if there are geodes and it's not the best for its minute. (Slight possibility of suboptimum, but whatevs.)
  if curr_geode_num >= best_geode_num(curr_minute)
    move curr_geode_num to best_geode_num(curr_minute)
    *> display "NEW BEST: [" curr_minute "]: " best_geode_num(curr_minute)
  else
    set skip_state to true
  end-if

  *> if curr_geode_num > 0 and curr_geode_num > high_geode_num
  *>   move curr_state to high_state
  *> end-if

  if create_state
    add 1 to states_cnt
    move curr_state to states(states_cnt)

    *> if minute(states_cnt) = 23 and num_geode_bots(states_cnt) > 2 and geode_num(states_cnt) > 6
    *>   move states_cnt to print_idx
    *>   display "ADDED " no advancing
    *>   perform print_state
    *> end-if
  end-if
  .

print_state.
  display "MIN[" minute(print_idx) "][" state_score(print_idx) "]: ORE " ore_num(print_idx) " CLY: " clay_num(print_idx) " OBS: " obsidian_num(print_idx) " GEO: " geode_num(print_idx) " === BOTS: ORE: " num_ore_bots(print_idx) " CLY: " num_clay_bots(print_idx) " OBS: " num_obsidian_bots(print_idx) " GEO: " num_geode_bots(print_idx)
  .

print_curr_state.
  display "MIN[" curr_minute "][" curr_state_score "]: ORE " curr_ore_num " CLY: " curr_clay_num " OBS: " curr_obsidian_num " GEO: " curr_geode_num " === BOTS: ORE: " curr_num_ore_bots " CLY: " curr_num_clay_bots " OBS: " curr_num_obsidian_bots " GEO: " curr_num_geode_bots
  .
