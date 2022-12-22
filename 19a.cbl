           >>source format free
identification division.
program-id. 19a.

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

    77 gen_cnt pic 9(3) comp.
    77 gen_buffer pic x(4).

    77 gen_idx usage is index.
    77 gen_idx_disp pic z9.
    77 gen_idx2 usage is index.
    77 gen_done pic 9.
    77 gen_char pic x.
    77 gen_word pic x(2).

    01 to_build.
      02 ore_bots_building pic 9(2) comp value 0.
      02 clay_bots_building pic 9(2) comp value 0.
      02 obsidian_bots_building pic 9(2) comp value 0.
      02 geode_bots_building pic 9(2) comp value 0.

    01 all_states.
      *> 02 high_state usage is index.
      02 create_state pic 9.
      *> 02 states_head usage is index.
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
        *> 03 state_score_str pic x(18).

    01 v_all_states.
      *> 02 high_state usage is index.
      02 v_create_state pic 9.
      02 states_head usage is index.
      02 v_states_cnt pic 9(8) comp.
      02 v_states occurs 999999 times indexed by v_states_idx.
        03 v_minute pic 9(2) comp value 1.
        03 v_prior_state usage is index.
        03 v_minerals.
          05 v_ore_num pic 9(2) comp value 0.
          05 v_clay_num pic 9(2) comp value 0.
          05 v_obsidian_num pic 9(2) comp value 0.
          05 v_geode_num pic 9(2) comp value 0.
        03 v_robots.
          05 v_num_ore_bots pic 9(2) comp value 0.
          05 v_num_clay_bots pic 9(2) comp value 0.
          05 v_num_obsidian_bots pic 9(2) comp value 0.
          05 v_num_geode_bots pic 9(2) comp value 0.
        03 v_state_score pic 9(18).

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
      *> 03 state_score_str pic x(18).

    77 orig_ore_num pic 9(2) comp.
    77 orig_clay_num pic 9(2) comp.
    77 orig_obsidian_num pic 9(2) comp.
    77 orig_geode_num pic 9(2) comp.
    77 bot_built pic 9.

    01 high_state.
      03 high_minute pic 9(2) comp value 1.
      03 high_prior_state usage is index.
      03 high_minerals.
        05 high_ore_num pic 9(2) comp value 0.
        05 high_clay_num pic 9(2) comp value 0.
        05 high_obsidian_num pic 9(2) comp value 0.
        05 high_geode_num pic 9(2) comp value 0.
      03 high_robots.
        05 high_num_ore_bots pic 9(2) comp value 0.
        05 high_num_clay_bots pic 9(2) comp value 0.
        05 high_num_obsidian_bots pic 9(2) comp value 0.
        05 high_num_geode_bots pic 9(2) comp value 0.
      03 high_state_score pic 9(18).

    01 best_minutes_stuff.
      02 best_minute_cnt pic 9(2) comp.
      02 best_minute occurs 24 times.
        03 best_geode_minute pic 9(2) comp.
        03 best_geode_bots pic 9(2) comp.
        03 best_geode_num pic 9(2) comp.

    77 print_idx usage is index.

    77 total_found pic s9(8) comp.

procedure division.
*>   call 'lib-readdata' using function module-id ".dat" rf_all_lines
  call 'lib-readdata' using function module-id ".da1" rf_all_lines

  move 0 to total_found
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

  perform varying bp_idx from 1 by 1 until bp_idx > bp_cnt
    display "BOT[" bp_idx "]: ORE: " ore_bot_ore(bp_idx) "  CLY: " clay_bot_ore(bp_idx) "  OBS: " obsidian_bot_ore(bp_idx) " + " obsidian_bot_clay(bp_idx) "  GEO: " geode_bot_ore(bp_idx) " + " geode_bot_obsidian(bp_idx)
  end-perform

*>   perform varying bp_idx from 1 by 1 until bp_idx > bp_cnt
  perform varying bp_idx from 1 by 1 until bp_idx > 1
    move 2 to bp_idx

    initialize all_states
    initialize v_all_states
    initialize curr_state
    initialize high_state
    initialize best_minutes_stuff

    add 1 to states_cnt
    move 1 to minute(states_cnt)
    move 1 to num_ore_bots(states_cnt)

    *> move states_cnt to print_idx
    *> perform print_state

  *>   move 1 to bp_idx
    *> move 2 to bp_idx
    compute max_ore_bots = ore_bot_ore(bp_idx) + clay_bot_ore(bp_idx) + obsidian_bot_ore(bp_idx) + geode_bot_ore(bp_idx)
    *> move 2 to max_ore_bots
    compute max_ore_total = ore_bot_ore(bp_idx) + clay_bot_ore(bp_idx) + obsidian_bot_ore(bp_idx) + geode_bot_ore(bp_idx)
    *> compute max_ore_bots = function max(ore_bot_ore(bp_idx) clay_bot_ore(bp_idx) obsidian_bot_ore(bp_idx) geode_bot_ore(bp_idx))

    move obsidian_bot_clay(bp_idx) to max_clay_bots
    move geode_bot_obsidian(bp_idx) to max_obsidian_bots

    display "BOT MAXES: ORE: " max_ore_bots " CLY: " max_clay_bots " OBS: " max_obsidian_bots

    *> BFS
    move 1 to states_head
    perform process_state until states_head > states_cnt
    *> perform process_state until states_head > states_cnt or curr_minute = 25

    *> DFS
    *> perform process_state until states_cnt = 0

    *> display "HIGHEST : " no advancing
    *> move high_state to print_idx
    *> perform print_state
    *> display "FINAL: " geode_num(high_state)

    *> display "MIN[" high_minute "][" high_state_score "]: ORE " high_ore_num " CLY: " high_clay_num " OBS: " high_obsidian_num " GEO: " high_geode_num " === BOTS: ORE: " high_num_ore_bots " CLY: " high_num_clay_bots " OBS: " high_num_obsidian_bots " GEO: " high_num_geode_bots
    *> move high_prior_state to print_idx
    *> perform varying gen_idx from 1 by 1 until print_idx < 1
    *>   perform print_v_state
    *>   move v_prior_state(print_idx) to print_idx
    *> end-perform



    display "HIGH: [" bp_idx "] " high_geode_num

    compute total_found = total_found + (bp_idx * high_geode_num)
  end-perform

  display "FINAL: " total_found

  goback.


process_state.
  *> move states_cnt to print_idx
  *> display "START " no advancing
  *> perform print_state

  *> Make a copy for convenience.
*>   move states(states_head) to curr_state  *> BFS
*>   add 1 to states_head  *> Remove from "queue". (BFS)
  move states(states_cnt) to curr_state  *> DFS
  subtract 1 from states_cnt  *> Remove from "stack". (DFS)

  *> Add to visited states.
*>   add 1 to v_states_cnt
*>   move curr_state to v_states(v_states_cnt)



  *> Get score of this branch
  *> Comp values produce garbage if treated as alpha without move.
  *> string curr_num_geode_bots
      *>   curr_geode_num
      *>   curr_num_obsidian_bots
      *>   curr_obsidian_num
      *>   curr_num_clay_bots
      *>   curr_clay_num
      *>   curr_num_ore_bots
      *>   curr_ore_num
      *>   into curr_state_score_str
  *> end-string
  *> move curr_state_score_str to curr_state_score
  *> display "SCORE: " curr_state_score_str " " curr_state_score
  compute curr_state_score =
         ((24 - curr_minute)* 10000000000000000) +
         (curr_num_geode_bots * 100000000000000) +
         (curr_geode_num        * 1000000000000) +
         (curr_num_obsidian_bots  * 10000000000) +
         (curr_obsidian_num         * 100000000) +
         (curr_num_clay_bots          * 1000000) +
         (curr_clay_num                 * 10000) +
         (curr_num_ore_bots               * 100) +
         curr_ore_num
  *> display "SCORE: " curr_state_score

  *> Store original amounts to determine bot creation options.
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

  *> Sometimes, you do nothing. Not sure if this is helpful, though, as it creates lots of uninteresting states?
  perform add_branch
  *> move states_cnt to print_idx
  *> display "ADDED NON: " no advancing
  *> perform print_state

  *> Building new bots and adding new decision branches.
  move 0 to bot_built
  if orig_obsidian_num >= geode_bot_obsidian(bp_idx) and orig_ore_num >= geode_bot_ore(bp_idx)
    *> Can we add more than one bot in a turn? Would that even make sense? Disabling, because it starts to add a ton of branches later on.
    *> perform until curr_obsidian_num < geode_bot_obsidian(bp_idx) or curr_ore_num < geode_bot_ore(bp_idx)
    *>   perform add_branch
      subtract geode_bot_obsidian(bp_idx) from curr_obsidian_num
      subtract geode_bot_ore(bp_idx) from curr_ore_num
      add 1 to curr_num_geode_bots
      add 1 to bot_built
      perform add_branch

      *> move states_cnt to print_idx
      *> display "ADDED GEO: " no advancing
      *> perform print_state
    *> end-perform
  end-if
  if bot_built = 0
      and orig_clay_num >= obsidian_bot_clay(bp_idx) and orig_ore_num >= obsidian_bot_ore(bp_idx)
      and curr_num_obsidian_bots < max_obsidian_bots
    *> perform until curr_clay_num < obsidian_bot_clay(bp_idx) or curr_ore_num < obsidian_bot_ore(bp_idx)
      *> Even though we have enough, we'll skip?
    *>   perform add_branch
      subtract obsidian_bot_clay(bp_idx) from curr_clay_num
      subtract obsidian_bot_ore(bp_idx) from curr_ore_num
      add 1 to curr_num_obsidian_bots
      add 1 to bot_built
      perform add_branch

      *> move states_cnt to print_idx
      *> display "ADDED OBS: " no advancing
      *> perform print_state
    *> end-perform
  end-if
  if bot_built = 0
      and orig_ore_num >= clay_bot_ore(bp_idx) and curr_num_clay_bots < max_clay_bots
    *> perform until curr_ore_num < clay_bot_ore(bp_idx)
      *> Even though we have enough, we'll skip?
    *>   perform add_branch
      subtract clay_bot_ore(bp_idx) from curr_ore_num
      add 1 to curr_num_clay_bots
      add 1 to bot_built
      perform add_branch

      *> move states_cnt to print_idx
      *> display "ADDED CLY: " no advancing
      *> perform print_state
    *> end-perform
  end-if
  if bot_built = 0
      and orig_ore_num >= ore_bot_ore(bp_idx) and curr_num_ore_bots < max_ore_bots
    *> perform until curr_ore_num < ore_bot_ore(bp_idx)
      *> Even though we have enough, we'll add a skip scenario for each possible new collector.
    *>   perform add_branch
      *> Add branch where something is done!
      subtract ore_bot_ore(bp_idx) from curr_ore_num
      add 1 to curr_num_ore_bots
      add 1 to bot_built
      perform add_branch

      *> move states_cnt to print_idx
      *> display "ADDED ORE: " no advancing
      *> perform print_state
    *> end-perform
  end-if

  *> if curr_geode_num > geode_num(high_state)
    *> move states_head to high_state
  *> end-if
  if curr_geode_num > high_geode_num
    move curr_state to high_state
    compute high_minute = high_minute + 1
    move v_states_cnt to high_prior_state
  end-if

  *> move states_head to print_idx
  *> display "END " no advancing
  *> perform print_state

  *> add 1 to states_head
  .

add_branch.
  *> Prevent duplicate states.
  move 1 to create_state

  *> Pruning.
  *> Test for highest is at the end of each round, while adding happens in the middle. So, we end if 24.
  if curr_minute >= 24  *> Limit of puzzle.
      or curr_ore_num > max_ore_total  *> Stop branches with ore hogs.
    *>   or (curr_minute > 20 and curr_state_score < 100000000000000)  *> If you don't have a geode_bot by 20, you're not doing your job.
    *>   or curr_geode_num = 1
    *> or curr_obsidian_num = 1
    *> display "SKIP: INITIAL PRUNING."
    move 0 to create_state
  end-if

*>   if create_state = 1 and curr_geode_num = 1
*>     move 0 to create_state

*>     if curr_state_score > high_state_score
*>       move curr_state to high_state
*>       compute high_minute = high_minute + 1
*>       move v_states_cnt to high_prior_state
*>     end-if

*>     *> display "OBS: " no advancing
*>     *> perform print_curr_state
*>   end-if

*>   *> See if this end of path is the best from the first time a geode bot is seen.
*>   if create_state = 1
*>     if curr_num_obsidian_bots < best_geode_bots(curr_minute)
*>         *> and curr_geode_num < best_geode_num
*>       *> display "SKIP: NOT BEST ENDING."
*>       move 0 to create_state
*>     else
*>       if best_geode_minute(curr_minute) = 0
*>       *> or ((curr_minute + 1) < best_geode_minute and curr_num_geode_bots >= best_geode_bots)
*>           or (curr_num_obsidian_bots > best_geode_bots(curr_minute))
*>       *> and curr_geode_num >= best_geode_num
*>         compute best_geode_minute(curr_minute) = curr_minute + 1
*>         move curr_num_obsidian_bots to best_geode_bots(curr_minute)
*>         *> move curr_geode_num to best_geode_num
*>         *> display "BEST: NEW VALUE! " best_geode_minute(curr_minute) " " best_geode_bots(curr_minute) " " best_geode_num(curr_minute)
*>       end-if
*>     end-if
*>   end-if

  *> *> See if this end of path is the best from the first time a geode bot is seen.
  *> if create_state = 1 and curr_minute > 15
  *>   if curr_num_geode_bots < best_geode_bots(curr_minute)
  *>       *> and curr_geode_num < best_geode_num
  *>     *> display "SKIP: NOT BEST ENDING."
  *>     move 0 to create_state
  *>   else
  *>     if best_geode_minute(curr_minute) = 0
  *>     *> or ((curr_minute + 1) < best_geode_minute and curr_num_geode_bots >= best_geode_bots)
  *>         or (curr_num_geode_bots > best_geode_bots(curr_minute))
  *>     *> and curr_geode_num >= best_geode_num
  *>       compute best_geode_minute(curr_minute) = curr_minute + 1
  *>       move curr_num_geode_bots to best_geode_bots(curr_minute)
  *>       *> move curr_geode_num to best_geode_num
  *>       *> display "BEST: NEW VALUE! " best_geode_minute(curr_minute) " " best_geode_bots(curr_minute) " " best_geode_num(curr_minute)
  *>     end-if
  *>   end-if
  *> end-if

  *> Avoid states already visited. (Mostly for DFS)
*>   perform varying v_states_idx from 1 by 1 until v_states_idx > v_states_cnt or create_state = 0
*>     if v_states(v_states_idx) = curr_state
*>     *> if curr_minute + 1 = v_minute(v_states_idx)
*>     *>     and curr_ore_num = v_ore_num(v_states_idx)
*>     *>     and curr_clay_num = v_clay_num(v_states_idx)
*>     *>     and curr_obsidian_num = v_obsidian_num(v_states_idx)
*>     *>     and curr_geode_num = v_geode_num(v_states_idx)
*>     *>     and curr_num_ore_bots = v_num_ore_bots(v_states_idx)
*>     *>     and curr_num_clay_bots = v_num_clay_bots(v_states_idx)
*>     *>     and curr_num_obsidian_bots = v_num_obsidian_bots(v_states_idx)
*>     *>     and curr_num_geode_bots = v_num_geode_bots(v_states_idx)
*>         *> display "SKIP: DUPE!"
*>       move 0 to create_state
*>     end-if
*>   end-perform
  *> perform varying states_idx from 1 by 1 until states_idx > states_cnt or create_state = 0
  *>   *> if states_idx <> states_head  *> Only relevant for BFS?
  *>     if minute(states_idx) = curr_minute + 1
  *>     *>    and minerals(states_idx) = curr_minerals
  *>     *>    and robots(states_idx) = curr_robots
  *>     *>     and state_score(states_idx) = curr_state_score
  *>          and curr_ore_num = ore_num(states_idx)
  *>          and curr_clay_num = clay_num(states_idx)
  *>          and curr_obsidian_num = obsidian_num(states_idx)
  *>          and curr_geode_num = geode_num(states_idx)
  *>          and curr_num_ore_bots = num_ore_bots(states_idx)
  *>          and curr_num_clay_bots = num_clay_bots(states_idx)
  *>          and curr_num_obsidian_bots = num_obsidian_bots(states_idx)
  *>          and curr_num_geode_bots = num_geode_bots(states_idx)
  *>         display "DUPE!"
  *>       move 0 to create_state
  *>     end-if
  *>   *> end-if
  *> end-perform

  if create_state = 1
    add 1 to states_cnt
    initialize states(states_cnt)

    move curr_state to states(states_cnt)
    *> move curr_minute to minute(states_cnt)
    *> move curr_ore_num to ore_num(states_cnt)
    *> move curr_clay_num to clay_num(states_cnt)
    *> move curr_obsidian_num to obsidian_num(states_cnt)
    *> move curr_geode_num to geode_num(states_cnt)
    *> move curr_num_ore_bots to num_ore_bots(states_cnt)
    *> move curr_num_clay_bots to num_clay_bots(states_cnt)
    *> move curr_num_obsidian_bots to num_obsidian_bots(states_cnt)
    *> move curr_num_geode_bots to num_geode_bots(states_cnt)

    *> if curr_minute <> minute(states_cnt)
    *>     or curr_ore_num <> ore_num(states_cnt)
    *>     or curr_clay_num <> clay_num(states_cnt)
    *>     or curr_obsidian_num <> obsidian_num(states_cnt)
    *>     or curr_geode_num <> geode_num(states_cnt)
    *>     or curr_num_ore_bots <> num_ore_bots(states_cnt)
    *>     or curr_num_clay_bots <> num_clay_bots(states_cnt)
    *>     or curr_num_obsidian_bots <> num_obsidian_bots(states_cnt)
    *>     or curr_num_geode_bots <> num_geode_bots(states_cnt)
    *>   display ">>>>>>>>>>>>> NOT COPYING CORRECTLY <<<<<<<<<<<<"
    *> end-if

    add 1 to minute(states_cnt)
    move v_states_cnt to prior_state(states_cnt)

    *> move states_cnt to print_idx
    *> display "ADDED " no advancing
    *> perform print_state
  end-if
  .

print_state.
  display "MIN[" minute(print_idx) "][" state_score(print_idx) "]: ORE " ore_num(print_idx) " CLY: " clay_num(print_idx) " OBS: " obsidian_num(print_idx) " GEO: " geode_num(print_idx) " === BOTS: ORE: " num_ore_bots(print_idx) " CLY: " num_clay_bots(print_idx) " OBS: " num_obsidian_bots(print_idx) " GEO: " num_geode_bots(print_idx)
  .

print_v_state.
  display "MIN[" v_minute(print_idx) "][" v_state_score(print_idx) "]: ORE " v_ore_num(print_idx) " CLY: " v_clay_num(print_idx) " OBS: " v_obsidian_num(print_idx) " GEO: " v_geode_num(print_idx) " === BOTS: ORE: " v_num_ore_bots(print_idx) " CLY: " v_num_clay_bots(print_idx) " OBS: " v_num_obsidian_bots(print_idx) " GEO: " v_num_geode_bots(print_idx)
  .

print_curr_state.
  display "MIN[" curr_minute "][" curr_state_score "]: ORE " curr_ore_num " CLY: " curr_clay_num " OBS: " curr_obsidian_num " GEO: " curr_geode_num " === BOTS: ORE: " curr_num_ore_bots " CLY: " curr_num_clay_bots " OBS: " curr_num_obsidian_bots " GEO: " curr_num_geode_bots
  .

*>  77 skip_ore_bots pic 9.
*>  77 skip_clay_bots pic 9.
*>  77 skip_obsidian_bots pic 9.
*>
*>  perform varying bp_idx from 1 by 1 until bp_idx > bp_cnt
*>    initialize to_build
*>    initialize minerals
*>    initialize robots
*>
*>    *> move 0 to ore_bots_building
*>    *> move 0 to clay_bots_building
*>    *> move 0 to obsidian_bots_building
*>    *> move 0 to geode_bots_building
*>    *> move 0 to ore_num
*>    *> move 0 to clay_num
*>    *> move 0 to obsidian_num
*>    *> move 0 to geode_num
*>    *> move 0 to num_ore_bots
*>    *> move 0 to num_clay_bots
*>    *> move 0 to num_obsidian_bots
*>    *> move 0 to num_geode_bots
*>
*>    move 1 to num_ore_bots
*>
*>    display space
*>    display space
*>    display "BOT[" bp_idx "]: ORE: " ore_bot_ore(bp_idx) "  CLY: " clay_bot_ore(bp_idx) "  OBS: " obsidian_bot_ore(bp_idx) " + " obsidian_bot_clay(bp_idx) "  GEO: " geode_bot_ore(bp_idx) " + " geode_bot_obsidian(bp_idx)
*>    display "BUILD: " ore_bots_building " " clay_bots_building " " obsidian_bots_building " " geode_bots_building
*>    display "MINERALS: " ore_num " " clay_num " " obsidian_num " " geode_num
*>    display "BOTS: " num_ore_bots " " num_clay_bots " " num_obsidian_bots " " num_geode_bots
*>
*>    perform varying gen_idx from 1 by 1 until gen_idx > 24
*>      display space
*>      move gen_idx to gen_idx_disp
*>      display "== Minute " function trim(gen_idx_disp) " =="
*>      *> First, start building new bots.
*>
*>      if
*>          obsidian_num >= geode_bot_obsidian(bp_idx) and
*>          ore_num > geode_bot_ore(bp_idx)
*>        perform until obsidian_num < geode_bot_obsidian(bp_idx) or ore_num < geode_bot_ore(bp_idx)
*>          subtract geode_bot_obsidian(bp_idx) from obsidian_num
*>          subtract geode_bot_ore(bp_idx) from ore_num
*>          add 1 to geode_bots_building
*>          display "Spend " geode_bot_ore(bp_idx) " ore and " geode_bot_obsidian(bp_idx) " obsidian to start building a geode-cracking robot."
*>        end-perform
*>      end-if
*>      if
*>          skip_obsidian_bots = 0 and
*>          clay_num >= obsidian_bot_clay(bp_idx) and
*>          ore_num >= obsidian_bot_ore(bp_idx)
*>        perform until clay_num < obsidian_bot_clay(bp_idx) or ore_num < obsidian_bot_ore(bp_idx)
*>          subtract obsidian_bot_clay(bp_idx) from clay_num
*>          subtract obsidian_bot_ore(bp_idx) from ore_num
*>          add 1 to obsidian_bots_building
*>          display "Spend " obsidian_bot_ore(bp_idx) " ore and " obsidian_bot_clay(bp_idx) " clay to start building an obsidian-collecting robot."
*>        end-perform
*>      end-if
*>      if
*>          skip_clay_bots = 0 and
*>          ore_num >= clay_bot_ore(bp_idx)
*>        perform until ore_num < clay_bot_ore(bp_idx)
*>          subtract clay_bot_ore(bp_idx) from ore_num
*>          add 1 to clay_bots_building
*>          display "Spend " clay_bot_ore(bp_idx) " ore to start building a clay-collecting robot."
*>        end-perform
*>      end-if
*>      if
*>          skip_ore_bots = 0 and
*>          ore_num >= ore_bot_ore(bp_idx)
*>        perform until ore_num < ore_bot_ore(bp_idx)
*>          subtract ore_bot_ore(bp_idx) from ore_num
*>          add 1 to ore_bots_building
*>          display "Spend " clay_bot_ore(bp_idx) " ore to start building a ore-collecting robot."
*>        end-perform
*>      end-if
*>
*>      *> Second, collection.
*>      add num_ore_bots to ore_num
*>      display num_ore_bots " ore-collecting robot collects " num_ore_bots " ore; you now have " ore_num " ore."
*>      if num_clay_bots > 0
*>        add num_clay_bots to clay_num
*>        display num_clay_bots " clay-collecting robot collects " num_clay_bots " clay; you now have " clay_num " clay."
*>      end-if
*>      if num_obsidian_bots > 0
*>        add num_obsidian_bots to obsidian_num
*>        display num_obsidian_bots " obsidian-collecting robot collects " num_obsidian_bots " obsidian; you now have " obsidian_num " obsidian."
*>      end-if
*>      if num_geode_bots > 0
*>        add num_geode_bots to geode_num
*>        display num_geode_bots " geode-collecting robot cracks " num_geode_bots " geode; you now have " geode_num " open geodes."
*>      end-if
*>
*>      *> Then create new bots.
*>      if ore_bots_building > 0
*>        add ore_bots_building to num_ore_bots
*>        move 0 to ore_bots_building
*>        display "The new ore-collecting robot is ready; you now have " num_ore_bots " of them."
*>      end-if
*>      if clay_bots_building > 0
*>        add clay_bots_building to num_clay_bots
*>        move 0 to clay_bots_building
*>        display "The new clay-collecting robot is ready; you now have " num_clay_bots " of them."
*>      end-if
*>      if obsidian_bots_building > 0
*>        add obsidian_bots_building to num_obsidian_bots
*>        move 0 to obsidian_bots_building
*>        display "The new obsidian-collecting robot is ready; you now have " num_obsidian_bots " of them."
*>      end-if
*>      if geode_bots_building > 0
*>        add geode_bots_building to num_geode_bots
*>        move 0 to geode_bots_building
*>        display "The new geode-cracking robot is ready; you now have " num_geode_bots " of them."
*>      end-if
*>
*>    *>   move 0 to skip_obsidian_bots
*>    *>   move 0 to skip_clay_bots
*>    *>   move 0 to skip_ore_bots
*>    *>   if ore_num + (2 * num_ore_bots) >= geode_bot_ore(bp_idx) and obsidian_num + (2 * num_obsidian_bots) >= geode_bot_obsidian(bp_idx)
*>    *>     move 1 to skip_obsidian_bots
*>    *>     move 1 to skip_clay_bots
*>    *>     move 1 to skip_ore_bots
*>    *>   else
*>    *>     if ore_num + (2 * num_ore_bots) >= obsidian_bot_ore(bp_idx) and clay_num + (2 * num_clay_bots) >= obsidian_bot_clay(bp_idx)
*>    *>       move 1 to skip_clay_bots
*>    *>       move 1 to skip_ore_bots
*>    *>     else
*>    *>       if ore_num + (2 * num_ore_bots) >= clay_bot_ore(bp_idx)
*>    *>         move 1 to skip_ore_bots
*>    *>       end-if
*>    *>     end-if
*>    *>   end-if
*>    end-perform
*>    display "BP " bp_idx ": GEODES" geode_num
