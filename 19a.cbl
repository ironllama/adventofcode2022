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
      02 high_state usage is index.
      02 state_exists pic 9.
      02 states_head usage is index.
      02 states_cnt pic 9(8) comp.
      02 states occurs 999999 times indexed by states_idx.
        03 minute pic 9(2) comp value 1.
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

    77 print_idx usage is index.

    77 total_found pic s9(8) comp.

procedure division.
  *> call 'lib-readdata' using function module-id ".dat" rf_all_lines
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

  add 1 to states_cnt
  move 1 to minute(states_cnt)
  move 1 to num_ore_bots(states_cnt)
  move 1 to states_head

  move states_head to print_idx
  perform print_state

  move 1 to bp_idx
  perform process_state until states_head > states_cnt or minute(states_head) = 25

  display "FINAL: " geode_num(high_state)

  goback.


print_state.
  display "MIN[" minute(print_idx) "]: ORE " ore_num(print_idx) " CLY: " clay_num(print_idx) " OBS: " obsidian_num(print_idx) " GEO: " geode_num(print_idx) " === BOTS: ORE: " num_ore_bots(print_idx) " CLY: " num_clay_bots(print_idx) " OBS: " num_obsidian_bots(print_idx) " GEO: " num_geode_bots(print_idx)
  .

process_state.
  move states_head to print_idx
  display "START " no advancing
  perform print_state
  *> Mineral collection.
  add num_ore_bots(states_head) to ore_num(states_head)
  if num_clay_bots(states_head) > 0
    add num_clay_bots(states_head) to clay_num(states_head)
  end-if
  if num_obsidian_bots(states_head) > 0
    add num_obsidian_bots(states_head) to obsidian_num(states_head)
  end-if
  if num_geode_bots(states_head) > 0
    add num_geode_bots(states_head) to geode_num(states_head)
  end-if

  *> Building new bots and adding new decision branches.
  perform add_branch
  *> move states_cnt to print_idx
  *> display "ADDED NON: " no advancing
  *> perform print_state

  if obsidian_num(states_head) >= geode_bot_obsidian(bp_idx) and ore_num(states_head) >= geode_bot_ore(bp_idx)
    perform until obsidian_num(states_head) < geode_bot_obsidian(bp_idx) or ore_num(states_head) < geode_bot_ore(bp_idx)
    *>   perform add_branch
      subtract geode_bot_obsidian(bp_idx) from obsidian_num(states_head)
      subtract geode_bot_ore(bp_idx) from ore_num(states_head)
      add 1 to num_geode_bots(states_head)
      perform add_branch

      *> move states_cnt to print_idx
      *> display "ADDED GEO: " no advancing
      *> perform print_state
    end-perform
  end-if
  if clay_num(states_head) >= obsidian_bot_clay(bp_idx) and ore_num(states_head) >= obsidian_bot_ore(bp_idx)
    perform until clay_num(states_head) < obsidian_bot_clay(bp_idx) or ore_num(states_head) < obsidian_bot_ore(bp_idx)
      *> Even though we have enough, we'll skip?
    *>   perform add_branch
      subtract obsidian_bot_clay(bp_idx) from clay_num(states_head)
      subtract obsidian_bot_ore(bp_idx) from ore_num(states_head)
      add 1 to num_obsidian_bots(states_head)
      perform add_branch

      *> move states_cnt to print_idx
      *> display "ADDED OBS: " no advancing
      *> perform print_state
    end-perform
  end-if
  if ore_num(states_head) >= clay_bot_ore(bp_idx)
    perform until ore_num(states_head) < clay_bot_ore(bp_idx)
      *> Even though we have enough, we'll skip?
    *>   perform add_branch
      subtract clay_bot_ore(bp_idx) from ore_num(states_head)
      add 1 to num_clay_bots(states_head)
      perform add_branch

      *> move states_cnt to print_idx
      *> display "ADDED CLY: " no advancing
      *> perform print_state
    end-perform
  end-if
  if ore_num(states_head) >= ore_bot_ore(bp_idx)
    perform until ore_num(states_head) < ore_bot_ore(bp_idx)
      *> Even though we have enough, we'll add a skip scenario for each possible new collector.
    *>   perform add_branch
      *> Add branch where something is done!
      subtract ore_bot_ore(bp_idx) from ore_num(states_head)
      add 1 to num_ore_bots(states_head)
      perform add_branch

      *> move states_cnt to print_idx
      *> display "ADDED ORE: " no advancing
      *> perform print_state
    end-perform
  end-if

  if geode_num(states_head) > geode_num(high_state)
    move states_head to high_state
  end-if

  *> move states_head to print_idx
  *> display "END " no advancing
  *> perform print_state

  add 1 to states_head
  .

add_branch.
  move 0 to state_exists
  perform varying states_idx from 1 by 1 until states_idx > states_cnt or state_exists = 1
    if states_idx <> states_head
      if minute(states_idx) = minute(states_head) + 1
          and minerals(states_idx) = minerals(states_head)
          and robots(states_idx) = robots(states_head)
          *> display "DUPE!"
        move 1 to state_exists
      end-if
    end-if
  end-perform

  if state_exists = 0
    add 1 to states_cnt
    move states(states_head) to states(states_cnt)
    *> move minute(states_head) minute
    *> move ore_num
    *> move clay_num
    *> move obsidian_num
    *> move geode_num
    *> move num_ore_bots
    *> move num_clay_bots
    *> move num_obsidian_bots
    *> move num_geode_bots
    add 1 to minute(states_cnt)
  end-if
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
