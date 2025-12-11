/* Complete Web Interface for Dungeon Escape
   JC4002 Knowledge Representation Assessment - Bonus Feature

   Usage:
   ?- consult('dungeon_escape.pl').
   ?- consult('game_web.pl').
   ?- start_web(8080).

   Then open: http://localhost:8080/
*/

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).

/* HTTP Handlers */
:- http_handler(root(.), game_page, []).
:- http_handler(root(cmd), handle_cmd, []).
:- http_handler(root('player.png'), serve_player_image, []).

/* ============================================================================
   TEXT TRANSLATIONS (English Only)
   ============================================================================ */

/* Get text for a key */
t(Key, Text) :-
    text(Key, Text), !.
t(Key, Key).  % Fallback to key itself

/* Room names */
text(room_entrance, '🚪Entrance').
text(room_exit, '🏁Exit').
text(room_guard, '⚔️Guard').
text(room_treasure, '💎Treasure').
text(room_armory, '🛡️Armory').
text(room_library, '📚Library').
text(room_prison, '⛓️Prison').
text(room_secret, '🕳️Secret').
text(room_storage, '📦Storage').
text(room_unknown, '❓???').

/* Item names */
text(item_torch, '🔦Torch').
text(item_sword, '⚔️Sword').
text(item_shield, '🛡️Shield').
text(item_key, '🔑Key').
text(item_gold, '💰Treasure').
text(item_potion, '🧪Potion').
text(item_book, '📖Book').
text(item_rope, '🪢Rope').
text(item_gem, '💎Gem').
text(item_gem_ruby, '🔴Ruby').
text(item_gem_emerald, '🟢Emerald').
text(item_gem_sapphire, '🔵Sapphire').
text(item_golden_goblet, '🏆Goblet').
text(item_silver_necklace, '📿Necklace').
text(item_crown, '👑Crown').
text(item_ancient_coin, '🪙Coin').
text(item_lantern, '🏮Lantern').
text(item_dagger, '🗡️Dagger').
text(item_battle_axe, '🪓Axe').
text(item_armor, '🦺Armor').
text(item_magic_ring, '💍Ring').
text(item_map_scroll, '🗺️Map').
text(item_food_ration, '🍖Food').
text(item_bomb, '💣Bomb').
text(item_invisibility_cloak, '🧥Cloak').
text(item_speed_boots, '👢Boots').
text(item_lucky_charm, '🍀Charm').
text(item_magic_staff, '🪄Staff').
text(item_lockpick, '🔧Lockpick').
text(item_unknown, '📦Item').

/* Enemy names */
text(enemy_guard, '👮Guard').
text(enemy_thief, '🦹Thief').

/* UI Labels */
text(title, '🏰 Dungeon Escape').
text(location, '📍Location: ').
text(health, '❤️Health: ').
text(turn, '⏱️Turns Left: ').
text(inventory, '🎒 Inventory').
text(map, '🗺️ Map').
text(movement, '🧭 Movement').
text(items_here, '📍 Items Here').
text(use_item, '✨ Use Item').
text(drop_item, '📤 Drop Item').
text(combat, '⚔️ Combat').
text(quick_actions, '🎮 Quick Actions').
text(custom_cmd, '⌨️ Custom Command').

/* Buttons */
text(btn_north, '⬆️N').
text(btn_south, '⬇️S').
text(btn_east, '➡️E').
text(btn_west, '⬅️W').
text(btn_look, '👁️Look').
text(btn_inventory, '🎒Inventory').
text(btn_status, '📊Status').
text(btn_help, '❓Help').
text(btn_attack, 'Attack!').
text(btn_execute, 'Execute').
text(btn_play_again, '🔄 Play Again').
text(btn_try_again, '🔄 Try Again').

/* Messages */
text(empty_here, 'Nothing here').
text(empty_use, 'Nothing to use').
text(empty_drop, 'Nothing to drop').
text(empty_inv, 'Empty').
text(danger, '⚠️ DANGER!').
text(enemies_here, 'Enemies here: ').
text(warning, '⚠️ WARNING!').
text(trap_dark, 'You sense danger in the darkness... Get a torch!').
text(trap_visible, '🔦 Your torch reveals a TRAP here!').
text(treasure_alert, '🚨 ALERT!').
text(treasure_stolen_msg, 'The thief has stolen the treasure! Chase and catch the thief before they escape!').
text(victory, '🎉 VICTORY!').
text(victory_msg, 'You escaped the dungeon with the treasure!').
text(defeat, '💀 GAME OVER').
text(defeat_msg, 'You have been defeated...').
text(cmd_placeholder, 'Enter command...').

/* Start web server */
start_web(Port) :-
    http_server(http_dispatch, [port(Port)]),
    format('~n╔════════════════════════════════════════╗~n'),
    format('║     DUNGEON ESCAPE WEB SERVER          ║~n'),
    format('╠════════════════════════════════════════╣~n'),
    format('║  Open browser: http://localhost:~w/   ║~n', [Port]),
    format('╚════════════════════════════════════════╝~n~n'),
    /* Keep server running */
    thread_get_message(_).

/* Stop server */
stop_web :-
    stop_web(8080).

stop_web(Port) :-
    http_stop_server(Port, []).

/* Serve player image - pixel art knight using base64 embedded SVG */
serve_player_image(_Request) :-
    format('Content-type: image/svg+xml~n~n'),
    format('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 32 32" width="48" height="48">'),
    format('<rect x="12" y="2" width="8" height="8" fill="#8B4513"/>'),  /* Hair */
    format('<rect x="10" y="6" width="12" height="10" fill="#FFE4C4"/>'), /* Face */
    format('<rect x="12" y="8" width="2" height="2" fill="#000"/>'),      /* Left eye */
    format('<rect x="18" y="8" width="2" height="2" fill="#000"/>'),      /* Right eye */
    format('<rect x="8" y="16" width="16" height="10" fill="#4169E1"/>'), /* Body armor */
    format('<rect x="4" y="16" width="4" height="8" fill="#4169E1"/>'),   /* Left arm */
    format('<rect x="24" y="16" width="4" height="8" fill="#4169E1"/>'),  /* Right arm */
    format('<rect x="0" y="18" width="6" height="2" fill="#C0C0C0"/>'),   /* Sword */
    format('<rect x="10" y="26" width="5" height="6" fill="#8B4513"/>'),  /* Left leg */
    format('<rect x="17" y="26" width="5" height="6" fill="#8B4513"/>'),  /* Right leg */
    format('<rect x="6" y="4" width="4" height="12" fill="#DC143C"/>'),   /* Cape */
    format('</svg>').

/* Main game page */
game_page(_Request) :-
    reply_html_page(
        title('Dungeon Escape - Text Adventure'),
        [\game_html]
    ).

/* Handle command */
handle_cmd(Request) :-
    http_parameters(Request, [c(CmdStr, [default("")])]),
    atom_string(Cmd, CmdStr),
    /* Check if this is a difficulty command - return to start page */
    (is_difficulty_cmd(CmdStr) ->
        (with_output_to(string(_), execute_cmd(Cmd)),
         reply_html_page(
             title('Dungeon Escape - Text Adventure'),
             [\game_html]))
    ;
        /* Normal command - show game page */
        (with_output_to(string(Output), execute_cmd(Cmd)),
         get_state(State),
         get_room_items(RoomItems),
         reply_html_page(
             title('Dungeon Escape - Text Adventure'),
             [\game_html_with_output(Output, State, RoomItems)]))
    ).

/* Check if command is a difficulty setting */
is_difficulty_cmd(CmdStr) :-
    sub_string(CmdStr, 0, 10, _, "difficulty").

/* Execute command */
execute_cmd('') :- !.
execute_cmd(start) :- !, init_game, look.
execute_cmd(restart) :- !, init_game, look.
execute_cmd(n) :- !, n.
execute_cmd(s) :- !, s.
execute_cmd(e) :- !, e.
execute_cmd(w) :- !, w.
execute_cmd(look) :- !, look.
execute_cmd(i) :- !, inventory.
execute_cmd(inventory) :- !, inventory.
execute_cmd(status) :- !, status.
execute_cmd(instructions) :- !, instructions.
execute_cmd(help) :- !, instructions.
execute_cmd(attack) :- !, attack.
execute_cmd(fight) :- !, attack.
execute_cmd(kill) :- !, attack.
execute_cmd(shop) :- !, shop.
execute_cmd(gold) :- !, gold.
execute_cmd(map) :- !, map.

/* Handle buy commands */
execute_cmd(Cmd) :-
    atom(Cmd),
    atom_string(Cmd, CmdStr),
    sub_string(CmdStr, 0, 4, _, "buy("),
    sub_string(CmdStr, 4, _, _, ItemStrRaw),
    % Remove trailing ) and optional .
    (sub_string(ItemStrRaw, _, _, 0, ").") ->
        sub_string(ItemStrRaw, 0, _, 2, ItemStr)
    ; sub_string(ItemStrRaw, _, _, 0, ")") ->
        sub_string(ItemStrRaw, 0, _, 1, ItemStr)
    ; ItemStr = ItemStrRaw
    ),
    atom_string(Item, ItemStr),
    !, buy(Item).

/* Handle sell commands */
execute_cmd(Cmd) :-
    atom(Cmd),
    atom_string(Cmd, CmdStr),
    sub_string(CmdStr, 0, 5, _, "sell("),
    sub_string(CmdStr, 5, _, _, ItemStrRaw),
    % Remove trailing ) and optional .
    (sub_string(ItemStrRaw, _, _, 0, ").") ->
        sub_string(ItemStrRaw, 0, _, 2, ItemStr)
    ; sub_string(ItemStrRaw, _, _, 0, ")") ->
        sub_string(ItemStrRaw, 0, _, 1, ItemStr)
    ; ItemStr = ItemStrRaw
    ),
    atom_string(Item, ItemStr),
    !, sell(Item).

/* Handle take commands */
execute_cmd(Cmd) :-
    atom(Cmd),
    atom_string(Cmd, CmdStr),
    sub_string(CmdStr, 0, 5, _, "take("),
    sub_string(CmdStr, 5, _, _, ItemStrRaw),
    % Remove trailing ) and optional .
    (sub_string(ItemStrRaw, _, _, 0, ").") ->
        sub_string(ItemStrRaw, 0, _, 2, ItemStr)
    ; sub_string(ItemStrRaw, _, _, 0, ")") ->
        sub_string(ItemStrRaw, 0, _, 1, ItemStr)
    ; ItemStr = ItemStrRaw
    ),
    atom_string(Item, ItemStr),
    !, take(Item).

/* Handle drop commands */
execute_cmd(Cmd) :-
    atom(Cmd),
    atom_string(Cmd, CmdStr),
    sub_string(CmdStr, 0, 5, _, "drop("),
    sub_string(CmdStr, 5, _, _, ItemStrRaw),
    % Remove trailing ) and optional .
    (sub_string(ItemStrRaw, _, _, 0, ").") ->
        sub_string(ItemStrRaw, 0, _, 2, ItemStr)
    ; sub_string(ItemStrRaw, _, _, 0, ")") ->
        sub_string(ItemStrRaw, 0, _, 1, ItemStr)
    ; ItemStr = ItemStrRaw
    ),
    atom_string(Item, ItemStr),
    !, drop(Item).

/* Handle use commands */
execute_cmd(Cmd) :-
    atom(Cmd),
    atom_string(Cmd, CmdStr),
    sub_string(CmdStr, 0, 4, _, "use("),
    sub_string(CmdStr, 4, _, _, ItemStrRaw),
    % Remove trailing ) and optional .
    (sub_string(ItemStrRaw, _, _, 0, ").") ->
        sub_string(ItemStrRaw, 0, _, 2, ItemStr)
    ; sub_string(ItemStrRaw, _, _, 0, ")") ->
        sub_string(ItemStrRaw, 0, _, 1, ItemStr)
    ; ItemStr = ItemStrRaw
    ),
    atom_string(Item, ItemStr),
    !, use(Item).

/* Handle difficulty commands */
execute_cmd(Cmd) :-
    atom(Cmd),
    atom_string(Cmd, CmdStr),
    sub_string(CmdStr, 0, 11, _, "difficulty("),
    sub_string(CmdStr, 11, _, _, LevelStrRaw),
    % Remove trailing ) and optional .
    (sub_string(LevelStrRaw, _, _, 0, ").") ->
        sub_string(LevelStrRaw, 0, _, 2, LevelStr)
    ; sub_string(LevelStrRaw, _, _, 0, ")") ->
        sub_string(LevelStrRaw, 0, _, 1, LevelStr)
    ; LevelStr = LevelStrRaw
    ),
    atom_string(Level, LevelStr),
    !, difficulty(Level).

/* Try to parse and call compound term - with safety checks */
execute_cmd(Cmd) :-
    atom(Cmd),
    catch(
        (term_to_atom(Term, Cmd), safe_game_command(Term), call(Term)),
        Error,
        (format(user_error, 'Command execution error: ~w for command: ~w~n', [Error, Cmd]), fail)
    ), !.

execute_cmd(Cmd) :-
    format('Unknown command: ~w~n', [Cmd]),
    write('Commands: n, s, e, w, look, take(item), drop(item), use(item)'), nl.

/* Safety check - only allow known game commands to prevent code injection */
safe_game_command(take(_)).
safe_game_command(drop(_)).
safe_game_command(use(_)).
safe_game_command(go(_)).
safe_game_command(attack(_)).
safe_game_command(difficulty(_)).
safe_game_command(n).
safe_game_command(s).
safe_game_command(e).
safe_game_command(w).
safe_game_command(look).
safe_game_command(inventory).
safe_game_command(status).
safe_game_command(instructions).
safe_game_command(attack).
safe_game_command(fight).
safe_game_command(kill).
safe_game_command(start).
safe_game_command(restart).
safe_game_command(i).
safe_game_command(help).
safe_game_command(xyzzy).
safe_game_command(plugh).
safe_game_command(pray).
safe_game_command(dance).
safe_game_command(sing).
safe_game_command(meditate).
safe_game_command(kick).
safe_game_command(scream).
safe_game_command(credits).
safe_game_command(shop).
safe_game_command(buy(_)).
safe_game_command(sell(_)).

/* Get current state */
get_state(state(Loc, Health, Turn, Items, GameOver, Won, Enemies, Exits, Gold)) :-
    (i_am_at(Loc) -> true ; Loc = nowhere),
    (player_health(Health) -> true ; Health = 0),
    (turn_count(Turn) -> true ; Turn = 0),
    findall(X, holding(X), Items),
    (game_over -> GameOver = yes ; GameOver = no),
    /* Use the game_won flag for accurate victory detection */
    (game_won -> Won = yes ; Won = no),
    /* Only include discovered enemies - can't attack what you don't know is there */
    findall(E, (adversary_at(E, Loc), enemy_is_discovered(E)), Enemies),
    findall(Dir, get_available_exit(Loc, Dir), Exits),
    (player_gold(Gold) -> true ; Gold = 0).

/* Check if enemy has been discovered by the player */
enemy_is_discovered(guard) :- user:guard_discovered, !.
enemy_is_discovered(thief) :- user:thief_discovered, !.
enemy_is_discovered(_) :- fail.  % Unknown enemies are not discovered

/* Get available exits - uses dynamic_path only (random map) */
get_available_exit(Loc, Dir) :-
    dynamic_path(Loc, Dir, _).

/* Get items in current room */
get_room_items(RoomItems) :-
    (i_am_at(Loc) -> findall(Item, at(Item, Loc), RoomItems) ; RoomItems = []).

/* HTML Generation - Start Page */
game_html -->
    {get_current_difficulty(CurrentDiff)},
    html([
        \html_style,
        div(class(container), [
            div(class(header), [
                h1('Dungeon Escape'),
                p(class(subtitle), 'A Text Adventure Game with AI Adversaries')
            ]),
            div(class(story), [
                p('You wake up in a dark dungeon. The air is damp and cold.'),
                p('Somewhere in these depths lies a golden treasure - your only hope of buying freedom.'),
                p('But you are not alone... A guard patrols these halls, and a cunning thief also seeks the treasure.')
            ]),
            /* Difficulty Selection */
            div(class(difficulty_section), [
                h3('Select Difficulty'),
                div(class(difficulty_buttons), [
                    \difficulty_button(easy, CurrentDiff),
                    \difficulty_button(normal, CurrentDiff),
                    \difficulty_button(hard, CurrentDiff)
                ]),
                div(class(difficulty_info), [
                    \difficulty_info_display
                ])
            ]),
            div(class(start), [
                form([action('/cmd'), method('get')], [
                    input([type(hidden), name(c), value(start)]),
                    button([type(submit), class('btn btn-large')], 'Start New Game')
                ])
            ]),
            \game_map_legend,
            div(class(footer), [
                p('JC4002 Knowledge Representation - Bonus Web Interface')
            ])
        ])
    ]).

/* Get current difficulty level */
get_current_difficulty(Level) :-
    user:get_difficulty(Level),  % Get from user module (dungeon_escape.pl)
    !.
get_current_difficulty(normal).

/* Difficulty button - highlighted if current */
difficulty_button(Level, CurrentDiff) -->
    {
        (Level = CurrentDiff -> BtnClass = 'diff-btn diff-btn-active' ; BtnClass = 'diff-btn'),
        atom_concat('difficulty(', Level, Tmp),
        atom_concat(Tmp, ')', Cmd)
    },
    html(form([action('/cmd'), method('get'), class(inline)], [
        input([type(hidden), name(c), value(Cmd)]),
        button([type(submit), class(BtnClass)], Level)
    ])).

/* Display difficulty effects */
difficulty_info_display -->
    {
        get_current_difficulty(Level),
        get_difficulty_stats(Level, Health, TrapDmg, GuardDmg, EnemyMoves, HitPen, KillPen, CounterMult)
    },
    html(table(class(diff_table), [
        tr([th('Current:'), td(class(highlight), Level)]),
        tr([th('Health:'), td(Health)]),
        tr([th('Trap Damage:'), td(TrapDmg)]),
        tr([th('Guard Damage:'), td(GuardDmg)]),
        tr([th('Enemy Moves:'), td(EnemyMoves)]),
        tr([th('Hit Penalty:'), td(HitPen)]),
        tr([th('Kill Penalty:'), td(KillPen)]),
        tr([th('Counter Mult:'), td(CounterMult)])
    ])).

/* Get stats for a difficulty level */
get_difficulty_stats(Level, Health, TrapDmg, GuardDmg, EnemyMoves, HitPen, KillPen, CounterMult) :-
    user:difficulty_modifier(Level, health_bonus, HBonus),  % Get from user module
    user:difficulty_modifier(Level, trap_damage, TrapMult),
    (user:difficulty_modifier(Level, guard_damage, GuardMult) -> true ; user:difficulty_modifier(Level, guard_speed, GuardMult)),
    !,
    Health is 100 + HBonus,
    TrapDmg is round(10 * TrapMult),
    GuardDmg is round(25 * GuardMult),
    get_difficulty_combat_stats(Level, EnemyMoves, HitPen, KillPen, CounterMult).

get_difficulty_stats(normal, 100, 10, 25, '1x/turn', '0%', '0%', 'x1.0').

/* Get combat stats based on difficulty level */
get_difficulty_combat_stats(easy, '50% skip', '+25%', '+25%', 'x0.5') :- !.
get_difficulty_combat_stats(hard, '3x/turn', '-25%', '-25%', 'x2.0') :- !.
get_difficulty_combat_stats(_, '1x/turn', '0%', '0%', 'x1.0').  % Normal

/* HTML Generation - Game Page */
game_html_with_output(Output, state(Loc, Health, Turn, Items, GameOver, Won, Enemies, Exits, Gold), RoomItems) -->
    {
        t(title, Title),
        t(location, LocLabel),
        t(health, HealthLabel),
        t(turn, TurnLabel),
        TurnsLeft is 100 - Turn
    },
    html([
        \html_style,
        div(class(container), [
            div(class(header), [
                h1(Title)
            ]),

            /* Status Bar with Player Avatar */
            div(class(status_bar), [
                div(class(player_avatar), [
                    img([src('/player.png'), alt('Player'), class(avatar_img)], []),
                    span(class(player_name), 'Hero')
                ]),
                div(class(stat), [span(class(label), LocLabel), span(class(value), \location_display(Loc))]),
                div(class(stat), [span(class(label), HealthLabel), \health_display(Health)]),
                div(class(stat), [span(class(label), '💰Gold: '), span(class(gold_value), Gold)]),
                div(class(stat), [span(class(label), TurnLabel), span(class(value), TurnsLeft)])
            ]),

            /* Main Game Area - Map on LEFT, Text on RIGHT */
            div(class(game_area), [
                /* Map Panel (LEFT) */
                div(class(map_panel), [
                    /* Full Map */
                    \mini_map(Loc)
                ]),

                /* Side Panel (RIGHT) - Output + Quick Actions */
                div(class(side_panel), [
                    /* Output Panel */
                    div(class(output_panel), [
                        div(class(output), [pre(Output)])
                    ]),

                    /* Quick Actions - Moved from bottom controls */
                    \quick_actions_panel,

                    /* Treasure Stolen Warning */
                    \treasure_stolen_warning,

                    /* Enemies Warning */
                    \enemies_warning(Enemies),

                    /* Trap Warning */
                    \trap_warning(Loc)
                ])
            ]),

            /* Game Over Check */
            \game_over_display(GameOver, Won),

            /* Controls - only show if game not over */
            \game_controls(GameOver, Exits, RoomItems, Items, Enemies),

            /* Command Input */
            \command_input(GameOver),

            /* Enemy movement animation data and script */
            \enemy_animation_script
        ])
    ]).

/* Health display with color */
health_display(Health) -->
    {
        get_max_health_value(MaxHealth),
        Percent is (Health * 100) / MaxHealth,
        (Percent >= 70 -> Color = '#7f7' ;
         Percent >= 40 -> Color = '#ff7' ;
         Color = '#f77'),
        format(atom(Style), 'color: ~w', [Color]),
        format(atom(HealthStr), '~w/~w', [Health, MaxHealth])
    },
    html(span([class(value), style(Style)], HealthStr)).

/* Get max health based on difficulty */
get_max_health_value(MaxHealth) :-
    user:get_difficulty(Level),  % Get from user module
    user:difficulty_modifier(Level, health_bonus, Bonus),
    !,
    MaxHealth is 100 + Bonus.

get_max_health_value(100).

/* Location display with icon */
location_display(Loc) -->
    {room_short_name(Loc, Display)},
    html(Display).

/* Quick Actions Panel - for side panel */
quick_actions_panel -->
    {
        t(quick_actions, QuickLabel),
        t(btn_inventory, InvBtn),
        t(btn_status, StatusBtn),
        t(btn_help, HelpBtn)
    },
    html(div(class(quick_actions_box), [
        h4(QuickLabel),
        div(class(quick_btn_group), [
            \action_form(i, InvBtn, quick_actbtn),
            \action_form(status, StatusBtn, quick_actbtn),
            \action_form(instructions, HelpBtn, quick_actbtn)
        ])
    ])).

/* Enemies warning */
enemies_warning([]) --> [].
enemies_warning(Enemies) -->
    {t(danger, DangerLabel), t(enemies_here, EnemiesLabel)},
    html(div(class(enemies_box), [
        h4(DangerLabel),
        p([EnemiesLabel, \enemy_list(Enemies)])
    ])).

enemy_list([]) --> [].
enemy_list([guard]) --> {t(enemy_guard, G)}, html(span(class(enemy), G)).
enemy_list([thief]) --> {t(enemy_thief, T)}, html(span(class(enemy), T)).
enemy_list([E]) --> html(span(class(enemy), E)).
enemy_list([E|Rest]) -->
    {enemy_display(E, Display)},
    html([span(class(enemy), Display), ', ']),
    enemy_list(Rest).

enemy_display(guard, Display) :- t(enemy_guard, Display).
enemy_display(thief, Display) :- t(enemy_thief, Display).
enemy_display(E, E).

/* Trap warning - uses dynamic trap_location from dungeon_escape */
trap_warning(Loc) -->
    {
        user:trap_location(Loc),
        \+ user:trap_triggered(Loc),
        t(warning, WarnLabel),
        t(trap_dark, TrapDark),
        t(trap_visible, TrapVisible)
    },
    !,
    html(div(class(trap_box), [
        h4(WarnLabel),
        ({\+ user:has_light} ->
            html(p(TrapDark))
        ;
            html(p(TrapVisible))
        )
    ])).
trap_warning(_) --> [].

/* Treasure stolen warning */
treasure_stolen_warning -->
    {user:treasure_stolen},
    !,
    {t(treasure_alert, AlertLabel), t(treasure_stolen_msg, StolenMsg)},
    html(div(class(treasure_stolen_box), [
        h4(AlertLabel),
        p(StolenMsg)
    ])).
treasure_stolen_warning --> [].

/* Game over display */
game_over_display(no, _) --> [].
game_over_display(yes, yes) -->
    {t(victory, VictoryLabel), t(victory_msg, VictoryMsg), t(btn_play_again, PlayAgain)},
    html(div(class(victory), [
        h2(VictoryLabel),
        p(VictoryMsg),
        form([action('/cmd'), method('get')], [
            input([type(hidden), name(c), value(restart)]),
            button([type(submit), class(btn)], PlayAgain)
        ])
    ])).
game_over_display(yes, no) -->
    {t(defeat, DefeatLabel), t(defeat_msg, DefeatMsg), t(btn_try_again, TryAgain)},
    html(div(class(defeat), [
        h2(DefeatLabel),
        p(DefeatMsg),
        form([action('/cmd'), method('get')], [
            input([type(hidden), name(c), value(restart)]),
            button([type(submit), class(btn)], TryAgain)
        ])
    ])).

/* Game controls */
game_controls(yes, _, _, _, _) --> [].
game_controls(no, Exits, RoomItems, HeldItems, Enemies) -->
    {
        t(movement, MovementLabel),
        t(btn_north, NorthBtn), t(btn_south, SouthBtn),
        t(btn_east, EastBtn), t(btn_west, WestBtn),
        t(btn_look, LookBtn),
        t(items_here, ItemsLabel),
        t(use_item, UseLabel),
        t(drop_item, DropLabel),
        t(combat, CombatLabel)
    },
    html(div(class(controls_area), [
        /* Movement Controls */
        div(class(movement_section), [
            h4(MovementLabel),
            div(class(dpad), [
                div(class(dpad_row), [
                    div([class(dpad_empty)], []),
                    \dir_button(n, NorthBtn, Exits),
                    div([class(dpad_empty)], [])
                ]),
                div(class(dpad_row), [
                    \dir_button(w, WestBtn, Exits),
                    \action_form(look, LookBtn, btn_center),
                    \dir_button(e, EastBtn, Exits)
                ]),
                div(class(dpad_row), [
                    div([class(dpad_empty)], []),
                    \dir_button(s, SouthBtn, Exits),
                    div([class(dpad_empty)], [])
                ])
            ])
        ]),

        /* Room Items */
        div(class(items_section), [
            h4(ItemsLabel),
            div(class(btn_group), \room_item_buttons(RoomItems))
        ]),

        /* Use Items */
        div(class(use_section), [
            h4(UseLabel),
            div(class(btn_group), \use_item_buttons(HeldItems))
        ]),

        /* Drop Items */
        div(class(drop_section), [
            h4(DropLabel),
            div(class(btn_group), \drop_item_buttons(HeldItems))
        ]),

        /* Combat */
        div(class(combat_section), [
            h4(CombatLabel),
            div(class(btn_group), [
                \attack_button(HeldItems, Enemies)
            ])
        ])
    ])).

/* Direction button - enabled only if exit exists */
dir_button(Dir, Label, Exits) -->
    {member(Dir, Exits) -> Disabled = '' ; Disabled = disabled},
    html(form([action('/cmd'), method('get'), class(inline)], [
        input([type(hidden), name(c), value(Dir)]),
        button([type(submit), class(dirbtn), Disabled], Label)
    ])).

/* Attack button - only enabled when have weapon and enemies present */
attack_button(HeldItems, Enemies) -->
    {
        t(btn_attack, AttackLabel),
        ((member(sword, HeldItems); member(dagger, HeldItems)), Enemies \= []) ->
            (BtnClass = attackbtn, Disabled = '')
        ;
            (BtnClass = attackbtn_disabled, Disabled = disabled)
    },
    html(form([action('/cmd'), method('get'), class(inline)], [
        input([type(hidden), name(c), value(attack)]),
        button([type(submit), class(BtnClass), Disabled], AttackLabel)
    ])).

/* Item icons mapping */
/* Get item display name with icon - bilingual */
item_display(torch, Display) :- t(item_torch, Display).
item_display(sword, Display) :- t(item_sword, Display).
item_display(shield, Display) :- t(item_shield, Display).
item_display(key, Display) :- t(item_key, Display).
item_display(gold_treasure, Display) :- t(item_gold, Display).
item_display(health_potion, Display) :- t(item_potion, Display).
item_display(ancient_book, Display) :- t(item_book, Display).
item_display(rope, Display) :- t(item_rope, Display).
item_display(magic_gem, Display) :- t(item_gem, Display).
/* New items with unique icons */
item_display(gem_ruby, Display) :- t(item_gem_ruby, Display).
item_display(gem_emerald, Display) :- t(item_gem_emerald, Display).
item_display(gem_sapphire, Display) :- t(item_gem_sapphire, Display).
item_display(golden_goblet, Display) :- t(item_golden_goblet, Display).
item_display(silver_necklace, Display) :- t(item_silver_necklace, Display).
item_display(crown, Display) :- t(item_crown, Display).
item_display(ancient_coin, Display) :- t(item_ancient_coin, Display).
item_display(lantern, Display) :- t(item_lantern, Display).
item_display(dagger, Display) :- t(item_dagger, Display).
item_display(battle_axe, Display) :- t(item_battle_axe, Display).
item_display(armor, Display) :- t(item_armor, Display).
item_display(magic_ring, Display) :- t(item_magic_ring, Display).
item_display(map_scroll, Display) :- t(item_map_scroll, Display).
item_display(food_ration, Display) :- t(item_food_ration, Display).
item_display(bomb, Display) :- t(item_bomb, Display).
item_display(invisibility_cloak, Display) :- t(item_invisibility_cloak, Display).
item_display(speed_boots, Display) :- t(item_speed_boots, Display).
item_display(lucky_charm, Display) :- t(item_lucky_charm, Display).
item_display(magic_staff, Display) :- t(item_magic_staff, Display).
item_display(lockpick, Display) :- t(item_lockpick, Display).
item_display(_, Display) :- t(item_unknown, Display).

/* Room item take buttons */
room_item_buttons([]) -->
    {t(empty_here, Msg)},
    html(span(class(empty), Msg)).
room_item_buttons([Item|Rest]) -->
    {atom_concat('take(', Item, Tmp), atom_concat(Tmp, ')', Cmd),
     item_display(Item, Display)},
    action_form(Cmd, Display, itembtn),
    room_item_buttons(Rest).

/* Use item buttons */
use_item_buttons([]) -->
    {t(empty_use, Msg)},
    html(span(class(empty), Msg)).
use_item_buttons([Item|Rest]) -->
    {atom_concat('use(', Item, Tmp), atom_concat(Tmp, ')', Cmd),
     item_display(Item, Display)},
    action_form(Cmd, Display, itembtn),
    use_item_buttons(Rest).

/* Drop item buttons */
drop_item_buttons([]) -->
    {t(empty_drop, Msg)},
    html(span(class(empty), Msg)).
drop_item_buttons([Item|Rest]) -->
    {atom_concat('drop(', Item, Tmp), atom_concat(Tmp, ')', Cmd),
     item_display(Item, Display)},
    action_form(Cmd, Display, itembtn_drop),
    drop_item_buttons(Rest).

/* Generic action form */
action_form(Cmd, Label, Class) -->
    html(form([action('/cmd'), method('get'), class(inline)], [
        input([type(hidden), name(c), value(Cmd)]),
        button([type(submit), class(Class)], Label)
    ])).

/* Command input */
command_input(yes) --> [].
command_input(no) -->
    {t(custom_cmd, CmdLabel), t(cmd_placeholder, Placeholder), t(btn_execute, ExecBtn)},
    html(div(class(cmd_section), [
        h4(CmdLabel),
        form([action('/cmd'), method('get'), class(cmd_form)], [
            input([type(text), name(c), placeholder(Placeholder), autocomplete(off), class(cmd_input)]),
            button([type(submit), class(btn)], ExecBtn)
        ])
    ])).

/* Enemy movement animation script */
enemy_animation_script -->
    {
        /* Get guard movement path */
        (user:enemy_move_path(guard, GuardPath) ->
            maplist(room_to_grid_pos_safe, GuardPath, GuardPositions)
        ; GuardPositions = []),
        /* Get thief movement path */
        (user:enemy_move_path(thief, ThiefPath) ->
            maplist(room_to_grid_pos_safe, ThiefPath, ThiefPositions)
        ; ThiefPositions = []),
        /* Format as JSON arrays */
        format_positions_json(GuardPositions, GuardJSON),
        format_positions_json(ThiefPositions, ThiefJSON)
    },
    html(script([], [
        'var guardPath = ', GuardJSON, ';\n',
        'var thiefPath = ', ThiefJSON, ';\n',
        'var ANIM_DELAY = 400;\n',
        'function animateEnemy(path, enemyClass, icon) {\n',
        '  if (path.length <= 1) return;\n',
        '  var cells = document.querySelectorAll(".map_cell");\n',
        '  var step = 0;\n',
        '  function showStep() {\n',
        '    if (step >= path.length) return;\n',
        '    var pos = path[step];\n',
        '    if (pos[0] < 0) { step++; showStep(); return; }\n',
        '    var idx = pos[0] * 8 + pos[1];\n',
        '    var cell = cells[idx];\n',
        '    if (cell && !cell.classList.contains("current")) {\n',
        '      if (step > 0) {\n',
        '        var prevPos = path[step-1];\n',
        '        var prevIdx = prevPos[0] * 8 + prevPos[1];\n',
        '        var prevCell = cells[prevIdx];\n',
        '        if (prevCell) {\n',
        '          prevCell.classList.add("enemy-trail");\n',
        '          prevCell.classList.remove(enemyClass);\n',
        '        }\n',
        '      }\n',
        '      cell.classList.add(enemyClass);\n',
        '      cell.innerHTML = \'<span class="enemy_icon">\' + icon + \'</span>\';\n',
        '    }\n',
        '    step++;\n',
        '    if (step < path.length) setTimeout(showStep, ANIM_DELAY);\n',
        '  }\n',
        '  setTimeout(showStep, 100);\n',
        '}\n',
        'document.addEventListener("DOMContentLoaded", function() {\n',
        '  animateEnemy(guardPath, "enemy-guard-anim", "👮");\n',
        '  animateEnemy(thiefPath, "enemy-thief-anim", "🦹");\n',
        '});\n'
    ])).

/* Helper: convert room to grid position */
room_to_grid_pos_safe(Room, [Row, Col]) :-
    user:grid_pos(Row, Col, Room), !.
room_to_grid_pos_safe(_, [-1, -1]).

/* Format list of positions as JSON array */
format_positions_json([], '[]').
format_positions_json(Positions, JSON) :-
    Positions \= [],
    format_pos_list(Positions, Inner),
    atom_concat('[', Inner, Tmp),
    atom_concat(Tmp, ']', JSON).

format_pos_list([], '').
format_pos_list([[R,C]], Str) :- format(atom(Str), '[~w,~w]', [R, C]).
format_pos_list([[R,C]|Rest], Str) :-
    Rest \= [],
    format_pos_list(Rest, RestStr),
    format(atom(Str), '[~w,~w],~w', [R, C, RestStr]).

/* Mini map with fog of war - Full 12x12 grid map display */
mini_map(Loc) -->
    {
        t(map, MapLabel),
        /* Get visited rooms count */
        findall(R, user:room_visited(R), Visited),
        length(Visited, VisitedCount)
    },
    html(div(class(minimap), [
        h4(MapLabel),
        div(class(map_grid_container), [
            \full_grid_map(Loc)
        ]),
        div(class(map_stats), [
            span(['Explored: ', VisitedCount, '/36 rooms'])
        ]),
        div(class(map_legend_mini), [
            span(class(legend_player), ['🧑 You']),
            span(class(legend_exit), ['🏁 Exit']),
            span(class(legend_entrance), ['🚪 Start']),
            span(class(legend_guard), ['👮 Guard']),
            span(class(legend_thief), ['🦹 Thief']),
            span(class(legend_wall), ['█ Wall'])
        ])
    ])).

/* Full 12x12 grid map - dynamically reads from grid_pos */
full_grid_map(CurrentLoc) -->
    {
        /* Build 8 rows dynamically from grid_pos - 8x8 grid */
        findall(R, between(0, 7, R), Rows),
        maplist(get_grid_row, Rows, GridRows)
    },
    html(div(class(map_grid), [
        \dynamic_map_rows(GridRows, CurrentLoc)
    ])).

/* Get a row of rooms from grid_pos - 8 columns */
get_grid_row(Row, RoomList) :-
    findall(Room, (between(0, 7, Col), get_grid_cell(Row, Col, Room)), RoomList).

/* Get room at grid position */
get_grid_cell(Row, Col, Room) :-
    user:grid_pos(Row, Col, Room), !.
get_grid_cell(_, _, wall).  % Default to wall if no grid_pos

/* Generate rows dynamically */
dynamic_map_rows([], _) --> [].
dynamic_map_rows([Row|Rest], CurrentLoc) -->
    map_row(Row, CurrentLoc),
    dynamic_map_rows(Rest, CurrentLoc).

/* Generate a row of map cells */
map_row([], _) --> [].
map_row([Room|Rest], CurrentLoc) -->
    map_cell(Room, CurrentLoc),
    map_row(Rest, CurrentLoc).

/* Map cell - wall */
map_cell(wall, _) -->
    !,
    html(div(class('map_cell wall'), [])).

/* Map cell - empty/none */
map_cell(none, _) -->
    !,
    html(div(class('map_cell empty'), [])).

/* Map cell - current location WITH guard (combat situation!) */
map_cell(Room, Room) -->
    {user:adversary_at(guard, Room), user:guard_discovered},
    !,
    html(div(class('map_cell current combat guard'), [
        img([src('/player.png'), alt('You'), class(player_icon)], []),
        span(class(enemy_icon), '👮'),
        span(class(combat_indicator), '⚔️')
    ])).

/* Map cell - current location WITH thief (encounter situation!) */
map_cell(Room, Room) -->
    {user:adversary_at(thief, Room), user:thief_discovered},
    !,
    html(div(class('map_cell current combat thief'), [
        img([src('/player.png'), alt('You'), class(player_icon)], []),
        span(class(enemy_icon), '🦹'),
        span(class(combat_indicator), '⚔️')
    ])).

/* Map cell - current location with player icon only */
map_cell(Room, Room) -->
    {room_short_icon(Room, Icon)},
    !,
    html(div(class('map_cell current'), [
        img([src('/player.png'), alt('You'), class(player_icon)], []),
        span(class(room_label), Icon)
    ])).

/* Map cell - room with guard (only visible after entering guard's room) */
map_cell(Room, _) -->
    {user:adversary_at(guard, Room), user:guard_discovered},
    !,
    html(div(class('map_cell enemy guard'), [
        span(class(enemy_icon), '👮')
    ])).

/* Map cell - room where guard was discovered (but guard moved away) */
map_cell(Room, _) -->
    {user:guard_discovered_at(Room), \+ user:adversary_at(guard, Room), user:room_visited(Room)},
    !,
    html(div(class('map_cell visited guard_spotted'), [
        span(class(spotted_icon), '⚠️')
    ])).

/* Map cell - room with thief (only visible after entering thief's room) */
map_cell(Room, _) -->
    {user:adversary_at(thief, Room), user:thief_discovered},
    !,
    html(div(class('map_cell enemy thief'), [
        span(class(enemy_icon), '🦹')
    ])).

/* Map cell - exit_hall (always visible with special highlight, but not when player is there) */
map_cell(exit_hall, CurrentLoc) -->
    {CurrentLoc \= exit_hall},
    !,
    html(div(class('map_cell exit_highlight'), [
        span(class(exit_icon), '🏁')
    ])).

/* Map cell - entrance (always visible, but not when player is there) */
map_cell(entrance, CurrentLoc) -->
    {CurrentLoc \= entrance},
    !,
    html(div(class('map_cell entrance_highlight'), [
        span(class(entrance_icon), '🚪')
    ])).

/* Map cell - visited room with special icon */
map_cell(Room, _) -->
    {user:room_visited(Room), room_short_icon(Room, Icon)},
    !,
    html(div(class('map_cell visited'), [
        span(class(room_icon), Icon),
        \room_items_indicator(Room)
    ])).

/* Map cell - visited room without special icon, check for items */
map_cell(Room, _) -->
    {user:room_visited(Room)},
    !,
    html(div(class('map_cell visited_plain'), [
        \room_content_icon(Room)
    ])).

/* Map cell - unvisited room (fog) */
map_cell(_, _) -->
    html(div(class('map_cell fog'), [
        span('?')
    ])).

/* Room items indicator - show if items present */
room_items_indicator(Room) -->
    {findall(Item, user:at(Item, Room), Items), Items \= []},
    !,
    html(span(class(item_indicator), '📦')).
room_items_indicator(_) --> [].

/* Room content icon - for rooms without special icons */
room_content_icon(Room) -->
    {findall(Item, user:at(Item, Room), Items), Items \= [], get_primary_item_icon(Items, Icon)},
    !,
    html(span(class(content_icon), Icon)).
room_content_icon(_) -->
    html(span(class(visited_dot), '·')).

/* Get icon for most important item in room */
get_primary_item_icon(Items, '💰') :- member(gold_treasure, Items), !.
get_primary_item_icon(Items, '🔑') :- member(key, Items), !.
get_primary_item_icon(Items, '⚔️') :- member(sword, Items), !.
get_primary_item_icon(Items, '⚔️') :- member(battle_axe, Items), !.
get_primary_item_icon(Items, '🛡️') :- member(shield, Items), !.
get_primary_item_icon(Items, '🛡️') :- member(armor, Items), !.
get_primary_item_icon(Items, '🔦') :- member(torch, Items), !.
get_primary_item_icon(Items, '🔦') :- member(lantern, Items), !.
get_primary_item_icon(Items, '🧪') :- member(health_potion, Items), !.
get_primary_item_icon(Items, '📖') :- member(ancient_book, Items), !.
get_primary_item_icon(Items, '🪢') :- member(rope, Items), !.
get_primary_item_icon(Items, '💎') :- member(gem_ruby, Items), !.
get_primary_item_icon(Items, '💎') :- member(gem_emerald, Items), !.
get_primary_item_icon(Items, '💎') :- member(gem_sapphire, Items), !.
get_primary_item_icon(Items, '👑') :- member(crown, Items), !.
get_primary_item_icon(Items, '🍷') :- member(golden_goblet, Items), !.
get_primary_item_icon(Items, '💊') :- member(food_ration, Items), !.
get_primary_item_icon(Items, '💣') :- member(bomb, Items), !.
get_primary_item_icon(Items, '🗺️') :- member(map_scroll, Items), !.
get_primary_item_icon(Items, '🔪') :- member(dagger, Items), !.
get_primary_item_icon(Items, '🪄') :- member(magic_staff, Items), !.
get_primary_item_icon(Items, '💍') :- member(magic_ring, Items), !.
get_primary_item_icon(Items, '👻') :- member(invisibility_cloak, Items), !.
get_primary_item_icon(Items, '👟') :- member(speed_boots, Items), !.
get_primary_item_icon(Items, '🔓') :- member(lockpick, Items), !.
get_primary_item_icon(Items, '🪙') :- member(ancient_coin, Items), !.
get_primary_item_icon(_, '📦').  /* Default: generic item box */

/* Short icons for important rooms only */
room_short_icon(entrance, '🚪').
room_short_icon(exit_hall, '🏁').
room_short_icon(guard_room, '⚔️').
room_short_icon(treasure_room, '💎').
room_short_icon(marketplace, '🛒').
room_short_icon(vault, '🔒').
room_short_icon(treasury, '💰').
room_short_icon(lava_room, '🔥').
room_short_icon(ice_chamber, '❄️').
room_short_icon(underground_lake, '🌊').
room_short_icon(portal_room, '🌀').
room_short_icon(throne_room, '👑').
room_short_icon(chapel, '⛪').
room_short_icon(alchemy_lab, '⚗️').
room_short_icon(crypt, '💀').
/* Default: simple dot for normal rooms */
room_short_icon(_, '·').

/* Get room at slot position */
get_slot_room(Slot, Room) :-
    user:room_mapping(Slot, Room), !.
get_slot_room(_, unknown).

/* Get short display name for room with icons - bilingual */
room_short_name(entrance, Name) :- t(room_entrance, Name).
room_short_name(exit_hall, Name) :- t(room_exit, Name).
room_short_name(guard_room, Name) :- t(room_guard, Name).
room_short_name(treasure_room, Name) :- t(room_treasure, Name).
room_short_name(armory, Name) :- t(room_armory, Name).
room_short_name(library, Name) :- t(room_library, Name).
room_short_name(prison_cell, Name) :- t(room_prison, Name).
room_short_name(secret_passage, Name) :- t(room_secret, Name).
room_short_name(storage, Name) :- t(room_storage, Name).
/* New rooms */
room_short_name(dark_corridor, '🌑Corridor').
room_short_name(throne_room, '👑Throne').
room_short_name(kitchen, '🍳Kitchen').
room_short_name(well_room, '💧Well').
room_short_name(alchemy_lab, '⚗️Alchemy').
room_short_name(crypt, '💀Crypt').
room_short_name(barracks, '🏠Barracks').
room_short_name(chapel, '⛪Chapel').
room_short_name(dungeon_pit, '🕳️Pit').
room_short_name(torture_chamber, '⛓️Torture').
room_short_name(wine_cellar, '🍷Cellar').
room_short_name(servants_quarters, '🛏️Servants').
room_short_name(great_hall, '🏛️Hall').
room_short_name(garden, '🌿Garden').
room_short_name(observatory, '🔭Observatory').
room_short_name(vault, '🔒Vault').
room_short_name(smithy, '🔨Smithy').
room_short_name(stable, '🐴Stable').
room_short_name(watchtower, '🗼Tower').
room_short_name(marketplace, '🛒Market').
room_short_name(bathhouse, '🛁Bath').
room_short_name(gallery, '🖼️Gallery').
room_short_name(_, Name) :- t(room_unknown, Name).

/* Room fog - show ??? if not visited, show name if visited, mark if current */
room_fog(Current, Room, DisplayName, ' <--') :-
    Current = Room, !,
    room_short_name(Room, DisplayName).

room_fog(_, Room, DisplayName, '') :-
    user:room_visited(Room), !,
    room_short_name(Room, DisplayName).

room_fog(_, _, '???', '').

/* Connection fog - only show if both rooms explored or player is in one */
conn_fog(Room1, Room2, visible) :-
    (user:room_visited(Room1) ; user:i_am_at(Room1)),
    (user:room_visited(Room2) ; user:i_am_at(Room2)),
    !.

conn_fog(_, _, hidden).

/* Map legend for start page */
game_map_legend -->
    html(div(class(map_legend), [
        h3('Dungeon Map (8x8 Grid)'),
        pre(class(map_pre_large),
'█ █ █ █ █ █ █ █
█ ? ? ? ? ? ? █
█ ? 🚪? ? ? ? █
█ ? ? ? ? ? ? █
█ ? ? ? ? 💎? █
█ ? ? ? ? ? ? █
█ ? ? ? ? ? 🏁█
█ █ █ █ █ █ █ █

36 rooms are randomly generated each game!
Entrance: Northwest area, Exit: Southeast corner
'),
        div(class(legend_items), [
            p([b('Goal: '), 'Find the treasure and escape through the exit!']),
            p([b('Key: '), 'Needed to unlock the exit door']),
            p([b('Torch: '), 'Reveals traps in dark rooms - avoid damage!']),
            p([b('Sword/Shield: '), 'Defend against the guard and thief']),
            p([b('Enemies: '), '👮 Guard patrols, 🦹 Thief steals treasure'])
        ])
    ])).

/* CSS Styles */
html_style -->
    html(style('
        * { box-sizing: border-box; margin: 0; padding: 0; }
        body {
            font-family: "Consolas", "Courier New", monospace;
            background: linear-gradient(135deg, #0d0d1a 0%, #1a1a2e 50%, #0d0d1a 100%);
            color: #e0e0e0;
            min-height: 100vh;
            padding: 15px;
        }
        .container {
            max-width: 1000px;
            margin: 0 auto;
        }
        .header {
            text-align: center;
            padding: 15px 0;
            border-bottom: 2px solid #333;
            margin-bottom: 15px;
            position: relative;
        }
        h1 {
            color: #ffd700;
            text-shadow: 0 0 10px rgba(255,215,0,0.5);
            font-size: 2em;
            letter-spacing: 3px;
        }
        .subtitle { color: #888; margin-top: 5px; }
        h3, h4 { color: #ffd700; margin-bottom: 8px; }

        /* Story intro */
        .story {
            background: rgba(0,0,0,0.3);
            border: 1px solid #333;
            border-radius: 8px;
            padding: 20px;
            margin: 20px 0;
            text-align: center;
        }
        .story p { margin: 10px 0; line-height: 1.6; }

        /* Difficulty selection */
        .difficulty_section {
            background: rgba(0,0,0,0.3);
            border: 1px solid #333;
            border-radius: 8px;
            padding: 20px;
            margin: 20px 0;
            text-align: center;
        }
        .difficulty_buttons {
            display: flex;
            justify-content: center;
            gap: 15px;
            margin: 15px 0;
        }
        .diff-btn {
            padding: 12px 30px;
            font-size: 16px;
            font-family: inherit;
            background: linear-gradient(180deg, #2a2a4a, #1a1a2a);
            color: #aaa;
            border: 2px solid #444;
            border-radius: 8px;
            cursor: pointer;
            transition: all 0.3s;
            text-transform: uppercase;
            letter-spacing: 2px;
        }
        .diff-btn:hover {
            background: linear-gradient(180deg, #3a3a5a, #2a2a3a);
            border-color: #ffd700;
            color: #ffd700;
        }
        .diff-btn-active {
            background: linear-gradient(180deg, #4a3a1a, #3a2a0a);
            border-color: #ffd700;
            color: #ffd700;
            box-shadow: 0 0 15px rgba(255,215,0,0.3);
        }
        .difficulty_info {
            margin-top: 15px;
        }
        .diff_table {
            margin: 0 auto;
            border-collapse: collapse;
            text-align: left;
        }
        .diff_table th {
            color: #888;
            padding: 5px 15px 5px 0;
            font-weight: normal;
        }
        .diff_table td {
            color: #7fbfff;
            padding: 5px 0;
        }
        .diff_table td.highlight {
            color: #ffd700;
            font-weight: bold;
            text-transform: uppercase;
        }

        /* Status bar */
        .status_bar {
            display: flex;
            justify-content: space-around;
            align-items: center;
            flex-wrap: wrap;
            background: linear-gradient(180deg, #1a1a2e, #0d0d1a);
            border: 2px solid #333;
            border-radius: 8px;
            padding: 12px;
            margin-bottom: 15px;
        }
        .stat { margin: 5px 15px; }
        .label { color: #888; }
        .value { color: #7fbfff; font-weight: bold; }

        /* Player avatar */
        .player_avatar {
            display: flex;
            flex-direction: column;
            align-items: center;
            margin-right: 15px;
        }
        .avatar_img {
            width: 48px;
            height: 48px;
            border: 2px solid #ffd700;
            border-radius: 8px;
            background: #1a1a2e;
            image-rendering: pixelated;
        }
        .player_name {
            color: #ffd700;
            font-size: 12px;
            margin-top: 4px;
            font-weight: bold;
        }

        /* Gold display */
        .gold_value {
            color: #ffd700;
            font-weight: bold;
            text-shadow: 0 0 5px rgba(255,215,0,0.5);
        }

        /* Game area - Map LEFT, Text RIGHT */
        .game_area {
            display: grid;
            grid-template-columns: 420px 1fr;
            gap: 15px;
            margin-bottom: 15px;
        }
        @media (max-width: 900px) {
            .game_area { grid-template-columns: 1fr; }
        }

        /* Map panel (LEFT) */
        .map_panel {
            background: #0a0a12;
            border: 2px solid #333;
            border-radius: 8px;
            padding: 10px;
        }

        /* Output panel */
        .output_panel {
            background: #0a0a12;
            border: 2px solid #333;
            border-radius: 8px;
            padding: 15px;
            flex: 1;
        }
        .output {
            min-height: 280px;
            max-height: 400px;
            overflow-y: auto;
        }
        .output pre {
            white-space: pre-wrap;
            word-wrap: break-word;
            color: #7fff7f;
            line-height: 1.5;
            font-size: 14px;
        }

        /* Side panel (RIGHT) - stretch to match map height */
        .side_panel {
            display: flex;
            flex-direction: column;
            gap: 10px;
            min-height: 420px;
        }

        /* Mini map */
        .minimap {
            background: #0a0a12;
            border: 2px solid #333;
            border-radius: 8px;
            padding: 10px;
        }
        .map_pre {
            font-size: 10px;
            line-height: 1.2;
            color: #7fbfff;
        }
        .map_pre_large {
            font-size: 12px;
            line-height: 1.3;
            color: #7fbfff;
        }
        /* Grid map container - 12x12, no scrolling */
        .map_grid_container {
            overflow: visible;
            padding: 5px;
        }
        .map_grid {
            display: grid;
            grid-template-columns: repeat(8, 40px);
            grid-template-rows: repeat(8, 40px);
            gap: 2px;
            justify-content: center;
        }
        .map_cell {
            width: 40px;
            height: 40px;
            display: flex;
            flex-direction: column;
            align-items: center;
            justify-content: center;
            border-radius: 4px;
            font-size: 16px;
            position: relative;
        }
        .map_cell.current {
            background: linear-gradient(180deg, #4a6b2a, #3a5a1a);
            border: 2px solid #ffd700;
            box-shadow: 0 0 8px rgba(255,215,0,0.6);
            z-index: 10;
        }
        .map_cell.current .player_icon {
            width: 26px;
            height: 26px;
            image-rendering: pixelated;
        }
        .map_cell.current .room_label {
            display: none;
        }
        /* Combat cell - player and enemy in same location */
        .map_cell.current.combat {
            background: linear-gradient(180deg, #6a2a2a, #4a1a1a);
            border: 3px solid #ff4444;
            box-shadow: 0 0 15px rgba(255,68,68,0.8);
            animation: combat-pulse 0.8s infinite;
            display: grid;
            grid-template-columns: 1fr 1fr;
            grid-template-rows: auto auto;
            gap: 1px;
            padding: 2px;
        }
        .map_cell.current.combat .player_icon {
            width: 16px;
            height: 16px;
            grid-column: 1;
            grid-row: 1 / 3;
            align-self: center;
            justify-self: center;
        }
        .map_cell.current.combat .enemy_icon {
            font-size: 16px;
            grid-column: 2;
            grid-row: 1 / 3;
            align-self: center;
            justify-self: center;
        }
        .map_cell.current.combat .combat_indicator {
            position: absolute;
            top: -2px;
            right: -2px;
            font-size: 10px;
            background: #ff0000;
            border-radius: 50%;
            width: 14px;
            height: 14px;
            display: flex;
            align-items: center;
            justify-content: center;
            animation: combat-spin 1s linear infinite;
        }
        @keyframes combat-pulse {
            0%, 100% {
                border-color: #ff4444;
                box-shadow: 0 0 15px rgba(255,68,68,0.8);
            }
            50% {
                border-color: #ff8888;
                box-shadow: 0 0 25px rgba(255,68,68,1);
            }
        }
        @keyframes combat-spin {
            from { transform: rotate(0deg); }
            to { transform: rotate(360deg); }
        }
        .map_cell.visited {
            background: #2a2a3e;
            border: 1px solid #444;
            position: relative;
        }
        .map_cell.visited .room_icon {
            font-size: 14px;
        }
        .map_cell.visited .item_indicator {
            position: absolute;
            top: 0;
            right: 0;
            font-size: 8px;
            background: rgba(255,215,0,0.2);
            border-radius: 2px;
            padding: 1px;
        }
        .map_cell.visited_plain {
            background: #2a2a3e;
            border: 1px solid #444;
        }
        .map_cell.visited_plain .content_icon {
            font-size: 16px;
        }
        .map_cell.visited_plain .visited_dot {
            font-size: 20px;
            color: #666;
        }
        .map_cell.fog {
            background: #1a1a1a;
            border: 1px solid #333;
            color: #555;
            font-size: 10px;
        }
        .map_cell.empty {
            background: transparent;
            border: none;
        }
        .map_cell.wall {
            background: #2a2a2a;
            border: 1px solid #1a1a1a;
        }
        .map_cell.enemy {
            border: 1px solid #444;
        }
        .map_cell.enemy.guard {
            background: linear-gradient(180deg, #4a2a2a, #3a1a1a);
            border: 2px solid #ff4444;
            animation: enemy-pulse 1s infinite;
        }
        .map_cell.guard_spotted {
            background: linear-gradient(180deg, #3a2a2a, #2a1a1a);
            border: 2px dashed #ff6644;
        }
        .spotted_icon {
            font-size: 1.2em;
        }
        .map_cell.enemy.thief {
            background: linear-gradient(180deg, #2a2a4a, #1a1a3a);
            border: 2px solid #aa44ff;
            animation: enemy-pulse 1.5s infinite;
        }
        .enemy_icon {
            font-size: 14px;
        }
        @keyframes enemy-pulse {
            0%, 100% { opacity: 1; }
            50% { opacity: 0.6; }
        }
        .map_cell.exit_highlight {
            background: linear-gradient(180deg, #2a4a2a, #1a3a1a);
            border: 2px solid #44ff44;
            animation: exit-pulse 2s infinite;
        }
        .map_cell.entrance_highlight {
            background: linear-gradient(180deg, #3a3a2a, #2a2a1a);
            border: 2px solid #ffff44;
        }
        .map_cell.danger_room {
            background: linear-gradient(180deg, #4a2a2a, #3a1a1a);
            border: 2px solid #ff4444;
            animation: danger-pulse 1.5s infinite;
        }
        .danger_icon {
            font-size: 14px;
        }
        @keyframes danger-pulse {
            0%, 100% { border-color: #ff4444; opacity: 1; }
            50% { border-color: #ff8888; opacity: 0.7; }
        }
        /* Enemy movement animation styles */
        .enemy-guard-anim {
            background: linear-gradient(180deg, #4a2a2a, #3a1a1a) !important;
            border: 2px solid #ff4444 !important;
            animation: enemy-move-pulse 0.3s ease-in-out !important;
        }
        .enemy-thief-anim {
            background: linear-gradient(180deg, #3a2a4a, #2a1a3a) !important;
            border: 2px solid #aa44ff !important;
            animation: enemy-move-pulse 0.3s ease-in-out !important;
        }
        .enemy-trail {
            background: rgba(255, 100, 100, 0.2) !important;
            border: 1px dashed #ff6666 !important;
        }
        @keyframes enemy-move-pulse {
            0% { transform: scale(1.3); opacity: 0.5; }
            100% { transform: scale(1); opacity: 1; }
        }
        .exit_icon, .entrance_icon {
            font-size: 14px;
        }
        @keyframes exit-pulse {
            0%, 100% { border-color: #44ff44; }
            50% { border-color: #88ff88; }
        }
        .map_stats {
            color: #888;
            font-size: 11px;
            margin-top: 8px;
            text-align: center;
        }
        .map_legend_mini {
            display: flex;
            justify-content: center;
            gap: 10px;
            margin-top: 8px;
            font-size: 9px;
            color: #888;
        }
        .legend_player { color: #ffd700; }
        .legend_guard { color: #ff4444; }
        .legend_thief { color: #aa44ff; }
        .legend_wall { color: #555; }

        /* Quick Actions box - in side panel */
        .quick_actions_box {
            background: #0a0a12;
            border: 2px solid #333;
            border-radius: 8px;
            padding: 10px;
        }
        .quick_btn_group {
            display: flex;
            flex-direction: column;
            gap: 8px;
        }
        .quick_actbtn {
            width: 100%;
            padding: 10px;
            font-size: 14px;
            font-family: inherit;
            background: linear-gradient(180deg, #2a2a4a, #1a1a2a);
            color: #aaf;
            border: 2px solid #44a;
            border-radius: 6px;
            cursor: pointer;
            transition: all 0.2s;
            text-align: left;
        }
        .quick_actbtn:hover {
            background: linear-gradient(180deg, #3a3a5a, #2a2a3a);
            border-color: #ffd700;
            color: #ffd700;
            transform: translateX(3px);
        }
        .empty { color: #666; font-style: italic; }

        /* Enemies box */
        .enemies_box {
            background: #2a0a0a;
            border: 2px solid #f44;
            border-radius: 8px;
            padding: 10px;
            animation: pulse 1s infinite;
        }
        .enemies_box h4 { color: #f44; }
        .enemy { color: #f88; font-weight: bold; }
        @keyframes pulse {
            0%, 100% { opacity: 1; }
            50% { opacity: 0.7; }
        }

        /* Trap warning */
        .trap_box {
            background: #2a2a0a;
            border: 2px solid #fa0;
            border-radius: 8px;
            padding: 10px;
            margin-top: 10px;
            animation: pulse 1.5s infinite;
        }
        .trap_box h4 { color: #fa0; }
        .trap_box p { color: #fc8; }

        /* Treasure stolen warning */
        .treasure_stolen_box {
            background: #3a0a2a;
            border: 2px solid #f0f;
            border-radius: 8px;
            padding: 10px;
            margin-bottom: 10px;
            animation: pulse 0.8s infinite;
        }
        .treasure_stolen_box h4 { color: #f0f; }
        .treasure_stolen_box p { color: #faf; }

        /* Victory/Defeat */
        .victory {
            text-align: center;
            background: linear-gradient(180deg, #0a2a0a, #0a1a0a);
            border: 3px solid #4f4;
            border-radius: 10px;
            padding: 25px;
            margin: 15px 0;
        }
        .victory h2 { color: #4f4; font-size: 2em; }
        .victory p { color: #7f7; margin: 10px 0; }

        .defeat {
            text-align: center;
            background: linear-gradient(180deg, #2a0a0a, #1a0a0a);
            border: 3px solid #f44;
            border-radius: 10px;
            padding: 25px;
            margin: 15px 0;
        }
        .defeat h2 { color: #f44; font-size: 2em; }
        .defeat p { color: #f88; margin: 10px 0; }

        /* Controls area */
        .controls_area {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
            gap: 15px;
            margin-bottom: 15px;
        }
        .controls_area > div {
            background: #0a0a12;
            border: 2px solid #333;
            border-radius: 8px;
            padding: 12px;
        }

        /* D-pad */
        .dpad {
            display: flex;
            flex-direction: column;
            align-items: center;
        }
        .dpad_row {
            display: flex;
            gap: 3px;
        }
        .dpad_empty { width: 50px; height: 45px; visibility: hidden; }

        /* Buttons */
        .btn {
            padding: 10px 20px;
            font-size: 14px;
            font-family: inherit;
            background: linear-gradient(180deg, #3a3a6a, #2a2a4a);
            color: #ffd700;
            border: 2px solid #ffd700;
            border-radius: 6px;
            cursor: pointer;
            transition: all 0.2s;
        }
        .btn:hover {
            background: linear-gradient(180deg, #4a4a7a, #3a3a5a);
            transform: scale(1.02);
        }
        .btn-large {
            padding: 15px 40px;
            font-size: 18px;
        }
        .btn_center {
            width: 50px;
            height: 45px;
            font-size: 11px;
            background: #2a4a2a;
            color: #7f7;
            border: 2px solid #4a4;
            border-radius: 6px;
            cursor: pointer;
        }
        .btn_center:hover { background: #3a5a3a; }

        .dirbtn {
            width: 50px;
            height: 45px;
            font-size: 16px;
            font-weight: bold;
            background: linear-gradient(180deg, #3a3a5a, #2a2a3a);
            color: #fff;
            border: 2px solid #555;
            border-radius: 6px;
            cursor: pointer;
            transition: all 0.2s;
        }
        .dirbtn:hover:not([disabled]) {
            background: linear-gradient(180deg, #4a4a6a, #3a3a4a);
            border-color: #ffd700;
            color: #ffd700;
        }
        .dirbtn[disabled] {
            opacity: 0.3;
            cursor: not-allowed;
        }

        .btn_group {
            display: flex;
            flex-wrap: wrap;
            gap: 5px;
        }

        .actbtn, .itembtn, .itembtn_drop {
            padding: 6px 10px;
            font-size: 11px;
            font-family: inherit;
            border-radius: 4px;
            cursor: pointer;
            transition: all 0.2s;
        }
        .actbtn {
            background: #2a2a4a;
            color: #aaf;
            border: 1px solid #44a;
        }
        .actbtn:hover { background: #3a3a5a; color: #fff; }

        .itembtn {
            background: #2a4a2a;
            color: #afa;
            border: 1px solid #4a4;
        }
        .itembtn:hover { background: #3a5a3a; color: #fff; }

        .itembtn_drop {
            background: #4a2a2a;
            color: #faa;
            border: 1px solid #a44;
        }
        .itembtn_drop:hover { background: #5a3a3a; color: #fff; }

        .attackbtn {
            padding: 10px 20px;
            font-size: 14px;
            font-weight: bold;
            background: linear-gradient(180deg, #8a2a2a, #5a1a1a);
            color: #fff;
            border: 2px solid #f44;
            border-radius: 6px;
            cursor: pointer;
            text-transform: uppercase;
        }
        .attackbtn:hover { background: linear-gradient(180deg, #aa3a3a, #7a2a2a); }
        .attackbtn_disabled {
            padding: 10px 20px;
            font-size: 14px;
            font-weight: bold;
            background: #333;
            color: #666;
            border: 2px solid #444;
            border-radius: 6px;
            cursor: not-allowed;
            text-transform: uppercase;
        }

        .inline { display: inline-block; margin: 2px; }

        /* Command input */
        .cmd_section {
            background: #0a0a12;
            border: 2px solid #333;
            border-radius: 8px;
            padding: 12px;
            margin-bottom: 15px;
        }
        .cmd_form {
            display: flex;
            gap: 10px;
        }
        .cmd_input {
            flex: 1;
            padding: 10px;
            font-size: 14px;
            font-family: inherit;
            background: #050508;
            color: #fff;
            border: 2px solid #444;
            border-radius: 5px;
        }
        .cmd_input:focus {
            outline: none;
            border-color: #ffd700;
        }

        /* Start button area */
        .start { text-align: center; margin: 30px 0; }

        /* Map legend */
        .map_legend {
            background: #0a0a12;
            border: 2px solid #333;
            border-radius: 8px;
            padding: 20px;
            margin: 20px 0;
            text-align: center;
        }
        .legend_items {
            text-align: left;
            max-width: 400px;
            margin: 15px auto 0;
        }
        .legend_items p { margin: 8px 0; }

        /* Footer */
        .footer {
            text-align: center;
            color: #555;
            margin-top: 20px;
            padding-top: 15px;
            border-top: 1px solid #333;
        }

        /* Scrollbar */
        ::-webkit-scrollbar { width: 8px; }
        ::-webkit-scrollbar-track { background: #111; }
        ::-webkit-scrollbar-thumb { background: #444; border-radius: 4px; }
        ::-webkit-scrollbar-thumb:hover { background: #555; }
    ')).
