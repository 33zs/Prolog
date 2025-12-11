/* Dungeon Escape - A Text Adventure Game
   JC4002 Knowledge Representation Assessment

   Game Description:
   You are a brave adventurer trapped in a dark dungeon. Your goal is to find
   the golden treasure and escape through the exit. But beware! A guard patrols
   the dungeon trying to capture you, and a thief lurks in the shadows trying
   to steal the treasure before you can get it.

   Features:
   - Three difficulty levels (easy, normal, hard)
   - Dynamic adversary behavior using PDDL planning
   - Random events and item combinations
*/

:- use_module(pyperplan_runner).

/* Discontiguous declarations for predicates defined in multiple places */
:- discontiguous valuable/1.
:- discontiguous use/1.

/* ============================================================================
   DYNAMIC PREDICATES - Facts that change during gameplay
   ============================================================================ */

:- dynamic i_am_at/1.           % Player's current location
:- dynamic at/2.                % Object locations: at(Object, Location)
:- dynamic holding/1.           % Player's inventory
:- dynamic adversary_at/2.      % Adversary locations: adversary_at(Adversary, Location)
:- dynamic adversary_type/2.    % Adversary behavior type: adversary_type(Adversary, Type)
:- dynamic adversary_plan/2.    % Current plan for adversary: adversary_plan(Adversary, Plan)
:- dynamic guard_discovered/0.  % Flag: player has entered guard's room
:- dynamic guard_discovered_at/1.  % Room where guard was discovered
:- dynamic thief_discovered/0.  % Flag: player has entered thief's room
:- dynamic player_health/1.     % Player's health points
:- dynamic turn_count/1.        % Number of turns elapsed
:- dynamic game_over/0.         % Flag indicating game has ended
:- dynamic game_won/0.          % Flag indicating player won the game
:- dynamic treasure_stolen/0.   % Flag indicating treasure was stolen
:- dynamic door_locked/1.       % Locked doors
:- dynamic guard_defeated/0.    % Flag indicating guard was killed
:- dynamic thief_defeated/0.    % Flag indicating thief was defeated
:- dynamic has_light/0.         % Whether player has light source active
:- dynamic room_visited/1.      % Rooms the player has visited
:- dynamic trap_triggered/1.    % Traps that have been triggered
:- dynamic item_collected/1.    % Items that have been collected (for collector achievement)
:- dynamic enemy_move_path/2.   % Records enemy movement path: enemy_move_path(Adversary, PathList)
:- dynamic room_mapping/2.      % Maps slot positions to room names
:- dynamic player_gold/1.       % Player's gold coins
:- dynamic shop_item/2.         % Items available in shop: shop_item(Item, Price)
:- dynamic dynamic_path/3.      % Dynamic paths generated from random room mapping
:- dynamic grid_pos/3.          % Grid position: grid_pos(Row, Col, Room)
:- dynamic game_difficulty/1.   % Current difficulty level (easy/normal/hard)

/* ============================================================================
   STATIC KNOWLEDGE BASE - Facts that don't change
   ============================================================================ */

/* Room definitions - Optimized for 8x8 grid (6x6 interior = 36 rooms) */

/* Essential gameplay rooms (30 rooms with events) */
room(entrance).          % Starting point
room(exit_hall).         % Exit point
room(treasure_room).     % Main treasure
room(guard_room).        % Guard patrol area

/* Combat & equipment rooms */
room(armory).            % Weapons: sword, shield
room(library).           % Map scroll
room(prison_cell).       % Lockpick
room(secret_passage).    % (empty)
room(storage).           % Rope

/* Trading & special rooms */
room(marketplace).       % Shop for items

/* Valuable treasure rooms */
room(vault).             % Ruby gem
room(crypt).             % Emerald gem
room(observatory).       % Sapphire gem
room(throne_room).       % Golden goblet
room(gallery).           % Silver necklace
room(great_hall).        % Crown

/* Utility & item rooms */
room(dark_corridor).     % Lantern
room(kitchen).           % Dagger
room(smithy).            % (empty)
room(barracks).          % Armor
room(alchemy_lab).       % Magic ring
room(wine_cellar).       % Food ration
room(watchtower).        % Bomb
room(stable).            % (empty)
room(chapel).            % (empty)

/* Danger & coin rooms */
room(torture_chamber).   % Ancient coin
room(dungeon_pit).       % Ancient coin
room(well_room).         % Ancient coin
room(servants_quarters). % Ancient coin
room(bathhouse).         % Ancient coin

/* Decorative passage rooms (6 rooms for navigation) */
room(hallway_n).         % North corridor
room(hallway_s).         % South corridor
room(hallway_e).         % East corridor
room(hallway_w).         % West corridor
room(crossing).          % Central crossing point
room(courtyard).         % Open breathing space

/*
   8x8 Grid Layout:
   - Outer layer: Walls (border)
   - Interior: 6x6 = 36 rooms (all 36 defined above)
   - 30 rooms with events (83%) + 6 passage rooms (17%)
   - Every room has purpose!
*/

/* Adjacent rooms for PDDL planning - uses dynamic paths only (random map) */
adjacent(A, B) :- dynamic_path(A, _, B).

/* Objects in the game */
object(torch).
object(key).
object(sword).
object(shield).
object(gold_treasure).
object(health_potion).
object(ancient_book).
object(rope).

/* New items */
object(lantern).          % Better light source
object(armor).            % Better defense
object(lockpick).         % Alternative to key
object(map_scroll).       % Reveals map
object(food_ration).      % Heals small amount
object(bomb).             % Damages enemies
object(dagger).           % Weak weapon
object(magic_ring).       % Converts to gold
object(gem_ruby).         % Valuable
object(gem_emerald).      % Valuable
object(gem_sapphire).     % Valuable
object(golden_goblet).    % Valuable
object(silver_necklace).  % Valuable
object(ancient_coin).     % Currency/valuable
object(crown).            % Very valuable

/* Object properties */
weapon(sword).
weapon(dagger).
light_source(torch).
light_source(lantern).
valuable(gold_treasure).
valuable(gem_ruby).
valuable(gem_emerald).
valuable(gem_sapphire).
valuable(golden_goblet).
valuable(silver_necklace).
valuable(crown).
healing(health_potion).
healing(food_ration).
readable(ancient_book).
readable(map_scroll).

/* Item values for gold system */
item_value(gem_ruby, 100).
item_value(gem_emerald, 80).
item_value(gem_sapphire, 90).
item_value(golden_goblet, 150).
item_value(silver_necklace, 60).
item_value(ancient_coin, 25).
item_value(crown, 200).
item_value(gold_treasure, 500).

/* Trap locations - dynamic for random placement */
:- dynamic trap_location/1.

/* Damage values */
trap_damage(10).
guard_damage(25).
thief_escape_turns(3).

/* Healing values */
potion_heal(30).

/* ============================================================================
   DIFFICULTY SYSTEM
   ============================================================================ */

/* Difficulty modifiers */
difficulty_modifier(easy, guard_speed, 0.5).
difficulty_modifier(easy, thief_speed, 0.5).
difficulty_modifier(easy, trap_damage, 0.5).
difficulty_modifier(easy, health_bonus, 50).
difficulty_modifier(easy, guard_damage, 0.8).

difficulty_modifier(normal, guard_speed, 1.0).
difficulty_modifier(normal, thief_speed, 1.0).
difficulty_modifier(normal, trap_damage, 1.0).
difficulty_modifier(normal, health_bonus, 0).
difficulty_modifier(normal, guard_damage, 1.0).

difficulty_modifier(hard, guard_speed, 3.0).
difficulty_modifier(hard, thief_speed, 3.0).
difficulty_modifier(hard, trap_damage, 2.0).
difficulty_modifier(hard, health_bonus, -20).
difficulty_modifier(hard, guard_damage, 1.5).
difficulty_modifier(hard, hit_penalty, -25).
difficulty_modifier(hard, kill_penalty, -25).
difficulty_modifier(hard, counter_mult, 2.0).

/* Get current difficulty */
get_difficulty(Level) :-
    game_difficulty(Level),
    !.
get_difficulty(normal).  % Default to normal

/* Set difficulty */
set_difficulty(Level) :-
    member(Level, [easy, normal, hard]),
    retractall(game_difficulty(_)),
    assert(game_difficulty(Level)),
    format('Difficulty set to: ~w~n', [Level]).

set_difficulty(_) :-
    write('Invalid difficulty. Choose: easy, normal, or hard.'), nl.

/* Get initial health based on difficulty */
get_initial_health(Health) :-
    get_difficulty(Level),
    difficulty_modifier(Level, health_bonus, Bonus),
    Health is 100 + Bonus.

/* Get difficulty-adjusted trap damage */
get_trap_damage(Damage) :-
    get_difficulty(Level),
    difficulty_modifier(Level, trap_damage, Mult),
    trap_damage(BaseDmg),
    Damage is round(BaseDmg * Mult).

/* Get difficulty-adjusted guard damage */
get_guard_damage(Damage) :-
    get_difficulty(Level),
    guard_damage(BaseDmg),
    difficulty_modifier(Level, guard_damage, Mult),
    Damage is round(BaseDmg * Mult).

/* ============================================================================
   GAME INITIALIZATION
   ============================================================================ */

init_game :-
    retractall(i_am_at(_)),
    retractall(at(_, _)),
    retractall(holding(_)),
    retractall(adversary_at(_, _)),
    retractall(adversary_type(_, _)),
    retractall(adversary_plan(_, _)),
    retractall(player_health(_)),
    retractall(turn_count(_)),
    retractall(game_over),
    retractall(game_won),
    retractall(treasure_stolen),
    retractall(door_locked(_)),
    retractall(has_light),
    retractall(room_visited(_)),
    retractall(trap_triggered(_)),
    retractall(trap_location(_)),
    retractall(guard_defeated),
    retractall(thief_defeated),
    retractall(guard_discovered),
    retractall(guard_discovered_at(_)),
    retractall(thief_discovered),
    retractall(item_collected(_)),
    retractall(bonus_score(_)),
    retractall(event_cooldown(_)),
    retractall(room_mapping(_, _)),
    retractall(player_gold(_)),
    retractall(shop_item(_, _)),

    /* Initialize random room mapping */
    init_random_room_mapping,

    /* Fixed room names for key locations */
    StartRoom = entrance,
    LibraryRoom = library,
    TreasureRoom = treasure_room,
    ExitRoom = exit_hall,

    /* Initialize player with difficulty-adjusted health */
    assert(i_am_at(StartRoom)),
    get_initial_health(InitHealth),
    assert(player_health(InitHealth)),
    assert(turn_count(0)),
    assert(room_visited(StartRoom)),
    assert(player_gold(0)),  /* Start with 0 gold */

    /* Randomly place all items */
    random_item_placement,

    /* Place new items in new rooms */
    place_extended_items,

    /* Randomly place the key in one of several locations */
    random_key_placement,

    /* Place treasure */
    assert(at(gold_treasure, TreasureRoom)),

    /* Lock the exit door */
    assert(door_locked(ExitRoom)),

    /* Randomly place traps */
    random_trap_placement,

    /* Initialize shop in marketplace */
    init_shop,

    /* Initialize adversaries with random positions */
    init_adversaries.

/* Place items in the extended dungeon areas */
place_extended_items :-
    /* Valuable gems scattered around */
    assert(at(gem_ruby, vault)),
    assert(at(gem_emerald, crypt)),
    assert(at(gem_sapphire, observatory)),
    assert(at(golden_goblet, throne_room)),
    assert(at(silver_necklace, gallery)),
    assert(at(crown, great_hall)),

    /* Useful items */
    assert(at(lantern, dark_corridor)),
    assert(at(armor, barracks)),
    assert(at(map_scroll, library)),
    assert(at(food_ration, wine_cellar)),
    assert(at(bomb, watchtower)),
    assert(at(lockpick, prison_cell)),
    assert(at(dagger, kitchen)),
    assert(at(magic_ring, alchemy_lab)),

    /* Scatter some gold coins */
    assert(at(ancient_coin, torture_chamber)),
    assert(at(ancient_coin, dungeon_pit)),
    assert(at(ancient_coin, well_room)),
    assert(at(ancient_coin, servants_quarters)),
    assert(at(ancient_coin, bathhouse)).

/* Initialize shop items */
init_shop :-
    assert(shop_item(health_potion, 30)),
    assert(shop_item(torch, 15)),
    assert(shop_item(rope, 20)),
    assert(shop_item(food_ration, 10)),
    assert(shop_item(bomb, 50)),
    assert(shop_item(map_scroll, 40)).

/* ============================================================================
   RANDOM ROOM MAPPING SYSTEM
   ============================================================================ */

/*
   Map slots (fixed structure positions) to room names (randomly assigned)

   3x3 Grid Layout:
   [slot_nw] - [slot_n]  - [slot_ne]
       |          |           |
   [slot_w]  - [slot_c]  - [slot_e]
       |          |           |
   [slot_sw] - [slot_s]  - [slot_se]

   Fixed: slot_c = entrance (start), slot_se = exit_hall (end), slot_e = treasure_room
*/

/* ============================================================================
   FULL 12x12 RANDOM MAP GENERATION SYSTEM
   ============================================================================
   Grid layout (12x12):
   - Row 0 and Row 11: all walls (borders)
   - Col 0 and Col 11: all walls (borders)
   - Interior 10x10 (rows 1-10, cols 1-10): 100 playable cells

   Grid positions stored as grid_pos(Row, Col, Room) where Room can be 'wall' or room name
   Paths generated dynamically based on adjacent grid positions
*/

/* Initialize full 8x8 random map (6x6 interior = 36 rooms) */
init_random_room_mapping :-
    retractall(room_mapping(_, _)),
    retractall(dynamic_path(_, _, _)),
    retractall(grid_pos(_, _, _)),

    /* Get all 36 room names - exactly matches our interior grid size */
    findall(R, room(R), AllRooms),

    /* Remove entrance and exit_hall from shuffle pool (we place them specially) */
    delete(AllRooms, entrance, Rooms1),
    delete(Rooms1, exit_hall, RoomsToShuffle),

    /* Randomly shuffle remaining rooms */
    random_permutation(RoomsToShuffle, ShuffledRooms),

    /* Place border walls (row 0, 7 and col 0, 7) - 8x8 grid */
    place_border_walls,

    /* Place entrance at position (2, 2) */
    assert(grid_pos(2, 2, entrance)),

    /* Place exit_hall at position (5, 5) - close enough for faster gameplay */
    assert(grid_pos(5, 5, exit_hall)),

    /* Place remaining rooms in interior grid (excluding entrance/exit positions) */
    get_interior_positions(2, 2, 5, 5, Positions),
    place_rooms_in_grid(Positions, ShuffledRooms),

    /* Generate paths based on grid adjacency */
    generate_grid_paths,

    /* Create room_mapping for compatibility with map display */
    create_room_mappings_from_grid.

/* Place walls on all 4 borders - 8x8 grid */
place_border_walls :-
    /* Row 0 - top wall */
    forall(between(0, 7, C), assert(grid_pos(0, C, wall))),
    /* Row 7 - bottom wall */
    forall(between(0, 7, C), assert(grid_pos(7, C, wall))),
    /* Col 0 - left wall (rows 1-6) */
    forall(between(1, 6, R), assert(grid_pos(R, 0, wall))),
    /* Col 7 - right wall (rows 1-6) */
    forall(between(1, 6, R), assert(grid_pos(R, 7, wall))).

/* Get all interior positions except the specified excluded ones - 8x8 grid */
get_interior_positions(ExcR1, ExcC1, ExcR2, ExcC2, Positions) :-
    findall((R,C), (
        between(1, 6, R),
        between(1, 6, C),
        \+ (R = ExcR1, C = ExcC1),
        \+ (R = ExcR2, C = ExcC2)
    ), Positions).

/* Place rooms into grid positions */
place_rooms_in_grid([], _).
place_rooms_in_grid(_, []).
place_rooms_in_grid([(R,C)|RestPos], [Room|RestRooms]) :-
    assert(grid_pos(R, C, Room)),
    place_rooms_in_grid(RestPos, RestRooms).

/* Generate paths based on grid adjacency */
generate_grid_paths :-
    forall(
        (grid_pos(R, C, Room), Room \= wall),
        generate_paths_for_cell(R, C, Room)
    ).

/* Generate paths for a single cell to its neighbors */
generate_paths_for_cell(R, C, Room) :-
    /* North neighbor */
    RN is R - 1,
    (grid_pos(RN, C, NorthRoom), NorthRoom \= wall ->
        assert(dynamic_path(Room, n, NorthRoom))
    ; true),
    /* South neighbor */
    RS is R + 1,
    (grid_pos(RS, C, SouthRoom), SouthRoom \= wall ->
        assert(dynamic_path(Room, s, SouthRoom))
    ; true),
    /* West neighbor */
    CW is C - 1,
    (grid_pos(R, CW, WestRoom), WestRoom \= wall ->
        assert(dynamic_path(Room, w, WestRoom))
    ; true),
    /* East neighbor */
    CE is C + 1,
    (grid_pos(R, CE, EastRoom), EastRoom \= wall ->
        assert(dynamic_path(Room, e, EastRoom))
    ; true).

/* Create room_mapping facts for map display compatibility */
create_room_mappings_from_grid :-
    forall(
        grid_pos(R, C, Room),
        (atom_concat('pos_', R, T1),
         atom_concat(T1, '_', T2),
         atom_concat(T2, C, Slot),
         assert(room_mapping(Slot, Room)))
    ).

/* Get the room name for a slot */
get_mapped_room(Slot, Room) :-
    room_mapping(Slot, Room), !.
get_mapped_room(_, unknown).

/* Get the slot for a room name */
get_room_slot(Room, Slot) :-
    room_mapping(Slot, Room), !.
get_room_slot(_, unknown).

/* Get display position for a room (for map display) */
get_room_position(Room, Position) :-
    room_mapping(Position, Room), !.

/* Randomly place key in one of several locations */
random_key_placement :-
    /* Possible locations for key (not entrance, exit_hall, treasure_room) */
    KeyLocations = [guard_room, armory, library, prison_cell, secret_passage, storage,
                    vault, crypt, observatory, throne_room, gallery, great_hall],
    random_member(KeyLoc, KeyLocations),
    assert(at(key, KeyLoc)).

/* Randomly place items (torch, ancient_book, sword, shield, health_potion, rope) */
random_item_placement :-
    /* Possible locations for random items (not entrance, exit_hall, treasure_room) */
    ItemLocations = [guard_room, armory, library, prison_cell, secret_passage, storage,
                     dark_corridor, kitchen, barracks, wine_cellar, throne_room, gallery],

    /* Randomly place torch */
    random_member(TorchLoc, ItemLocations),
    assert(at(torch, TorchLoc)),

    /* Randomly place ancient_book (different from torch) */
    delete(ItemLocations, TorchLoc, Locs1),
    random_member(BookLoc, Locs1),
    assert(at(ancient_book, BookLoc)),

    /* Randomly place sword (different from above) */
    delete(Locs1, BookLoc, Locs2),
    random_member(SwordLoc, Locs2),
    assert(at(sword, SwordLoc)),

    /* Randomly place shield (different from above) */
    delete(Locs2, SwordLoc, Locs3),
    random_member(ShieldLoc, Locs3),
    assert(at(shield, ShieldLoc)),

    /* Randomly place health_potion (different from above) */
    delete(Locs3, ShieldLoc, Locs4),
    random_member(PotionLoc, Locs4),
    assert(at(health_potion, PotionLoc)),

    /* Randomly place rope (different from above) */
    delete(Locs4, PotionLoc, Locs5),
    random_member(RopeLoc, Locs5),
    assert(at(rope, RopeLoc)).

/* Randomly place 2 traps in dangerous locations */
random_trap_placement :-
    /* Possible trap locations (not entrance, exit_hall, treasure_room - too unfair) */
    TrapLocations = [guard_room, armory, library, prison_cell, secret_passage, storage],

    /* Place first trap */
    random_member(Trap1, TrapLocations),
    assert(trap_location(Trap1)),

    /* Place second trap (different location) */
    delete(TrapLocations, Trap1, Locs2),
    random_member(Trap2, Locs2),
    assert(trap_location(Trap2)).

/* Initialize adversaries with random starting positions */
init_adversaries :-
    /* Possible starting locations for enemies (not entrance, exit_hall, treasure_room) */
    EnemyLocations = [guard_room, armory, library, prison_cell, secret_passage, storage],

    /* Randomly place guard */
    random_member(GuardLoc, EnemyLocations),
    assert(adversary_at(guard, GuardLoc)),
    assert(adversary_type(guard, patrol)),

    /* Randomly place thief (different location) */
    delete(EnemyLocations, GuardLoc, Locs2),
    random_member(ThiefLoc, Locs2),
    assert(adversary_at(thief, ThiefLoc)),
    assert(adversary_type(thief, thief)),

    /* Generate initial plans for adversaries */
    generate_adversary_plan(guard),
    generate_adversary_plan(thief).

/* ============================================================================
   PDDL INTEGRATION - Dynamic Problem Generation
   ============================================================================ */

/* Generate PDDL problem file for an adversary */
generate_pddl_problem(Adversary, GoalLocation) :-
    adversary_at(Adversary, CurrentLoc),
    atom_concat('adversary_problem_', Adversary, BaseName),
    atom_concat(BaseName, '.pddl', FileName),
    open(FileName, write, Stream),
    write_pddl_problem(Stream, Adversary, CurrentLoc, GoalLocation),
    close(Stream).

write_pddl_problem(Stream, Adversary, CurrentLoc, GoalLoc) :-
    format(Stream, "(define (problem ~w_pursuit)~n", [Adversary]),
    format(Stream, "  (:domain dungeon)~n", []),
    format(Stream, "  (:objects~n", []),
    format(Stream, "    ~w - adversary~n", [Adversary]),
    write_locations(Stream),
    format(Stream, "  )~n", []),
    format(Stream, "  (:init~n", []),
    format(Stream, "    (at ~w ~w)~n", [Adversary, CurrentLoc]),
    write_connections(Stream),
    format(Stream, "  )~n", []),
    format(Stream, "  (:goal (at ~w ~w))~n", [Adversary, GoalLoc]),
    format(Stream, ")~n", []).

write_locations(Stream) :-
    findall(R, room(R), Rooms),
    forall(member(Room, Rooms),
           format(Stream, "    ~w - location~n", [Room])).

write_connections(Stream) :-
    findall((A,B), adjacent(A, B), Conns),
    forall(member((A,B), Conns),
           format(Stream, "    (connected ~w ~w)~n", [A, B])).

/* Generate and store plan for adversary */
generate_adversary_plan(Adversary) :-
    determine_goal(Adversary, GoalLoc),
    generate_pddl_problem(Adversary, GoalLoc),
    atom_concat('adversary_problem_', Adversary, BaseName),
    atom_concat(BaseName, '.pddl', ProblemFile),
    catch(
        run_pyperplan_soln(pyperplan, 'adversary_domain.pddl', ProblemFile, Plan),
        _Error,
        Plan = []
    ),
    retractall(adversary_plan(Adversary, _)),
    assert(adversary_plan(Adversary, Plan)),
    !.

generate_adversary_plan(Adversary) :-
    /* Fallback: if planning fails, use empty plan */
    retractall(adversary_plan(Adversary, _)),
    assert(adversary_plan(Adversary, [])).

/* Determine goal location based on adversary type */
/* Chaser with prediction: try to intercept player's predicted next location */
determine_goal(Adversary, GoalLoc) :-
    adversary_type(Adversary, chaser),
    predict_player_destination(PredictedLoc),
    PredictedLoc \= unknown,
    random(R),
    R < 0.6,  % 60% chance to use prediction
    !,
    GoalLoc = PredictedLoc.

determine_goal(Adversary, GoalLoc) :-
    adversary_type(Adversary, chaser),
    i_am_at(GoalLoc),
    !.

determine_goal(Adversary, GoalLoc) :-
    adversary_type(Adversary, thief),
    (at(gold_treasure, GoalLoc) -> true ;
     holding(gold_treasure) -> i_am_at(GoalLoc) ;
     GoalLoc = treasure_room),
    !.

determine_goal(Adversary, GoalLoc) :-
    adversary_type(Adversary, escaping),
    /* Thief with stolen treasure tries to reach exit */
    GoalLoc = exit_hall,
    !.

/* Patrol: random movement to adjacent locations */
determine_goal(Adversary, GoalLoc) :-
    adversary_type(Adversary, patrol),
    adversary_at(Adversary, CurrentLoc),
    /* Only consider adjacent rooms that are not locked */
    findall(L, (adjacent(CurrentLoc, L), \+ door_locked(L)), AdjList),
    length(AdjList, Len),
    Len > 0,
    random_between(1, Len, Idx),
    nth1(Idx, AdjList, GoalLoc),
    !.

determine_goal(_, entrance).  % Default fallback

/* Predict where player will go next - simplified without learning */
predict_player_destination(unknown).

/* ============================================================================
   PLAN EXECUTION AND VALIDATION
   ============================================================================ */

/* Execute one step of adversary's plan */
execute_adversary_turn(_) :-
    game_over,
    !.

/* If adversary is already at goal, don't move */
execute_adversary_turn(Adversary) :-
    adversary_at(Adversary, CurrentLoc),
    determine_goal(Adversary, GoalLoc),
    CurrentLoc = GoalLoc,
    !.

execute_adversary_turn(Adversary) :-
    adversary_plan(Adversary, [Action|Rest]),
    validate_and_execute_action(Adversary, Action, Success),
    (Success = true ->
        retract(adversary_plan(Adversary, _)),
        assert(adversary_plan(Adversary, Rest))
    ;
        /* Action failed, regenerate plan */
        generate_adversary_plan(Adversary)
    ),
    !.

execute_adversary_turn(Adversary) :-
    /* Empty plan or no plan - regenerate */
    generate_adversary_plan(Adversary),
    adversary_plan(Adversary, [Action|Rest]),
    validate_and_execute_action(Adversary, Action, _),
    retract(adversary_plan(Adversary, _)),
    assert(adversary_plan(Adversary, Rest)),
    !.

/* Fallback: random move if plan failed */
execute_adversary_turn(Adversary) :-
    adversary_at(Adversary, CurrentLoc),
    /* Only consider adjacent rooms that are not locked */
    findall(NextLoc, (adjacent(CurrentLoc, NextLoc), \+ door_locked(NextLoc)), AdjList),
    AdjList \= [],
    random_member(NextLoc, AdjList),
    retract(adversary_at(Adversary, CurrentLoc)),
    assert(adversary_at(Adversary, NextLoc)),
    check_adversary_encounter(Adversary, NextLoc),
    !.

execute_adversary_turn(_).  % Final fallback: do nothing

/* Validate action before execution */
/* Handle format: move(adversary, from, to) from pyperplan */
validate_and_execute_action(Adversary, move(Adversary, From, To), Success) :-
    adversary_at(Adversary, From),
    adjacent(From, To),
    /* Check if door is locked - enemies cannot pass through locked doors */
    \+ door_locked(To),
    !,
    retract(adversary_at(Adversary, From)),
    assert(adversary_at(Adversary, To)),
    Success = true,
    check_adversary_encounter(Adversary, To).

/* Handle format: move(from, to) - alternative format */
validate_and_execute_action(Adversary, move(From, To), Success) :-
    adversary_at(Adversary, From),
    adjacent(From, To),
    /* Check if door is locked - enemies cannot pass through locked doors */
    \+ door_locked(To),
    !,
    retract(adversary_at(Adversary, From)),
    assert(adversary_at(Adversary, To)),
    Success = true,
    check_adversary_encounter(Adversary, To).

validate_and_execute_action(_, _, false).

/* Check if adversary encounters player */
/* Thief robs player if player has treasure */
check_adversary_encounter(thief, Location) :-
    i_am_at(Location),
    holding(gold_treasure),
    !,
    handle_thief_robs_player.

/* Guard (or other chaser) attacks player */
check_adversary_encounter(Adversary, Location) :-
    i_am_at(Location),
    adversary_type(Adversary, chaser),
    Adversary \= thief,  /* Thief doesn't attack, only robs */
    !,
    handle_guard_encounter.

/* Thief steals treasure from ground */
check_adversary_encounter(thief, Location) :-
    at(gold_treasure, Location),
    !,
    handle_thief_steals.

check_adversary_encounter(_, _).

/* Handle guard capturing player */
handle_guard_encounter :-
    game_over,
    !.

handle_guard_encounter :-
    holding_weapon,
    holding_defense,
    !,
    nl, write('*** The guard attacks you! ***'), nl,
    guard_dialogue(combat),
    write('You block with your shield and counter with your sword!'), nl,
    get_guard_damage(Dmg),  % Use difficulty-adjusted damage
    BaseDmg is Dmg // 4,
    (holding(armor) ->
        (ActualDmg is max(1, BaseDmg * 9 // 10),
         write('Your armor absorbs some of the impact!'), nl)
    ;
        ActualDmg = BaseDmg
    ),
    player_health(H),
    NewH is H - ActualDmg,
    retractall(player_health(_)),
    assert(player_health(NewH)),
    format('You take only ~w damage. Health: ~w~n', [ActualDmg, NewH]),
    (NewH =< 0 -> player_dies ;
     (write('The guard retreats to regroup...'), nl,
      move_adversary_away(guard))).

handle_guard_encounter :-
    holding_weapon,
    !,
    nl, write('*** The guard attacks you! ***'), nl,
    guard_dialogue(combat),
    write('You defend yourself with your sword!'), nl,
    get_guard_damage(Dmg),  % Use difficulty-adjusted damage
    BaseDmg is Dmg // 2,
    (holding(armor) ->
        (ActualDmg is max(1, BaseDmg * 9 // 10),
         write('Your armor absorbs some of the impact!'), nl)
    ;
        ActualDmg = BaseDmg
    ),
    player_health(H),
    NewH is H - ActualDmg,
    retractall(player_health(_)),
    assert(player_health(NewH)),
    format('You take ~w damage. Health: ~w~n', [ActualDmg, NewH]),
    (NewH =< 0 -> player_dies ;
     (write('The guard retreats to regroup...'), nl,
      move_adversary_away(guard))).

handle_guard_encounter :-
    holding_defense,
    !,
    nl, write('*** The guard attacks you! ***'), nl,
    guard_dialogue(combat),
    write('You block with your shield but cannot fight back!'), nl,
    get_guard_damage(Dmg),  % Use difficulty-adjusted damage
    BaseDmg is Dmg * 3 // 4,
    (holding(armor) ->
        (ActualDmg is max(1, BaseDmg * 9 // 10),
         write('Your armor absorbs some of the impact!'), nl)
    ;
        ActualDmg = BaseDmg
    ),
    player_health(H),
    NewH is H - ActualDmg,
    retractall(player_health(_)),
    assert(player_health(NewH)),
    format('You take ~w damage. Health: ~w~n', [ActualDmg, NewH]),
    (NewH =< 0 -> player_dies ; true).

handle_guard_encounter :-
    nl, write('*** The guard catches you! ***'), nl,
    guard_dialogue(capture),
    write('Without a weapon, you cannot defend yourself.'), nl,
    get_guard_damage(Dmg),  % Use difficulty-adjusted damage
    (holding(armor) ->
        (ActualDmg is max(1, Dmg * 9 // 10),
         write('Your armor absorbs some of the impact!'), nl)
    ;
        ActualDmg = Dmg
    ),
    player_health(H),
    NewH is H - ActualDmg,
    retractall(player_health(_)),
    assert(player_health(NewH)),
    format('You take ~w damage. Health: ~w~n', [ActualDmg, NewH]),
    (NewH =< 0 -> player_dies ; true).

move_adversary_away(Adversary) :-
    adversary_at(Adversary, Loc),
    /* Only consider adjacent rooms that are not locked and not occupied by player */
    findall(L, (adjacent(Loc, L), \+ i_am_at(L), \+ door_locked(L)), Options),
    Options \= [],
    random_member(NewLoc, Options),
    retract(adversary_at(Adversary, Loc)),
    assert(adversary_at(Adversary, NewLoc)),
    generate_adversary_plan(Adversary).

move_adversary_away(_).

/* Handle thief stealing treasure */
handle_thief_steals :-
    treasure_stolen,
    !.

handle_thief_steals :-
    retract(at(gold_treasure, _)),
    assert(treasure_stolen),
    nl, write('*** Oh no! The thief has stolen the treasure! ***'), nl,
    thief_dialogue(steal),
    write('You must catch the thief to recover it!'), nl,
    /* Thief now tries to escape */
    retract(adversary_type(thief, _)),
    assert(adversary_type(thief, escaping)),
    generate_adversary_plan(thief).

/* Handle thief robbing player of treasure */
handle_thief_robs_player :-
    treasure_stolen,
    !.  /* Already stolen, do nothing */

handle_thief_robs_player :-
    retract(holding(gold_treasure)),
    assert(treasure_stolen),
    nl, write('*** The thief snatches the treasure right from your hands! ***'), nl,
    thief_dialogue(steal),
    write('Chase the thief and get it back!'), nl,
    /* Thief now tries to escape */
    retractall(adversary_type(thief, _)),
    assert(adversary_type(thief, escaping)),
    generate_adversary_plan(thief).

/* ============================================================================
   DYNAMIC PLAN EVOLUTION - Adversaries adapt to game state
   ============================================================================ */

/* Update adversary behaviors based on game state */
update_adversary_behaviors :-
    /* Guard becomes chaser if player has treasure OR enters guard_room OR is spotted */
    trigger_guard_chase,

    /* Thief behavior based on treasure state */
    update_thief_behavior,

    /* Collaborative behavior: adversaries share player location */
    coordinate_adversaries.

/* Guard chase triggers */
trigger_guard_chase :-
    adversary_type(guard, chaser),
    !,
    /* Already chasing - but regenerate plan to track player's NEW position */
    generate_adversary_plan(guard).

/* Trigger 1: Player and guard in same room */
trigger_guard_chase :-
    i_am_at(PlayerLoc),
    adversary_at(guard, PlayerLoc),
    !,
    start_guard_chase('The guard spots you! You\'ve been discovered!').

/* Trigger 2: Player picked up the key */
trigger_guard_chase :-
    holding(key),
    !,
    start_guard_chase('The guard notices the key is missing! He\'s coming for you!').

trigger_guard_chase.  /* No trigger */

start_guard_chase(Message) :-
    retractall(adversary_type(guard, _)),
    assert(adversary_type(guard, chaser)),
    nl, write('*** '), write(Message), write(' ***'), nl,
    write('The guard is now actively hunting you!'), nl,
    /* If guard is already in player's room, attack immediately */
    (i_am_at(PlayerLoc), adversary_at(guard, PlayerLoc) ->
        handle_guard_encounter
    ;
        generate_adversary_plan(guard)
    ).

/* Thief chase triggers - ONLY when player has gold_treasure */
update_thief_behavior :-
    treasure_stolen,
    adversary_type(thief, escaping),
    !,
    /* Thief tries to reach exit with treasure */
    generate_adversary_plan(thief).

update_thief_behavior :-
    adversary_type(thief, chaser),
    !,
    /* Already chasing - but regenerate plan to track player's NEW position */
    generate_adversary_plan(thief).

update_thief_behavior :-
    holding(gold_treasure),
    !,
    /* Player has treasure, thief becomes chaser */
    start_thief_chase('The thief wants your treasure!').

update_thief_behavior.  /* No trigger */

start_thief_chase(Message) :-
    retractall(adversary_type(thief, _)),
    assert(adversary_type(thief, chaser)),
    nl, write('*** '), write(Message), write(' ***'), nl,
    write('The thief is now stalking you!'), nl,
    generate_adversary_plan(thief).

/* Adversaries coordinate - guard tells thief where player is */
coordinate_adversaries :-
    i_am_at(PlayerLoc),
    adversary_at(guard, PlayerLoc),
    adversary_at(thief, ThiefLoc),
    ThiefLoc \= PlayerLoc,
    /* Guard "signals" thief */
    random(R),
    R < 0.3,  % 30% chance of coordination
    !,
    nl, write('The guard signals to the thief your location!'), nl,
    generate_adversary_plan(thief).

coordinate_adversaries.

/* ============================================================================
   PLAYER ACTIONS
   ============================================================================ */

/* Take an object */
take(_) :-
    game_over,
    !,
    write('The game is over.'), nl.

take(X) :-
    holding(X),
    write('You''re already holding it!'),
    !, nl.

take(X) :-
    i_am_at(Place),
    at(X, Place),
    retract(at(X, Place)),
    /* Check if it's a valuable item - convert to gold */
    (X = ancient_coin ->
        (player_gold(G), NewG is G + 25, retract(player_gold(_)), assert(player_gold(NewG)),
         format('You pick up an ancient coin worth 25 gold! Total gold: ~w~n', [NewG]))
    ; item_value(X, Value) ->
        (assert(holding(X)),
         (item_collected(X) -> true ; assert(item_collected(X))),
         format('OK. You pick up the ~w (worth ~w gold).~n', [X, Value]))
    ;
        (assert(holding(X)),
         (item_collected(X) -> true ; assert(item_collected(X))),
         write('OK. You pick up the '), write(X), write('.'), nl)
    ),
    (light_source(X) ->
        (assert(has_light), write('The torch illuminates the area!'), nl)
    ; true),
    (X = gold_treasure ->
        (nl, write('*** You have the treasure! Now escape! ***'), nl,
         write('But beware - the THIEF will hunt you down!'), nl,
         /* Thief discovers player when treasure is taken */
         (\+ thief_discovered ->
            (assert(thief_discovered),
             write('*** The THIEF has noticed you have the treasure! They will chase you! ***'), nl)
         ; true))
    ; true),
    !, advance_turn.

take(thief) :-
    i_am_at(Place),
    adversary_at(thief, Place),
    adversary_at(guard, Place),
    treasure_stolen,
    !,
    write('You try to grab the thief, but the guard blocks your way!'), nl,
    write('Deal with the guard first, or find another way.'), nl.

take(thief) :-
    i_am_at(Place),
    adversary_at(thief, Place),
    treasure_stolen,
    !,
    write('You tackle the thief and recover the treasure!'), nl,
    retract(treasure_stolen),
    assert(holding(gold_treasure)),
    /* Track item collection for collector achievement */
    (item_collected(gold_treasure) -> true ; assert(item_collected(gold_treasure))),
    retract(adversary_at(thief, Place)),
    assert(adversary_at(thief, prison_cell)),
    write('The thief is knocked out. You have the treasure!'), nl,
    advance_turn.

take(_) :-
    write('I don''t see it here.'),
    nl.

/* Drop an object */
drop(_) :-
    game_over,
    !,
    write('The game is over.'), nl.

drop(X) :-
    holding(X),
    i_am_at(Place),
    retract(holding(X)),
    assert(at(X, Place)),
    write('OK. You drop the '), write(X), write('.'), nl,
    (light_source(X) -> retract(has_light) ; true),
    !, advance_turn.

drop(_) :-
    write('You aren''t holding it!'),
    nl.

/* Get maximum health based on difficulty */
get_max_health(MaxHealth) :-
    get_difficulty(Level),
    difficulty_modifier(Level, health_bonus, Bonus),
    MaxHealth is 100 + Bonus.

/* Use an object */
use(health_potion) :-
    holding(health_potion),
    !,
    player_health(H),
    potion_heal(Heal),
    get_max_health(MaxH),
    NewH is min(MaxH, H + Heal),
    retract(player_health(_)),
    assert(player_health(NewH)),
    retract(holding(health_potion)),
    format('You drink the health potion. Health restored to ~w/~w.~n', [NewH, MaxH]),
    advance_turn.

use(key) :-
    holding(key),
    door_locked(exit_hall),
    i_am_at(PlayerLoc),
    adjacent(PlayerLoc, exit_hall),
    !,
    retract(door_locked(exit_hall)),
    retract(holding(key)),
    write('You unlock the door to the exit hall!'), nl,
    write('The key breaks in the lock and crumbles to dust.'), nl,
    advance_turn.

use(key) :-
    holding(key),
    door_locked(exit_hall),
    !,
    write('You need to be next to the exit hall door to use the key.'), nl.

use(torch) :-
    holding(torch),
    !,
    (has_light ->
        write('The torch is already lit.')
    ;
        (assert(has_light), write('You light the torch. The darkness recedes!'))
    ), nl.

use(ancient_book) :-
    holding(ancient_book),
    !,
    write('You read the ancient book...'), nl,
    write('"To escape this dungeon, find the key to unlock the exit.'), nl,
    write('Beware the guard who patrols these halls.'), nl,
    write('And watch for the thief who seeks the same treasure you do."'), nl.

use(shield) :-
    holding(shield),
    !,
    write('You raise the shield defensively.'), nl,
    write('It will reduce damage from the guard''s attacks.'), nl.

use(sword) :-
    holding(sword),
    !,
    write('You brandish your sword menacingly.'), nl,
    write('You can attack enemies and defend yourself with it.'), nl.

use(dagger) :-
    holding(dagger),
    !,
    write('You grip the dagger tightly.'), nl,
    write('It''s not as good as a sword, but better than nothing!'), nl.

use(rope) :-
    holding(rope),
    i_am_at(Place),
    adversary_at(thief, Place),
    !,
    write('You use the rope to tie up the thief!'), nl,
    retract(adversary_at(thief, Place)),
    assert(adversary_at(thief, prison_cell)),
    (treasure_stolen ->
        (retract(treasure_stolen),
         assert(at(gold_treasure, Place)),
         write('The stolen treasure falls to the ground!'), nl)
    ; true),
    retract(holding(rope)),
    /* Note: Using rope is a non-violent capture, so we don't set thief_defeated */
    /* This allows the pacifist achievement to be earned */
    write('The thief is now imprisoned.'), nl,
    advance_turn.

use(rope) :-
    holding(rope),
    !,
    write('You need to be in the same room as the thief to use the rope.'), nl.

/* New item uses */
use(lantern) :-
    holding(lantern),
    !,
    (has_light ->
        write('You already have light.')
    ;
        (assert(has_light), write('The lantern glows brightly, illuminating the darkness!'))
    ), nl.

use(food_ration) :-
    holding(food_ration),
    !,
    player_health(H),
    get_max_health(MaxH),
    NewH is min(MaxH, H + 15),
    retract(player_health(_)),
    assert(player_health(NewH)),
    retract(holding(food_ration)),
    format('You eat the food ration. Health restored to ~w/~w.~n', [NewH, MaxH]),
    advance_turn.

use(map_scroll) :-
    holding(map_scroll),
    !,
    write('You unroll the ancient map scroll...'), nl,
    write('The entire dungeon layout is revealed to you!'), nl,
    reveal_all_rooms,
    retract(holding(map_scroll)),
    advance_turn.

use(bomb) :-
    holding(bomb),
    i_am_at(Place),
    adversary_at(Enemy, Place),
    !,
    format('You throw the bomb at the ~w!~n', [Enemy]),
    write('BOOM! The explosion echoes through the dungeon!'), nl,
    retract(holding(bomb)),
    (Enemy = guard ->
        (write('The guard is knocked unconscious!'), nl,
         retract(adversary_at(guard, Place)),
         assert(guard_defeated))
    ;
        (write('The thief is stunned and captured!'), nl,
         retract(adversary_at(thief, Place)),
         assert(adversary_at(thief, prison_cell)),
         (treasure_stolen ->
             (retract(treasure_stolen),
              assert(at(gold_treasure, Place)),
              write('The treasure falls to the ground!'), nl)
         ; true))
    ),
    advance_turn.

use(bomb) :-
    holding(bomb),
    !,
    write('There is no enemy here to use the bomb on.'), nl.

use(lockpick) :-
    holding(lockpick),
    i_am_at(PlayerLoc),
    adjacent(PlayerLoc, exit_hall),
    door_locked(exit_hall),
    !,
    random_between(1, 100, Roll),
    (Roll > 30 ->
        (retract(door_locked(exit_hall)),
         retract(holding(lockpick)),
         write('Click! You successfully pick the lock!'), nl,
         write('The door to the exit hall swings open.'), nl)
    ;
        (retract(holding(lockpick)),
         write('The lockpick breaks in the lock!'), nl,
         write('You will need to find the key now.'), nl)
    ),
    advance_turn.

use(lockpick) :-
    holding(lockpick),
    door_locked(exit_hall),
    !,
    write('You need to be next to the exit hall door to use the lockpick.'), nl.

use(lockpick) :-
    holding(lockpick),
    !,
    write('The exit door is already unlocked.'), nl.

use(armor) :-
    holding(armor),
    !,
    write('You put on the armor. You feel well protected!'), nl,
    write('Damage taken will be reduced.'), nl.

use(magic_ring) :-
    holding(magic_ring),
    !,
    retract(holding(magic_ring)),
    player_gold(G),
    NewG is G + 50,
    retractall(player_gold(_)),
    assert(player_gold(NewG)),
    nl, write('You activate the magic ring!'), nl,
    write('It dissolves in a shower of gold coins!'), nl,
    format('You gained 50 gold! Total gold: ~w~n', [NewG]),
    (bonus_score(BS) ->
        (retract(bonus_score(_)), NewBS is BS + 50)
    ;
        NewBS = 50
    ),
    assert(bonus_score(NewBS)).

/* Sell valuable items for gold */
use(X) :-
    holding(X),
    valuable(X),
    X \= gold_treasure,
    item_value(X, Value),
    !,
    retract(holding(X)),
    player_gold(G),
    NewG is G + Value,
    retract(player_gold(_)),
    assert(player_gold(NewG)),
    format('You sell the ~w for ~w gold! Total gold: ~w~n', [X, Value, NewG]).

use(X) :-
    holding(X),
    !,
    write('You can''t use that here.'), nl.

use(_) :-
    write('You don''t have that.'), nl.

/* ============================================================================
   GOLD AND SHOP SYSTEM
   ============================================================================ */

/* Show gold command */
gold :-
    player_gold(G),
    format('You have ~w gold coins.~n', [G]).

/* Shop command - only works in marketplace */
shop :-
    i_am_at(marketplace),
    !,
    nl, write('=== MARKETPLACE SHOP ==='), nl,
    write('Available items for sale:'), nl,
    forall(shop_item(Item, Price),
           format('  ~w - ~w gold~n', [Item, Price])),
    nl,
    player_gold(G),
    format('Your gold: ~w~n', [G]),
    write('Use: buy(item). to purchase.'), nl.

shop :-
    write('There is no shop here. Visit the marketplace to buy items.'), nl.

/* Buy command */
buy(Item) :-
    i_am_at(marketplace),
    shop_item(Item, Price),
    player_gold(G),
    G >= Price,
    !,
    NewG is G - Price,
    retract(player_gold(_)),
    assert(player_gold(NewG)),
    assert(holding(Item)),
    format('You bought ~w for ~w gold. Remaining gold: ~w~n', [Item, Price, NewG]),
    advance_turn.

buy(Item) :-
    i_am_at(marketplace),
    shop_item(Item, Price),
    player_gold(G),
    G < Price,
    !,
    format('You need ~w gold but only have ~w.~n', [Price, G]).

buy(Item) :-
    i_am_at(marketplace),
    \+ shop_item(Item, _),
    !,
    format('~w is not available in this shop.~n', [Item]).

buy(_) :-
    write('You need to be in the marketplace to buy items.'), nl.

/* Sell command */
sell(Item) :-
    holding(Item),
    item_value(Item, Value),
    Item \= gold_treasure,
    !,
    retract(holding(Item)),
    player_gold(G),
    NewG is G + Value,
    retract(player_gold(_)),
    assert(player_gold(NewG)),
    format('You sold ~w for ~w gold! Total gold: ~w~n', [Item, Value, NewG]).

sell(gold_treasure) :-
    holding(gold_treasure),
    !,
    write('You cannot sell the golden treasure! You need it to escape!'), nl.

sell(Item) :-
    holding(Item),
    !,
    format('~w has no gold value.~n', [Item]).

sell(_) :-
    write('You don''t have that item.'), nl.

/* ============================================================================
   COMBAT SYSTEM - Active Attack
   ============================================================================ */

/* Attack command */
attack :-
    game_over,
    !,
    write('The game is over.'), nl.

attack :-
    \+ holding_weapon,
    !,
    write('You have no weapon to attack with!'), nl,
    write('Find a sword first.'), nl.

attack :-
    i_am_at(Place),
    adversary_at(guard, Place),
    !,
    attack_guard.

attack :-
    i_am_at(Place),
    adversary_at(thief, Place),
    !,
    attack_thief.

attack :-
    write('There is no one here to attack.'), nl.

/* Get difficulty-adjusted hit chance modifier */
get_hit_bonus(25) :-
    get_difficulty(easy),
    !.

get_hit_bonus(Bonus) :-
    get_difficulty(hard),
    difficulty_modifier(hard, hit_penalty, Bonus),
    !.

get_hit_bonus(0).  % Normal difficulty

/* Get difficulty-adjusted kill threshold modifier */
get_kill_bonus(25) :-
    get_difficulty(easy),
    !.

get_kill_bonus(Bonus) :-
    get_difficulty(hard),
    difficulty_modifier(hard, kill_penalty, Bonus),
    !.

get_kill_bonus(0).  % Normal difficulty

/* Get difficulty-adjusted counter damage */
get_counter_damage(BaseDmg, FinalDmg) :-
    get_difficulty(easy),
    !,
    FinalDmg is BaseDmg // 2.  % Half damage on easy

get_counter_damage(BaseDmg, FinalDmg) :-
    get_difficulty(hard),
    !,
    difficulty_modifier(hard, counter_mult, Mult),
    FinalDmg is round(BaseDmg * Mult).

get_counter_damage(BaseDmg, BaseDmg).  % Normal difficulty

/* Attack the guard */
attack_guard :-
    holding_weapon,
    holding_defense,
    !,
    nl, write('*** You attack the guard with sword and shield! ***'), nl,
    random_between(1, 100, Roll),
    get_hit_bonus(HitBonus),
    get_kill_bonus(KillBonus),
    Hit is Roll + HitBonus,
    (Hit > 20 ->
        (write('Your blade strikes true! The guard is wounded!'), nl,
         random_between(1, 100, Kill),
         KillThreshold is 50 - KillBonus,
         (Kill > KillThreshold ->
             (write('The guard falls! You have defeated him!'), nl,
              i_am_at(Place),
              retractall(adversary_at(guard, Place)),
              assert(guard_defeated))
         ;
             (write('The guard staggers back but still stands...'), nl,
              move_adversary_away(guard))
         ))
    ;
        (write('The guard blocks your attack!'), nl,
         write('He counter-attacks!'), nl,
         player_health(H),
         get_counter_damage(10, Dmg),
         NewH is H - Dmg,
         retractall(player_health(_)),
         assert(player_health(NewH)),
         format('You take ~w damage. Health: ~w~n', [Dmg, NewH]),
         (NewH =< 0 -> player_dies ; true))
    ),
    advance_turn.

attack_guard :-
    holding_weapon,
    !,
    nl, write('*** You attack the guard with your sword! ***'), nl,
    random_between(1, 100, Roll),
    get_hit_bonus(HitBonus),
    get_kill_bonus(KillBonus),
    Hit is Roll + HitBonus,
    (Hit > 40 ->
        (write('You land a hit on the guard!'), nl,
         random_between(1, 100, Kill),
         KillThreshold is 70 - KillBonus,
         (Kill > KillThreshold ->
             (write('The guard collapses! Victory!'), nl,
              i_am_at(Place),
              retractall(adversary_at(guard, Place)),
              assert(guard_defeated))
         ;
             (write('The guard is hurt but fights on...'), nl,
              move_adversary_away(guard))
         ))
    ;
        (write('The guard dodges your attack!'), nl,
         write('He strikes back!'), nl,
         player_health(H),
         get_counter_damage(15, Dmg),
         NewH is H - Dmg,
         retractall(player_health(_)),
         assert(player_health(NewH)),
         format('You take ~w damage. Health: ~w~n', [Dmg, NewH]),
         (NewH =< 0 -> player_dies ; true))
    ),
    advance_turn.

/* Attack the thief */
attack_thief :-
    holding_weapon,
    !,
    nl, write('*** You attack the thief! ***'), nl,
    random_between(1, 100, Roll),
    get_hit_bonus(HitBonus),
    get_kill_bonus(KillBonus),
    Hit is Roll + HitBonus,
    (Hit > 35 ->
        (write('You strike the thief!'), nl,
         random_between(1, 100, Kill),
         KillThreshold is 40 - KillBonus,
         (Kill > KillThreshold ->
             (write('The thief is knocked unconscious!'), nl,
              i_am_at(Place),
              retractall(adversary_at(thief, Place)),
              assert(adversary_at(thief, prison_cell)),
              (treasure_stolen ->
                  (retract(treasure_stolen),
                   assert(at(gold_treasure, Place)),
                   write('The stolen treasure falls to the ground!'), nl)
              ; true),
              assert(thief_defeated))
         ;
             (write('The thief stumbles but escapes!'), nl,
              move_adversary_away(thief))
         ))
    ;
        (write('The thief dodges nimbly!'), nl,
         write('He slashes at you with a dagger!'), nl,
         player_health(H),
         get_counter_damage(10, Dmg),
         NewH is H - Dmg,
         retractall(player_health(_)),
         assert(player_health(NewH)),
         format('You take ~w damage. Health: ~w~n', [Dmg, NewH]),
         (NewH =< 0 -> player_dies ; true))
    ),
    advance_turn.

/* Shortcut commands for attack */
fight :- attack.
kill :- attack.
hit :- attack.

/* Attack specific target */
attack(guard) :-
    i_am_at(Place),
    adversary_at(guard, Place),
    !,
    attack_guard.
    % Note: advance_turn is already called inside attack_guard

attack(thief) :-
    i_am_at(Place),
    adversary_at(thief, Place),
    !,
    attack_thief.
    % Note: advance_turn is already called inside attack_thief

attack(Target) :-
    format('~w is not here.~n', [Target]).

/* Movement */
n :- go(n).
s :- go(s).
e :- go(e).
w :- go(w).

go(_) :-
    game_over,
    !,
    write('The game is over.'), nl.

go(Direction) :-
    i_am_at(Here),
    get_path(Here, Direction, There),
    check_locked(There),
    !,
    retract(i_am_at(Here)),
    assert(i_am_at(There)),
    (room_visited(There) -> true ; assert(room_visited(There))),
    /* Check if player discovers an adversary in this room */
    check_adversary_discovery(There),
    check_trap(There),
    look,
    advance_turn.

/* Check if player enters a room with an adversary - discover them */
check_adversary_discovery(Room) :-
    adversary_at(guard, Room),
    \+ guard_discovered,
    !,
    assert(guard_discovered),
    assert(guard_discovered_at(Room)),
    write('*** You have discovered the GUARD! They will now hunt you! ***'), nl.
/* Thief discovery is handled in take(gold_treasure), not by entering room */
check_adversary_discovery(_).

go(_) :-
    write('You can''t go that way.'), nl.

/* Get path - uses dynamic_path only (random map) */
get_path(Here, Direction, There) :-
    dynamic_path(Here, Direction, There).

check_locked(Room) :-
    door_locked(Room),
    !,
    write('The door is locked! You need a key.'), nl,
    fail.

check_locked(_).

/* Trap handling */
check_trap(Room) :-
    trap_location(Room),
    \+ trap_triggered(Room),
    \+ has_light,
    !,
    assert(trap_triggered(Room)),
    get_trap_damage(Dmg),  % Use difficulty-adjusted damage
    MinDmg is max(1, Dmg // 2),
    random_between(MinDmg, Dmg, ActualDmg),
    player_health(H),
    NewH is H - ActualDmg,
    retractall(player_health(_)),
    assert(player_health(NewH)),
    nl, write('*** TRAP! ***'), nl,
    format('You stumble into a trap in the darkness! You take ~w damage.~n', [ActualDmg]),
    format('Health: ~w~n', [NewH]),
    (NewH =< 0 -> player_dies ; true).

check_trap(Room) :-
    trap_location(Room),
    \+ trap_triggered(Room),
    has_light,
    !,
    assert(trap_triggered(Room)),
    write('Your torch reveals a trap! You carefully avoid it.'), nl.

check_trap(_).

/* Look around */
look :-
    i_am_at(Place),
    describe_with_fog(Place),
    nl,
    notice_objects_at(Place),
    notice_adversaries_at(Place),
    notice_trap_warning(Place),
    notice_exits_with_fog(Place),
    show_status.

/* Fog of War: describe room based on exploration status */
describe_with_fog(Place) :-
    room_visited(Place),
    !,
    describe(Place).

describe_with_fog(_) :-
    write('You enter an unexplored area...'), nl,
    write('The darkness makes it hard to see. You must explore to learn more.'), nl.

/* Warn about potential traps */
notice_trap_warning(Place) :-
    trap_location(Place),
    \+ trap_triggered(Place),
    has_light,
    !,
    write('*** Your torch reveals a TRAP here! Be careful! ***'), nl.

notice_trap_warning(Place) :-
    trap_location(Place),
    \+ trap_triggered(Place),
    \+ has_light,
    !,
    write('*** You sense danger in the darkness... ***'), nl.

notice_trap_warning(_).

notice_objects_at(Place) :-
    at(X, Place),
    write('There is a '), write(X), write(' here.'), nl,
    fail.

notice_objects_at(_).

notice_adversaries_at(Place) :-
    adversary_at(Adv, Place),
    write('*** WARNING: The '), write(Adv), write(' is here! ***'), nl,
    fail.

notice_adversaries_at(_).

notice_exits(Place) :-
    nl, write('Exits: '),
    findall(Dir, dynamic_path(Place, Dir, _), Dirs),
    print_directions(Dirs),
    nl.

/* Fog of War: Show exits with destination info only for explored rooms */
notice_exits_with_fog(Place) :-
    nl, write('Exits:'), nl,
    findall((Dir, Dest), dynamic_path(Place, Dir, Dest), Exits),
    print_exits_with_fog(Exits).

print_exits_with_fog([]) :- !.
print_exits_with_fog([(Dir, Dest)|Rest]) :-
    write('  '), write(Dir), write(' -> '),
    (room_visited(Dest) ->
        write(Dest)
    ;
        write('???')
    ),
    nl,
    print_exits_with_fog(Rest).

print_directions([]) :- !.
print_directions([D]) :- write(D), !.
print_directions([D|Rest]) :- write(D), write(', '), print_directions(Rest).

show_status :-
    player_health(H),
    turn_count(T),
    get_max_health(MaxH),
    TurnsLeft is 100 - T,
    format('~n[Health: ~w/~w | Turns Left: ~w]~n', [H, MaxH, TurnsLeft]).

/* Inventory */
inventory :-
    write('You are carrying:'), nl,
    holding(Item),
    write('  - '), write(Item), nl,
    fail.

inventory :-
    \+ holding(_),
    write('  (nothing)'), nl,
    !.

inventory :- !.

i :- inventory.

/* ============================================================================
   TURN ADVANCEMENT AND ADVERSARY AI
   ============================================================================ */

advance_turn :-
    game_over,
    !.

advance_turn :-
    turn_count(T),
    NewT is T + 1,
    retract(turn_count(_)),
    assert(turn_count(NewT)),

    /* Trigger random events */
    maybe_random_event,

    /* Update adversary behaviors */
    update_adversary_behaviors,

    /* Execute adversary turns */
    execute_all_adversary_turns,

    /* Check win/lose conditions */
    check_game_state.

/* ============================================================================
   RANDOM EVENTS SYSTEM
   ============================================================================ */

:- dynamic bonus_score/1.
:- dynamic event_cooldown/1.

/* Maybe trigger a random event (30% chance, no cooldown) */
maybe_random_event :-
    random(R),
    R < 0.30,  % 30% chance
    !,
    random_between(1, 8, EventType),
    trigger_random_event(EventType).

maybe_random_event.

/* Event 1: Hear footsteps - hints enemy direction */
trigger_random_event(1) :-
    i_am_at(PlayerLoc),
    adversary_at(guard, GuardLoc),
    GuardLoc \= PlayerLoc,
    !,
    nl, write('~~ RANDOM EVENT ~~'), nl,
    write('You hear heavy footsteps echoing from the '),
    get_direction_hint(PlayerLoc, GuardLoc, Dir),
    format('~w...~n', [Dir]),
    nl.

/* Event 2: Find coins - adds both gold and bonus score */
trigger_random_event(2) :-
    !,
    nl, write('~~ RANDOM EVENT ~~'), nl,
    random_between(10, 50, Coins),
    format('You find ~w gold coins hidden in a crack in the wall!~n', [Coins]),
    /* Add actual gold coins */
    player_gold(G),
    NewG is G + Coins,
    retract(player_gold(_)),
    assert(player_gold(NewG)),
    format('Total gold: ~w~n', [NewG]),
    /* Also add bonus score */
    (bonus_score(BS) ->
        (retract(bonus_score(_)), NewBS is BS + Coins)
    ;
        NewBS = Coins
    ),
    assert(bonus_score(NewBS)),
    nl.

/* Event 3: Earthquake - moves enemies randomly */
trigger_random_event(3) :-
    !,
    nl, write('~~ RANDOM EVENT ~~'), nl,
    write('The dungeon shakes! An earthquake!'), nl,
    write('You hear confused shouts as enemies scramble...'), nl,
    random_relocate_enemy(guard),
    random_relocate_enemy(thief),
    nl.

/* Event 4: Ghost whisper - gives hint */
trigger_random_event(4) :-
    !,
    nl, write('~~ RANDOM EVENT ~~'), nl,
    write('A ghostly voice whispers: '),
    random_hint(Hint),
    format('"~w"~n', [Hint]),
    nl.

/* Event 5: Find healing herbs */
trigger_random_event(5) :-
    player_health(H),
    get_max_health(MaxH),
    H < MaxH,
    !,
    nl, write('~~ RANDOM EVENT ~~'), nl,
    write('You find some healing herbs growing in the cracks!'), nl,
    random_between(5, 15, Heal),
    NewH is min(MaxH, H + Heal),
    retract(player_health(_)),
    assert(player_health(NewH)),
    format('You eat them and recover ~w health. Health: ~w/~w~n', [Heal, NewH, MaxH]),
    nl.

/* Event 6: Rat steals item (only if holding something non-essential) */
trigger_random_event(6) :-
    holding(Item),
    \+ member(Item, [gold_treasure, key, sword, shield]),
    !,
    nl, write('~~ RANDOM EVENT ~~'), nl,
    format('A giant rat snatches your ~w and runs away!~n', [Item]),
    retract(holding(Item)),
    i_am_at(Loc),
    random_adjacent_room(Loc, NewLoc),
    assert(at(Item, NewLoc)),
    /* Only hint direction, not the actual room name */
    (dynamic_path(Loc, Dir, NewLoc) ->
        format('You see it scurry ~w into the darkness...~n', [Dir])
    ;
        write('You see it disappear into a crack in the wall...'), nl
    ),
    nl.

/* Event 7: Torch flickers or extinguishes */
trigger_random_event(7) :-
    has_light,
    holding(torch),
    !,
    nl, write('~~ RANDOM EVENT ~~'), nl,
    random(R),
    (R < 0.3 ->
        /* 30% chance torch goes out */
        (retract(has_light),
         write('A sudden gust of wind blows out your torch!'), nl,
         write('You are plunged into darkness! Use the torch to relight it.'), nl)
    ;
        /* 70% chance just flickers */
        (write('Your torch flickers wildly in a cold breeze...'), nl,
         write('For a moment, shadows dance menacingly on the walls.'), nl)
    ),
    nl.

/* Event 7 fallback: Strange noise if no torch */
trigger_random_event(7) :-
    !,
    nl, write('~~ RANDOM EVENT ~~'), nl,
    random_member(Sound, [
        'You hear chains rattling in the distance...',
        'A cold wind blows through the corridor...',
        'You feel like you are being watched...',
        'Distant screams echo through the dungeon...',
        'Water drips somewhere in the darkness...'
    ]),
    write(Sound), nl,
    nl.

/* Event 8: Lucky find - discover something useful */
/* Note: old_map, small_gem, and rusty_dagger are event effects, not actual objects */
/* They are used as symbolic identifiers for different event outcomes */
trigger_random_event(8) :-
    i_am_at(_),
    !,
    nl, write('~~ RANDOM EVENT ~~'), nl,
    write('You notice something glinting in a corner...'), nl,
    /* old_map is rare (10% chance), others are more common */
    random_between(1, 100, Roll),
    (Roll =< 10 ->
        Found = old_map
    ; Roll =< 55 ->
        Found = small_gem
    ;
        Found = rusty_dagger
    ),
    (Found = old_map ->
        (write('An old map! The fog in your mind clears...'), nl,
         write('You now remember the entire dungeon layout!'), nl,
         reveal_all_rooms,
         write('(Use "map." to see the revealed dungeon)'), nl)
    ; Found = small_gem ->
        (write('A small gem! It might be valuable.'), nl,
         (bonus_score(BS) ->
            (retract(bonus_score(_)), NewBS is BS + 100)
         ;
            NewBS = 100
         ),
         assert(bonus_score(NewBS)))
    ;
        (write('A rusty dagger. Not very useful, but interesting.'), nl)
    ),
    nl.

/* Fallback - no event */
trigger_random_event(_) :- !.

/* Helper: Get direction hint */
get_direction_hint(From, To, Direction) :-
    dynamic_path(From, Dir, To),
    !,
    dir_name(Dir, Direction).

get_direction_hint(From, To, Direction) :-
    dynamic_path(From, Dir, Mid),
    dynamic_path(Mid, _, To),
    !,
    dir_name(Dir, Direction).

get_direction_hint(_, _, 'somewhere nearby').

dir_name(n, north).
dir_name(s, south).
dir_name(e, east).
dir_name(w, west).

/* Helper: Random hints */
random_hint(Hint) :-
    random_member(Hint, [
        'The key opens many doors, but breaks after one use...',
        'Light reveals what darkness hides...',
        'The thief fears the rope more than the sword...',
        'Treasure without escape is worthless...',
        'The guard tires when he cannot find you...',
        'Secret passages connect distant places...',
        'Shield yourself before facing the guard...'
    ]).

/* Helper: Randomly relocate an enemy */
random_relocate_enemy(Enemy) :-
    adversary_at(Enemy, OldLoc),
    EnemyLocations = [guard_room, armory, library, prison_cell, secret_passage, storage],
    delete(EnemyLocations, OldLoc, ValidLocs),
    random_member(NewLoc, ValidLocs),
    retract(adversary_at(Enemy, OldLoc)),
    assert(adversary_at(Enemy, NewLoc)),
    retractall(adversary_plan(Enemy, _)),
    assert(adversary_plan(Enemy, [])).

random_relocate_enemy(_).

/* Helper: Get random adjacent room (excluding locked doors) */
random_adjacent_room(Loc, Adjacent) :-
    findall(L, (adjacent(Loc, L), \+ door_locked(L)), Adjacents),
    Adjacents \= [],
    !,
    random_member(Adjacent, Adjacents).

random_adjacent_room(Loc, Loc).

/* Reveal all rooms (for old_map event) - marks all rooms as visited */
reveal_all_rooms :-
    forall(room(R),
           (room_visited(R) -> true ; assert(room_visited(R)))).

execute_all_adversary_turns :-
    /* Clear previous movement paths */
    retractall(enemy_move_path(_, _)),
    get_adversary_moves(GuardMoves, ThiefMoves),
    /* Guard only moves if discovered - record starting position */
    (guard_discovered ->
        (adversary_at(guard, GuardStart),
         assert(enemy_move_path(guard, [GuardStart])),
         execute_adversary_n_times_with_path(guard, GuardMoves))
    ; true),
    /* Thief always moves - actively hunting for treasure or chasing player */
    (adversary_at(thief, ThiefStart) ->
        (assert(enemy_move_path(thief, [ThiefStart])),
         execute_adversary_n_times_with_path(thief, ThiefMoves))
    ; true),
    report_adversary_movement.

/* Get number of moves based on difficulty */
get_adversary_moves(GuardMoves, ThiefMoves) :-
    get_difficulty(hard),
    !,
    GuardMoves = 3,   % Guard moves up to 3 steps per turn in hard mode (actual steps = min(3, distance to target))
    ThiefMoves = 3.   % Thief moves up to 3 steps per turn in hard mode (actual steps = min(3, distance to target))

get_adversary_moves(GuardMoves, ThiefMoves) :-
    get_difficulty(easy),
    !,
    /* Easy mode: 50% chance enemies skip their turn */
    (random(R1), R1 < 0.5 -> GuardMoves = 0 ; GuardMoves = 1),
    (random(R2), R2 < 0.5 -> ThiefMoves = 0 ; ThiefMoves = 1).

get_adversary_moves(1, 1).  % Normal: up to 1 step per turn (actual steps = min(1, distance to target))

/* Execute adversary turn N times WITH path recording */
execute_adversary_n_times_with_path(_, 0) :- !.
execute_adversary_n_times_with_path(_, _) :- game_over, !.
execute_adversary_n_times_with_path(Adversary, N) :-
    N > 0,
    /* Execute turn and record new position */
    ignore(execute_adversary_turn(Adversary)),
    /* Record current position to path */
    (adversary_at(Adversary, NewLoc) ->
        (retract(enemy_move_path(Adversary, OldPath)),
         append(OldPath, [NewLoc], NewPath),
         assert(enemy_move_path(Adversary, NewPath)))
    ; true),
    N1 is N - 1,
    execute_adversary_n_times_with_path(Adversary, N1).

/* Execute adversary turn N times (legacy, no path recording) */
execute_adversary_n_times(_, 0) :- !.
execute_adversary_n_times(_, _) :- game_over, !.
execute_adversary_n_times(Adversary, N) :-
    N > 0,
    /* Use ignore/1 to ensure we continue even if execute_adversary_turn fails */
    ignore(execute_adversary_turn(Adversary)),
    N1 is N - 1,
    execute_adversary_n_times(Adversary, N1).

/* Get enemy movement path as list of grid positions */
get_enemy_path_positions(Adversary, Positions) :-
    enemy_move_path(Adversary, Path),
    maplist(room_to_grid_pos, Path, Positions),
    !.
get_enemy_path_positions(_, []).

/* Convert room name to grid position [Row, Col] */
room_to_grid_pos(Room, [Row, Col]) :-
    grid_pos(Row, Col, Room), !.
room_to_grid_pos(_, [-1, -1]).  % Not found

report_adversary_movement :-
    i_am_at(PlayerLoc),
    (adversary_at(guard, GuardLoc), adjacent(PlayerLoc, GuardLoc) ->
        write('You hear heavy footsteps nearby...'), nl
    ; true),
    (adversary_at(thief, ThiefLoc), adjacent(PlayerLoc, ThiefLoc) ->
        write('You sense someone lurking in the shadows...'), nl
    ; true).

/* ============================================================================
   WIN AND LOSE CONDITIONS
   ============================================================================ */

check_game_state :-
    game_over,
    !.

check_game_state :-
    check_win,
    !.

check_game_state :-
    check_lose,
    !.

check_game_state.

/* Win condition: Have treasure and reach exit (door must be unlocked) */
check_win :-
    i_am_at(exit_hall),
    holding(gold_treasure),
    /* Verify door is unlocked - player cannot reach exit_hall if locked, but check for consistency */
    \+ door_locked(exit_hall),
    !,
    assert(game_over),
    assert(game_won),
    nl,
    write('*******************************************'), nl,
    write('*         CONGRATULATIONS!                *'), nl,
    write('*   You escaped with the treasure!        *'), nl,
    write('*******************************************'), nl,
    turn_count(T),
    player_health(H),
    calculate_score(T, H, Score),
    format('~nFinal Score: ~w~n', [Score]),
    format('Turns taken: ~w~n', [T]),
    format('Health remaining: ~w~n', [H]),
    nl,
    finish.

/* Lose conditions */
check_lose :-
    player_health(H),
    H =< 0,
    !,
    player_dies.

check_lose :-
    /* Thief escapes with treasure */
    treasure_stolen,
    adversary_at(thief, exit_hall),
    !,
    assert(game_over),
    nl,
    write('*******************************************'), nl,
    write('*            GAME OVER!                   *'), nl,
    write('*   The thief escaped with the treasure!  *'), nl,
    write('*******************************************'), nl,
    nl,
    finish.

check_lose :-
    /* Time limit - optional harder mode */
    turn_count(T),
    T > 100,
    !,
    assert(game_over),
    nl,
    write('*******************************************'), nl,
    write('*            GAME OVER!                   *'), nl,
    write('*   You took too long! Guards overwhelm   *'), nl,
    write('*   you with reinforcements.              *'), nl,
    write('*******************************************'), nl,
    nl,
    finish.

player_dies :-
    assert(game_over),
    nl,
    write('*******************************************'), nl,
    write('*            GAME OVER!                   *'), nl,
    write('*         You have been defeated!         *'), nl,
    write('*******************************************'), nl,
    nl,
    finish.

/* Score calculation using arithmetic */
calculate_score(Turns, Health, Score) :-
    BaseScore is 1000,
    TurnPenalty is Turns * 5,
    HealthBonus is Health * 2,
    (bonus_score(Bonus) -> true ; Bonus = 0),
    Score is max(0, BaseScore - TurnPenalty + HealthBonus + Bonus).

/* ============================================================================
   GAME CONTROL
   ============================================================================ */

finish :-
    nl,
    write('The game is over. Enter "start." to play again or "halt." to quit.'),
    nl.

die :- player_dies.

/* Instructions */
instructions :-
    nl,
    write('==============================================================================='), nl,
    write('                           DUNGEON ESCAPE'), nl,
    write('                      A Text Adventure Game'), nl,
    write('==============================================================================='), nl,
    nl,
    write('[ STORY ]'), nl,
    write('  You are a brave adventurer trapped in a dark dungeon.'), nl,
    write('  Your goal is to find the golden treasure and escape through the exit.'), nl,
    write('  But beware! A guard patrols the dungeon trying to capture you,'), nl,
    write('  and a thief lurks in the shadows trying to steal the treasure.'), nl,
    nl,
    write('[ WIN CONDITION ]'), nl,
    write('  - Find and pick up the golden treasure'), nl,
    write('  - Find the key and unlock the exit door'), nl,
    write('  - Reach the exit hall with the treasure'), nl,
    nl,
    write('[ LOSE CONDITIONS ]'), nl,
    write('  - Your health drops to 0'), nl,
    write('  - The thief escapes with the treasure'), nl,
    write('  - You exceed 100 turns'), nl,
    nl,
    write('[ MAP ]'), nl,
    write('  The dungeon layout is shrouded in darkness.'), nl,
    write('  Use the "map." command to see rooms you have explored.'), nl,
    write('  Unexplored areas are marked as "???"'), nl,
    nl,
    write('[ ITEMS ]'), nl,
    write('  torch         - Light source, reveals traps'), nl,
    write('  sword         - Weapon for combat'), nl,
    write('  shield        - Reduces damage taken'), nl,
    write('  health_potion - Restores 30 HP'), nl,
    write('  ancient_book  - Contains hints about the dungeon'), nl,
    write('  rope          - Can tie up the thief'), nl,
    write('  key           - Unlocks the exit door'), nl,
    write('  gold_treasure - Your goal! Find and escape with it'), nl,
    write('  dagger        - A weaker weapon, better than nothing'), nl,
    write('  lantern       - Another light source'), nl,
    write('  food_ration   - Restores 15 HP'), nl,
    write('  map_scroll    - Reveals the entire dungeon layout'), nl,
    write('  bomb          - Deals heavy damage to enemies'), nl,
    write('  lockpick      - Attempt to pick the exit lock (may break)'), nl,
    write('  armor         - Reduces damage taken'), nl,
    write('  magic_ring    - Grants +50 gold and bonus score'), nl,
    nl,
    write('[ ENEMIES ]'), nl,
    write('  Guard: Patrols dungeon, becomes aggressive when you have treasure.'), nl,
    write('         Deals 25 damage (reduced by sword/shield).'), nl,
    write('  Thief: Tries to steal treasure. Can be caught with rope or take(thief).'), nl,
    nl,
    write('[ TRAPS ]'), nl,
    write('  - 2 traps are randomly placed each game'), nl,
    write('  - Deal 5-10 damage in darkness; torch reveals and avoids them'), nl,
    nl,
    write('[ COMMANDS ]'), nl,
    write('  start.           -- Start/restart the game'), nl,
    write('  n.  s.  e.  w.   -- Move North/South/East/West'), nl,
    write('  look.            -- Look around current room'), nl,
    write('  map.             -- Show explored areas of the dungeon'), nl,
    write('  take(Object).    -- Pick up an object (e.g., take(torch).)'), nl,
    write('  drop(Object).    -- Put down an object'), nl,
    write('  use(Object).     -- Use an object (e.g., use(key).)'), nl,
    write('  inventory. / i.  -- Check your inventory'), nl,
    write('  status.          -- Check game status and enemy locations'), nl,
    write('  attack.          -- Attack an enemy in the same room'), nl,
    write('  shop.            -- View items available for purchase'), nl,
    write('  buy(Item).       -- Buy an item from the shop'), nl,
    write('  sell(Item).      -- Sell a valuable item for gold'), nl,
    write('  instructions.    -- Show these instructions'), nl,
    write('  difficulty(X).   -- Set difficulty: easy, normal, hard'), nl,
    write('  halt.            -- Quit the game'), nl,
    nl,
    write('[ GOLD & SHOP ]'), nl,
    write('  - Earn gold by selling valuable items or using magic_ring'), nl,
    write('  - Shop items: health_potion(30g), torch(15g), rope(20g),'), nl,
    write('                food_ration(10g), bomb(50g), map_scroll(40g)'), nl,
    nl,
    write('[ DIFFICULTY LEVELS ]'), nl,
    write('  Easy:   150 HP, slower enemies, less trap/guard damage'), nl,
    write('  Normal: 100 HP, standard speed and damage'), nl,
    write('  Hard:   80 HP, faster enemies, more damage, combat penalties'), nl,
    nl,
    write('[ SCORING ]'), nl,
    write('  Score = 1000 - (Turns x 5) + (Health x 2)'), nl,
    write('  Fewer turns and more health = higher score!'), nl,
    nl,
    write('[ TIPS ]'), nl,
    write('  1. Pick up the torch first to avoid traps'), nl,
    write('  2. Explore the dungeon to find useful items'), nl,
    write('  3. Find the key to unlock the exit'), nl,
    write('  4. Get the treasure, unlock the exit, and escape!'), nl,
    nl,
    write('==============================================================================='), nl,
    nl.

/* Status command */
status :-
    player_health(H),
    turn_count(T),
    get_max_health(MaxH),
    (player_gold(G) -> true ; G = 0),
    TurnsLeft is 100 - T,
    nl,
    write('=== STATUS ==='), nl,
    format('Health: ~w/~w~n', [H, MaxH]),
    format('Gold: ~w~n', [G]),
    format('Turns Left: ~w~n', [TurnsLeft]),
    write('Inventory: '),
    findall(X, holding(X), Items),
    (Items = [] -> write('empty') ; write(Items)),
    nl,
    write('Adversaries:'), nl,
    report_known_adversaries,
    nl.

report_known_adversaries :-
    adversary_at(Adv, Loc),
    adversary_type(Adv, Type),
    /* Only show location if player is in same room or has visited that room */
    (i_am_at(Loc) ->
        format('  ~w (~w) - HERE WITH YOU!~n', [Adv, Type])
    ; room_visited(Loc) ->
        format('  ~w (~w) - last seen near: ~w~n', [Adv, Type, Loc])
    ;
        format('  ~w (~w) - location unknown~n', [Adv, Type])
    ),
    fail.

report_known_adversaries.

/* ============================================================================
   FOG OF WAR - MAP COMMAND (8x8 Grid)
   ============================================================================ */

/* Map command - show explored areas with fog of war (8x8 grid system) */
map :-
    nl,
    write('============== DUNGEON MAP (8x8 Grid) =============='), nl,
    write('  [Room] = Explored, [???] = Unexplored, [###] = Wall'), nl,
    write('  *Room* = You are here'), nl,
    nl,
    /* Display grid row by row (0-7) */
    print_map_grid,
    nl,
    /* Show exploration stats */
    findall(R, room_visited(R), Visited),
    length(Visited, VCount),
    format('  Explored: ~w/36 rooms~n', [VCount]),
    write('==============================================='), nl,
    nl.

/* Print the entire 8x8 grid map */
print_map_grid :-
    /* Print column headers */
    write('   '),
    forall(between(0, 7, C), format('  ~w ', [C])),
    nl,
    /* Print each row */
    forall(between(0, 7, R), print_map_row(R)).

/* Print a single row of the map */
print_map_row(R) :-
    format('~w ', [R]),
    forall(between(0, 7, C), print_map_cell(R, C)),
    nl.

/* Print a single cell of the map */
print_map_cell(R, C) :-
    (grid_pos(R, C, Room) ->
        (Room = wall ->
            write('[###]')
        ;
            print_room_fog(Room)
        )
    ;
        write('[???]')
    ).

/* Print room with fog of war - compact format for grid */
print_room_fog(Room) :-
    i_am_at(Room),
    !,
    room_short(Room, Short),
    format('[*~w*]', [Short]).

print_room_fog(Room) :-
    room_visited(Room),
    !,
    room_short(Room, Short),
    format('[~w]', [Short]).

print_room_fog(_) :-
    write('[???]').

/* Short room names for CLI display - compact 4-5 char format */
room_short(entrance, 'ENTR').
room_short(exit_hall, 'EXIT').
room_short(guard_room, 'GUARD').
room_short(treasure_room, 'TRES').
room_short(armory, 'ARM').
room_short(library, 'LIB').
room_short(prison_cell, 'PRI').
room_short(secret_passage, 'SEC').
room_short(storage, 'STOR').
room_short(dark_corridor, 'DARK').
room_short(kitchen, 'KITC').
room_short(well_room, 'WELL').
room_short(alchemy_lab, 'ALCH').
room_short(crypt, 'CRYP').
room_short(barracks, 'BARR').
room_short(chapel, 'CHAP').
room_short(dungeon_pit, 'PIT').
room_short(torture_chamber, 'TORT').
room_short(wine_cellar, 'WINE').
room_short(servants_quarters, 'SERV').
room_short(great_hall, 'GREAT').
room_short(garden, 'GARD').
room_short(observatory, 'OBSV').
room_short(vault, 'VAUL').
room_short(smithy, 'SMIT').
room_short(stable, 'STAB').
room_short(watchtower, 'TOWR').
room_short(marketplace, 'MARK').
room_short(bathhouse, 'BATH').
room_short(gallery, 'GALL').
room_short(throne_room, 'THRN').
room_short(hallway_n, 'H-N').
room_short(hallway_s, 'H-S').
room_short(hallway_e, 'H-E').
room_short(hallway_w, 'H-W').
room_short(crossing, 'CROS').
room_short(courtyard, 'CTYD').
room_short(wall, 'WALL').
room_short(_, '???').

/* Legacy connection helpers - kept for compatibility but not used in new grid map */
print_h_conn(Room1, Room2) :-
    (room_visited(Room1) ; i_am_at(Room1)),
    (room_visited(Room2) ; i_am_at(Room2)),
    !,
    write('-').

print_h_conn(_, _) :-
    write(' ').

print_v_conn_row(Top1, Bot1, Top2, Bot2, Top3, Bot3) :-
    write('  '),
    print_v_conn(Top1, Bot1),
    write('       '),
    print_v_conn(Top2, Bot2),
    write('       '),
    print_v_conn(Top3, Bot3),
    nl.

print_v_conn(Top, Bot) :-
    (room_visited(Top) ; i_am_at(Top)),
    (room_visited(Bot) ; i_am_at(Bot)),
    !,
    write('|').

print_v_conn(_, _) :-
    write(' ').

/* Difficulty command */
difficulty(Level) :-
    set_difficulty(Level),
    /* If game already started, suggest restart */
    (player_health(_) ->
        write('Note: Restart the game for difficulty to take full effect.'), nl
    ; true).

/* Start the game */
start :-
    init_game,
    nl,
    write('============================================'), nl,
    write('           DUNGEON ESCAPE'), nl,
    write('============================================'), nl,
    show_current_difficulty,
    nl,
    write('You wake up in a dark dungeon. The air is damp and cold.'), nl,
    write('Somewhere in these depths lies a golden treasure, your'), nl,
    write('only hope of buying your freedom. But you are not alone...'), nl,
    write('A guard patrols these halls, and a cunning thief also'), nl,
    write('seeks the treasure.'), nl,
    nl,
    write('Find the treasure, unlock the exit, and escape!'), nl,
    nl,
    instructions,
    look.

/* Show current difficulty at game start */
show_current_difficulty :-
    get_difficulty(Level),
    format('         [ Difficulty: ~w ]~n', [Level]).

/* ============================================================================
   ROOM DESCRIPTIONS
   ============================================================================ */

describe(entrance) :-
    write('You are at the dungeon entrance.'), nl,
    write('Stone walls surround you, covered in moss and ancient markings.'), nl,
    write('A faint light filters in from cracks above.'), nl.

describe(storage) :-
    write('You are in an old storage room.'), nl,
    write('Dusty crates and barrels fill the space.'), nl,
    write('Some containers look like they haven''t been opened in years.'), nl.

describe(guard_room) :-
    write('You are in the guard room.'), nl,
    write('This appears to be where the dungeon guard rests.'), nl,
    write('There are weapons on the walls and a table with old food.'), nl.

describe(treasure_room) :-
    write('You are in the treasure room!'), nl,
    write('Golden light seems to emanate from within.'), nl,
    (at(gold_treasure, treasure_room) ->
        write('A magnificent golden treasure sits on a pedestal!')
    ;
        write('The pedestal is empty... the treasure is gone!')
    ), nl.

describe(armory) :-
    write('You are in the armory.'), nl,
    write('Rusty weapons and broken armor line the walls.'), nl,
    (\+ trap_triggered(armory), \+ has_light ->
        write('It''s very dark here. You should be careful...')
    ; true), nl.

describe(library) :-
    write('You are in an ancient library.'), nl,
    write('Dusty books and scrolls fill the shelves.'), nl,
    write('Some tomes seem to glow with arcane energy.'), nl.

describe(prison_cell) :-
    write('You are in an old prison cell.'), nl,
    write('Iron bars and chains hang from the walls.'), nl,
    write('You shudder thinking about who was kept here.'), nl.

describe(secret_passage) :-
    write('You are in a secret passage!'), nl,
    write('The walls here are rougher, clearly carved in haste.'), nl,
    (\+ trap_triggered(secret_passage), \+ has_light ->
        write('Strange shadows dance in the darkness...')
    ; true), nl.

describe(exit_hall) :-
    write('You are in the exit hall!'), nl,
    write('Daylight streams in from an opening ahead.'), nl,
    write('Freedom is just steps away!'), nl.

/* New room descriptions */
describe(dark_corridor) :-
    write('You are in a dark corridor.'), nl,
    write('The darkness here is almost palpable.'), nl,
    (has_light ->
        write('Your light reveals ancient runes on the walls.')
    ;
        write('You can barely see anything without a light source.')
    ), nl.

describe(throne_room) :-
    write('You are in the throne room.'), nl,
    write('A crumbling throne sits on a raised platform.'), nl,
    write('Faded tapestries hang from the walls, depicting forgotten kings.'), nl.

describe(kitchen) :-
    write('You are in an old kitchen.'), nl,
    write('Rusty pots and pans hang from hooks.'), nl,
    write('The smell of ancient meals lingers in the air.'), nl.

describe(well_room) :-
    write('You are in the well room.'), nl,
    write('A deep stone well dominates the center of the room.'), nl,
    write('You hear water dripping far below.'), nl.

describe(alchemy_lab) :-
    write('You are in an alchemy laboratory.'), nl,
    write('Bubbling vials and strange apparatus cover the tables.'), nl,
    write('Colorful liquids glow with mysterious energy.'), nl.

describe(crypt) :-
    write('You are in a burial crypt.'), nl,
    write('Stone sarcophagi line the walls.'), nl,
    write('The air is cold and still.'), nl.

describe(barracks) :-
    write('You are in the guard barracks.'), nl,
    write('Rows of bunk beds fill the room.'), nl,
    write('Old uniforms and equipment are scattered about.'), nl.

describe(chapel) :-
    write('You are in a small chapel.'), nl,
    write('An altar stands at the far end, covered in dust.'), nl,
    write('Stained glass windows cast colored light.'), nl.

describe(dungeon_pit) :-
    write('You are at the edge of a deep pit.'), nl,
    write('The darkness below seems endless.'), nl,
    write('Chains dangle into the abyss.'), nl.

describe(torture_chamber) :-
    write('You are in a torture chamber.'), nl,
    write('Sinister devices of pain fill the room.'), nl,
    write('You shudder at the thought of what happened here.'), nl.

describe(wine_cellar) :-
    write('You are in a wine cellar.'), nl,
    write('Rows of dusty bottles line wooden racks.'), nl,
    write('Some bottles look centuries old.'), nl.

describe(servants_quarters) :-
    write('You are in the servants quarters.'), nl,
    write('Simple beds and belongings fill the cramped space.'), nl,
    write('Remnants of daily life remain frozen in time.'), nl.

describe(great_hall) :-
    write('You are in the great hall.'), nl,
    write('A massive chandelier hangs from the vaulted ceiling.'), nl,
    write('Long tables stretch across the room.'), nl.

describe(garden) :-
    write('You are in an underground garden.'), nl,
    write('Bioluminescent plants provide an eerie glow.'), nl,
    write('Strange mushrooms grow in clusters.'), nl.

describe(observatory) :-
    write('You are in the observatory.'), nl,
    write('An ancient telescope points toward a shaft leading to the sky.'), nl,
    write('Star charts cover the walls.'), nl.

describe(vault) :-
    write('You are in a secure vault.'), nl,
    write('Heavy iron doors guard this room.'), nl,
    write('Valuable items glitter in the dim light.'), nl.

describe(smithy) :-
    write('You are in the smithy.'), nl,
    write('A cold forge sits in the center.'), nl,
    write('Weapons and tools hang from the walls.'), nl.

describe(stable) :-
    write('You are in old stables.'), nl,
    write('Empty stalls line the walls.'), nl,
    write('The smell of hay still lingers.'), nl.

describe(watchtower) :-
    write('You are in a watchtower.'), nl,
    write('Arrow slits provide views of the corridors.'), nl,
    write('A guard post stands abandoned.'), nl.

describe(marketplace) :-
    write('You are in an underground marketplace!'), nl,
    write('Abandoned stalls line the walls.'), nl,
    write('A mysterious merchant has set up shop here.'), nl,
    write('Type "shop." to see what is for sale!'), nl.

describe(bathhouse) :-
    write('You are in the bathhouse.'), nl,
    write('Marble pools lie empty and cracked.'), nl,
    write('Faded mosaics decorate the walls.'), nl.

describe(gallery) :-
    write('You are in an art gallery.'), nl,
    write('Dusty paintings and sculptures fill the room.'), nl,
    write('The eyes in the portraits seem to follow you.'), nl.

/* Fallback for any undefined room */
describe(_) :-
    write('You are in an unknown area.'), nl,
    write('The surroundings are unfamiliar.'), nl.

/* ============================================================================
   SECRET COMMANDS AND EASTER EGGS
   ============================================================================ */

/* Secret command: plugh - another classic */
plugh :-
    nl,
    write('Nothing happens... or does it?'), nl,
    random(R),
    (R < 0.2 ->
        /* 20% chance to reveal enemy locations */
        (write('You sense the presence of others...'), nl,
         adversary_at(guard, GLoc),
         adversary_at(thief, TLoc),
         /* Mark these rooms as visited so they show on map */
         (room_visited(GLoc) -> true ; assert(room_visited(GLoc))),
         (room_visited(TLoc) -> true ; assert(room_visited(TLoc))),
         format('Guard is in ~w. Thief is in ~w.~n', [GLoc, TLoc]))
    ; true),
    nl.

/* Secret command: pray - ask for divine help */
pray :-
    nl,
    write('You kneel and pray to the dungeon spirits...'), nl,
    random_between(1, 5, Result),
    pray_result(Result),
    nl.

pray_result(1) :-
    write('A warm light surrounds you! Health restored!'), nl,
    get_max_health(MaxH),
    retractall(player_health(_)),
    assert(player_health(MaxH)).

pray_result(2) :-
    write('The spirits whisper the location of treasure...'), nl,
    (at(gold_treasure, Loc) ->
        (format('The treasure is in ~w!~n', [Loc]),
         /* Mark this room as visited so it shows on map */
         (room_visited(Loc) -> true ; assert(room_visited(Loc))))
    ;
        write('Someone already has the treasure!')
    ).

pray_result(3) :-
    write('A protective aura surrounds you briefly.'), nl,
    (bonus_score(BS) ->
        (retract(bonus_score(_)), NewBS is BS + 50)
    ;
        NewBS = 50
    ),
    assert(bonus_score(NewBS)),
    write('You feel blessed! (+50 bonus points)'), nl.

pray_result(4) :-
    write('The spirits are silent... perhaps try again later.'), nl.

pray_result(5) :-
    write('You anger the spirits!'), nl,
    player_health(H),
    NewH is max(1, H - 10),
    retractall(player_health(_)),
    assert(player_health(NewH)),
    format('You feel weak... Health: ~w~n', [NewH]).

/* Secret command: meditate - calm mind bonus */
meditate :-
    nl,
    write('You sit down and meditate...'), nl,
    write('Your mind becomes clear and focused.'), nl,
    (bonus_score(BS) ->
        (retract(bonus_score(_)), NewBS is BS + 25)
    ;
        NewBS = 25
    ),
    assert(bonus_score(NewBS)),
    write('You feel at peace. (+25 bonus points)'), nl,
    nl.

/* ============================================================================
   ENEMY DIALOGUE SYSTEM
   ============================================================================ */

/* Guard dialogue */
guard_dialogue(combat) :-
    random_member(Line, [
        'Guard: "You dare trespass in MY dungeon?!"',
        'Guard: "Intruder! Prepare to meet your end!"',
        'Guard: "No one escapes on my watch!"',
        'Guard: "I''ve been waiting for this!"',
        'Guard: "You''ll regret coming here!"'
    ]),
    write(Line), nl.

guard_dialogue(capture) :-
    random_member(Line, [
        'Guard: "Ha! Too easy. You''re no match for me!"',
        'Guard: "Thought you could sneak past? Think again!"',
        'Guard: "Another prisoner for the cells!"',
        'Guard: "You should have stayed hidden, fool!"'
    ]),
    write(Line), nl.

guard_dialogue(retreat) :-
    random_member(Line, [
        'Guard: "This isn''t over! I''ll be back!"',
        'Guard: "You got lucky this time..."',
        'Guard: "Argh! I need reinforcements!"'
    ]),
    write(Line), nl.

/* Thief dialogue */
thief_dialogue(steal) :-
    random_member(Line, [
        'Thief: "Hehe, thanks for leading me to the treasure!"',
        'Thief: "Too slow! This gold is mine now!"',
        'Thief: "Better luck next time, adventurer!"',
        'Thief: "Catch me if you can! Hahaha!"'
    ]),
    write(Line), nl.

thief_dialogue(caught) :-
    random_member(Line, [
        'Thief: "No! Let me go! I''ll share the gold!"',
        'Thief: "Curses! You''re faster than you look!"',
        'Thief: "Okay okay, you win! Don''t hurt me!"',
        'Thief: "This isn''t fair! I saw it first!"'
    ]),
    write(Line), nl.

thief_dialogue(taunt) :-
    random_member(Line, [
        'Thief: "You''ll never catch me!"',
        'Thief: "I''m like a shadow... you can''t touch shadows!"',
        'Thief: "Still looking for the treasure? I know where it is!"'
    ]),
    write(Line), nl.

/* Random enemy taunts during gameplay */
maybe_enemy_taunt :-
    random(R),
    R < 0.1,  % 10% chance
    !,
    (adversary_at(thief, ThiefLoc), i_am_at(PlayerLoc), adjacent(ThiefLoc, PlayerLoc) ->
        (nl, thief_dialogue(taunt))
    ; true).

maybe_enemy_taunt.

holding_weapon :-
    holding(sword).
holding_weapon :-
    holding(dagger).

holding_defense :-
    holding(shield).
