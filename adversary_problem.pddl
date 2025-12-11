(define (problem guard_pursuit)
    ;;; Default problem file for guard adversary
    ;;; This file is dynamically regenerated during gameplay
    ;;; Initial state: guard at guard_room, goal: reach player location

    (:domain dungeon)

    (:objects
        guard - adversary
        entrance - location
        corridor_north - location
        corridor_south - location
        guard_room - location
        treasure_room - location
        armory - location
        library - location
        prison_cell - location
        secret_passage - location
        exit_hall - location
    )

    (:init
        ;; Guard starts in guard_room
        (at guard guard_room)

        ;; Room connections (bidirectional)
        (connected entrance corridor_north)
        (connected corridor_north entrance)
        (connected entrance corridor_south)
        (connected corridor_south entrance)
        (connected corridor_north guard_room)
        (connected guard_room corridor_north)
        (connected corridor_north library)
        (connected library corridor_north)
        (connected corridor_south armory)
        (connected armory corridor_south)
        (connected corridor_south prison_cell)
        (connected prison_cell corridor_south)
        (connected guard_room treasure_room)
        (connected treasure_room guard_room)
        (connected library secret_passage)
        (connected secret_passage library)
        (connected secret_passage treasure_room)
        (connected treasure_room secret_passage)
        (connected treasure_room exit_hall)
        (connected exit_hall treasure_room)
    )

    (:goal
        ;; Default goal: guard moves to entrance
        (at guard entrance)
    )
)
