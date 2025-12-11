(define (domain dungeon)
    ;;; Domain for adversary movement in the dungeon
    ;;; Used by Pyperplan (STRIPS planner)
    ;;; Tool: Pyperplan
    ;;; PDDL Version: STRIPS subset of PDDL 1.2
    ;;; No negative preconditions, no ADL features

    (:requirements :strips :typing)

    (:types
        adversary location - object
    )

    (:predicates
        (at ?a - adversary ?l - location)
        (connected ?l1 - location ?l2 - location)
    )

    ;;; Move adversary from one location to an adjacent location
    (:action move
        :parameters (?a - adversary ?from - location ?to - location)
        :precondition (and
            (at ?a ?from)
            (connected ?from ?to)
        )
        :effect (and
            (not (at ?a ?from))
            (at ?a ?to)
        )
    )
)
