;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; p1-bohnanza.lisp - default (rather stupid) Bohnanza player for 
;; CMSC471 project.  Does everything blindly but does at least buy
;; a third field when it's eligible.
;;
;; (c) Marie desJardins 2014
;; VERSION 1 - ALPHA VERSION - Distributed 3/2/14
;;
;; EVERY player file MUST define the following functions:
;;
;; plant-card (player card game)
;;     where player is your PLAYER structure, card is the name
;;     of a bean to be planted, and game is the current game structure
;;
;; optionally-plant-card (player game)
;;
;; handle-face-up-cards (player game)
;;
;; NOTE: buy-third-bean-field (player game) may be called from any
;;   of these functions (if you have three coins to spend...)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PACKAGE DEFINITION
;; You MUST include these two lines, with YOUR OWN TEAM NAME
;; for the defined package.  All of your code "lives" in this
;; package, and it must be the same as the team name that you
;; will use for the tournament!!

(defpackage :p1)
(in-package :p1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; IMPORTS - symbols (function names) to import from the main
;; game engine package.  Add imports of utility functions here
;; if you choose to use any.
;;

(import '(user::BeanTypes 
	  user::BeanConversion
	  user::game-deck-stats user::game-discards user::game-discard-stats 
	  user::game-coin-stats user::game-players user::game-rounds 
	  user::game-shuffles
	  user::player-name user::player-hand user::player-faceup
	  user::player-numfields user::player-fields user::player-coins
	  user::player-coin-stack
	  user::plant user::harvest user::bean-fits
	  user::legal-fields-to-harvest user::plant-card-at-front
	  user::harvest-rate user::buy-third-bean-field
	  user::is-singleton? user::is-multiple? user::is-empty?
	  user::is-planted?))
	  
	  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; REQUIRED FUNCTIONS:
;; EVERY player MUST define plant-card(), optionally-plant-second-card(),
;; and handle-face-up-cards().

;; Plant a card (which would have come either from the front
;; of the hand or from the table).  This function is ALWAYS called
;; on the first card in your hand at the beginning of your turn.
;; You MAY also call it from optionally-plant-card() and from
;; handle-face-up-cards().  This function MUST call API function
;; plant() as follows:
;;     (plant card player N)
;; where "card" and "player" are as they were passed in, and "N"
;; is the number of a field (0,1,2) in which "card" can LEGALLY be planted.
;; (I.e., field N must be empty OR contain only beans of type "card"
;; This function may call API function harvest() to clear some field
;; (or may call another helper function you define, which in turn
;; calls harvest, as I've done).
(defun plant-card (player card game)
  ;; Try to buy a third bean field
  (if (and (> (player-coins player) 3) (< (player-numfields player) 3))
      (buy-third-bean-field player game))
  (cond ((null card)) ;; no card --> nothing to do
	;; plant in field 1 if it fits
	((bean-fits card (first (player-fields player)))
	 (plant card player 0))
	;; plant in field 2 if it fits
	((bean-fits card (second (player-fields player)))
	 (plant card player 1))
	;; plant in field 3 if bought and card fits
	((and (eq (player-numfields player) 3)
	      (bean-fits card (third (player-fields player))))
	 (plant card player 2))
	;; doesn't fit anywhere -- harvest something!
	((let ((now-empty (harvest-some-field player game)))
	   (if (nth now-empty (player-fields player))
	       (error "Player ~s field ~s should be empty but is ~s~%"
		      (player-name player) now-empty
		      (nth now-empty (player-fields player)))
	     (plant card player now-empty))))
	))

;; This function MAY choose to call the API function:
;;    (plant-card-at-front player game)
;; which will in turn call your plant-card() function on the card
;; at the FRONT of your hand.  (This function will be called after
;; the first card has already been AUTOMATICALLY planted, but only
;; if there is a second card to plant.)
;;
;; My stupid version never plants a second card.
(defun optionally-plant-card (player game)
  )


;; This function MUST plant or trade all cards that are in your
;; (player-faceup player) slot.  (In the first tournament, there
;; is no trading, so the cards MUST be planted using plant-card(),
;; though you may plant them in either order, and may call
;; harvest() as desired to clear space for them.
;; 
;; My version is stupid - it just plants them in order, without
;; bothering to clear any space for them.
(defun handle-face-up-cards (player game)
  (plant-card player (pop (player-faceup player)) game)
  (plant-card player (pop (player-faceup player)) game))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; UTILITY FUNCTIONS - Other functions I wrote to support my
;; players.  Your additional code should go here.

;; Find some random field to harvest.  Behaves stupidly.
;; Returns the number (0,1,2) of the field that is now empty.
(defun harvest-some-field (player game)
  (let ((which (legal-fields-to-harvest (player-fields player))))
    (cond ((not which)
	   (error "Player ~s has no legal fields to harvest?! Fields are ~s~%"
		  (player-name player) (player-fields player)))
	  (t 
	   ;; dumb strategy - just harvest the leftmost legal field
	   (harvest player (car which) game)))
    ;; Return the field that is now empty
    (car which)))

