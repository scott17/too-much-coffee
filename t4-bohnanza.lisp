;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; t4-bohnanza.lisp - default (rather stupid) Bohnanza player for 
;; CMSC471 project.  Does everything blindly but does at least buy
;; a third field when it's eligible.  Knows how to trade!!
;; 
;; This player hates the first player in the game and always
;; gives that player half the score of other players.
;; (Designed solely to test the "differentiate between players"
;; capability of the trading mechanism.)
;;
;; (c) Marie desJardins 2014
;; VERSION 2 - BETA VERSION - Distributed 4/18/14
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
;;     May optionally call trade() in the API with a set of proposed trades
;;
;; evaluate-trades (player trades)
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

(defpackage :t4)
(in-package :t4)


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
	  user::trade-from-player user::trade-from-card user::trade-from-pos
	  user::trade-from-score
	  user::trade-to-player user::trade-to-card user::trade-to-pos
	  user::trade-to-score user::trade-info
	  user::plant user::harvest user::bean-fits
	  user::legal-fields-to-harvest user::plant-card-at-front
	  user::harvest-rate user::buy-third-bean-field
	  user::is-singleton? user::is-multiple? user::is-empty?
	  user::is-planted? user::trade user::viable-trade))
	  
	  

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
(defun handle-face-up-cards (player game &aux card)
  ;; Request and execute any trades
  (trade (generate-trades player) player)
  ;; Plant whatever's left in the faceup cards
  (loop while (setf card (pop (player-faceup player)))
	do (plant-card player card game)))


(defun generate-trades (player &aux (trades nil) desired-cards)
  (setf desired-cards
	(remove-if #'null (mapcar #'car (player-fields player))))
  ;; Trade away faceup cards if they can't be planted immediately
  (loop for card in (player-faceup player)
	do (if (not (member card desired-cards))
	       (setf trades (append trades
				    (make-new-trades 
				     player 'player-faceup card
				     desired-cards 1)))))
  ;; Trade away cards in hand if they can't be planted
  ;; immediately, with less urgency as the cards are farther
  ;; back in the deck
  (loop for i from 0 to (length (player-hand player))
	do (if (not (member (nth i (player-hand player)) desired-cards))
	       (setf trades (append trades 
				    (make-new-trades 
				     player i (nth i (player-hand player))
				     desired-cards (/ 1 (+ 1 i)))))))
  trades
  )

(defun make-new-trades (player loc bad goods value)
  (loop for card in goods
	collect (make-instance 'trade :from-player player
				      :from-card bad
				      :from-pos loc
				      :from-score (set-scores value)
				      :to-card card
				      :info nil)))


;; This function cuts the score in half for the first player
;; in *GAME*

(defun set-scores (value)
  (let ((scores (mapcar #'(lambda (p) (cons p value))
			(game-players user::*GAME*))))
    (setf (cdar scores) (/ (cdar scores) 2.0))))


;; This function takes the player object and a list of
;; proposed trades (objects of class "TRADE") and scores
;; each of the trades, pushing a list
;;    (PLAYER SCORE POSITION)
;; on the front of the TRADE-INFO slot for each trade offered.
;; 
;; Score: 1 means a fully acceptable trade; 0 means a nonviable or
;; unacceptable trade; scores in between are interpreted as
;; strength of preference for the given trade.  (POSITION will
;; be NIL for an accepted donation.)
;;
;; Where: always 'user::player-hand since the non-active player
;; can't trade faceup cards
;;
;; Position:  0-indexed location in hand
;;
;; This function may call the API function
;;    viable-trade (player trade)
;; which returns either NIL or a list of positions in the
;; player's hand where the requested card can be found
;;
;; (Note: viable-trades() will not work for requests to donate
;; a card, so that case should be handled separately!!)
;;
;; This foolish version just accepts every trade with value 1,
;; choosing the earliest card in its hand to trade away regardless
;; of its value to the player.
;; Except that this is player t4, who hates the first player
;; in the game, so any trade offered by that player receives
;; score 0

(defun evaluate-trades (player trades)
  (loop for trade in trades
	do (if (eq (trade-from-player trade) (car (game-players 
						   user::*game*)))
	       (push (list player 0 nil) (trade-info trade))
	     (if (trade-to-card trade)
		 ;; FROM is requesting a card to trade:
		 ;; say yes if this player has it at all
		 (let ((viable (viable-trade player trade)))
		   (if viable
		       (push (list player 1 (car viable))
			     (trade-info trade))
		     (push (list player 0 nil) (trade-info trade))))
	       ;; FROM is requesting to donate a card:
	       ;; always accept
	       (push (list player 1 nil) (trade-info trade))))
	))


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


