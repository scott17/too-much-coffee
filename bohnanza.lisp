;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; bohnanza.lisp - main game engine for CMSC 471 project
;; (c) Marie desJardins 2014
;; VERSION 1 - ALPHA VERSION - Distributed 3/2/14
;;
;; In the tournament, the game will be played with 3 players
;; Deviations from standard rules:
;;  - Cocoa beans are removed from the game
;;  - Players start with 2 bean fields and may buy a 3rd for 3 coins
;;  - The game ends when the deck is exhausted for the third timexs


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LISP CONFIGURATION
;; This prevents Lisp from complaining when the file is reloaded

(setf *SUPPRESS-SIMILAR-CONSTANT-REDEFINITION-WARNING* t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; CONSTANT DEFINITIONS
;;

;; Will be set to current game; useful for debugging and post-game
;; analysis
(defvar *GAME* nil)

;; Association list used to specify how many beans of each
;; type are in the game
(defconstant BeanTypes '((coffee . 24) (wax . 22) (blue . 20)
			 (chili . 18) (stink . 16) (green . 14)
			 (soy . 12) (blackeyed . 10) (red . 8)
			 (garden . 6)))
;; Removed:  (cocoa . 4)))


;; Specifies how many of each type of bean are needed when
;; harvesting to earn 1, 2, 3, or 4 coins, respectively.
;; NIL means that number of coins cannot be earned.
(defconstant BeanConversion
  '((coffee 4 7 10 12)
    (wax 4 7 9 11)
    (blue 4 6 8 10)
    (chili 3 6 8 9)
    (stink 3 5 7 8)
    (green 3 5 6 7)
    (soy 2 4 6 7)
    (blackeyed 2 4 5 6)
    (red 2 3 4 5)
    (garden nil 2 3 nil)))
;; Removed:  (cocoa nil 2 3 4))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CLASS DEFINITIONS
;;
;; Data marked as PRIVATE is off-limits to players -- if your
;;   code accesses this data structure, your player will be 
;;   disqualified from the tournament.
;; Data marked as SELF is accessible only to that player --
;;   if your code accesses SELF data for another player, your
;;   player will be disqualified from the tournament.
;; Data marked as PUBLIC is accessible to any player at any time.
;; ALL fields in the GAME and ALL PLAYER data structures, including
;;   your own player, is to be treated as READ ONLY -- you should
;;   not directly change any values of any of these fields; you
;;   may only call the specified functions in the API at the 
;;   appropriate times.  If your code edits any field in any GAME
;;   or PLAYER data structure, your player will be disqualified
;;   from the tournament.


;; GAME -- basic data structures for playing a game

(defclass game ()
  (
   ;; PRIVATE - deck of cards, next card to be dealt is car()
   (deck :accessor game-deck :initarg :deck)

   ;; PUBLIC - summary of cards that are currently in the deck 
   ;;  (updated each time a card is dealt or the cards are shuffled)
   (deck-stats :accessor game-deck-stats :initarg :deck-stats)

   ;; PUBLIC - discard stack, anybody can inspect this if they want
   (discards :accessor game-discards :initarg :discards)

   ;; PUBLIC - summary of cards that are currently in the discard pile
   ;;  (updated each time a card is discarded, a bean field is purchased,
   ;;  or the cards are shuffled)
   (discard-stats :accessor game-discard-stats :initarg :discard-stats)

   ;; PUBLIC - summary of cards that have been converted to coins
   ;;  (updated each time a field is harvested for coins, or coins
   ;;  are spent to buy a third bean field and returned to the 
   ;;  discard pile)
   (coin-stats :accessor game-coin-stats :initarg :coin-stats)

   ;; PUBLIC - list of players (each is an instance of type PLAYER
   (players :accessor game-players :initarg :players)

   ;; PUBLIC - how many rounds (each consisting of one turn per player)
   ;; have been played so far
   (rounds :accessor game-rounds :initform 0 :initarg :rounds)

   ;; PUBLIC - how many times the deck has been shuffled (including
   ;; the initial shuffle).  The game ends when the game engine
   ;; attempts to deal a card, but the deck has already been shuffled
   ;; three times.
   (shuffles :accessor game-shuffles :initform 1 :initarg :shuffles)
   )
  )


;; PLAYER - data associated with a given player

(defclass player ()
  (
   ;; PUBLIC - player's name, must be the name of their package
   (name :accessor player-name :initarg :name)

   ;; SELF - stack of cards in order - car() is the next card
   ;; to plant.  Other players may not view this list, though
   ;; they may see the size of the hand (number of cards)
   (hand :accessor player-hand :initform nil :initarg :hand)

   ;; PUBLIC - the current unplanted cards on the table, if any
   (faceup :accessor player-faceup :initform nil :initarg :faceup)

   ;; PUBLIC - how many fields the player owns (always either 2 or 3)
   (numfields :accessor player-numfields :initform 2 :initarg :numfields)

   ;; PUBLIC - what is planted in each field
   (fields :accessor player-fields :initform (list nil nil nil) 
				   :initarg fields)

   ;; PUBLIC - how many coins this player has
   (coins :accessor player-coins :initform 0 :initarg coins)

   ;; PUBLIC - which actual beans are in the player's coin stack (CAR 
   ;; is most recently earned -> next to be paid out for 3rd bean field)
   (coin-stack :accessor player-coin-stack :initform nil :initarg coin-stack)
   )
  )


;; Calls a specified function in a player's package
(defmacro player-fn (p fn)
  `(symbol-function (intern ,fn (player-name ,p))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GAME ENGINE FUNCTIONS
;;
;; Functions to be called ONLY by the tournament manager
;; (Of course, you can use any of these functions for testing,
;; but your submitted code MUST NOT call any of these functions,
;; or you will be disqualified from the tournament.

(defun play (&optional (players '(p1 p2)))
  (let ((game (make-instance 'game
			     :deck (generate-and-shuffle)
			     :deck-stats (copy-tree BeanTypes)
			     :discards nil
			     :discard-stats (empty-bean-stats)
			     :coin-stats (empty-bean-stats)
			     :players (mapcar #'make-player players)
			     :rounds 0
			     )))

    (setf *game* game)

    (format t "Here's the initial deck: ~%~s~%" (game-deck game))

    (format t "Initial deal:  five cards per player~%")
    (loop for p in (game-players game)
	  do (loop for i from 1 to 5 do (deal-to-player p game 'hand)))
    (print-round-summary game)

    (loop named gameloop
	  while (game-deck game)
	  do (loop for p in (game-players game)
		   do
		   (progn
		     (if (player-hand p)
			 (plant-card-at-front p game))
		     (if (player-hand p)
			 (funcall (player-fn p "OPTIONALLY-PLANT-CARD") p game))
		     (if (not (deal-to-player p game 'faceup))
			 (return-from gameloop))
		     (if (not (deal-to-player p game 'faceup))
			 (return-from gameloop))
		     (funcall (player-fn p "HANDLE-FACE-UP-CARDS") p game)
		     (if (not (deal-to-player p game 'hand))
			 (return-from gameloop))
		     (if (not (deal-to-player p game 'hand))
			 (return-from gameloop))
		     (if (not (deal-to-player p game 'hand))
			 (return-from gameloop))
		     ))
	  (print-round-summary game))

    ;; Harvest all remaining fields
    (loop for p in (game-players game)
	  do (loop for n in '(0 1 2)
		   do (harvest p n game)))

    (print-game-summary game)))


(defun print-game-summary (game)
  (format t "~%END OF GAME!~%~%")
  (format t "Final deck: ~s~%" (game-deck game))
  (format t "Deck stats: ~s~%" (game-deck-stats game))
  (format t "Discard stats: ~s~%" (game-discard-stats game))
  (format t "Coin stats: ~s~%" (game-coin-stats game))
  (loop for p in (game-players game)
	do (progn
	     (format t "Player ~s has ~s coins~%" 
		     (player-name p) (player-coins p))
	     (format t "  Player ~s's fields: ~s~%" 
		     (player-name p) (player-fields p))
	     (format t "  Player ~s's hand: ~s~%"
		     (player-name p) (player-hand p))))
  )


;; Create a player with the given name
(defun make-player (player)
  (make-instance 'player :name player))


;; Deal one card to a player's hand and update deck statistics
;; Reshuffles discard pile if deck is empty and the deck has
;; been shuffled fewer than three times.  Returns NIL if there
;; are still no cards to deal (game is over!)
;; WHERE should be either 'hand or 'faceup
(defun deal-to-player (player game where)
  ;; See if we need to shuffle the discard pile.
  (cond ((and (not (game-deck game)) (>= (game-shuffles game) 3))
	 (return nil))
	((not (game-deck game))
	 (setf (game-deck game) (randomize (game-discards game)))
	 (setf (game-deck-stats game) (game-discard-stats game))
	 (setf (game-discard-stats game) (empty-bean-stats))))

  ;; If the deck is still empty, then the discard pile was empty,
  ;; and so we simply end the game -- this would be very surprising,
  ;; so we also print a surprised message.
  (when (not (game-deck game)) 
    (format t "This is surprising: the discard pile was empty when I tried to shuffle it!~%")
    (return nil))

  ;; If we get here, there are cards, so we deal one.
  (let ((card (pop (game-deck game))))
    (format t "Dealing card ~s to player ~s's ~s~%" 
	    card (player-name player) where)
    (cond ((eq where 'hand)
	   (setf (player-hand player) 
		 (append (player-hand player) (list card))))
	  ((eq where 'faceup)
	   (setf (player-faceup player)
		 (push card (player-faceup player))))
	  (t (error "Don't know how to deal to ~s in DEAL-TO-PLAYER~%"
		    where)))
    ;; Update statistics of deck
    (decf (cdr (assoc card (game-deck-stats game)))))
  )


;; Generate a new deck of cards (using BeanTypes) and shuffle it
(defun generate-and-shuffle ()
  (randomize (mapcan #'(lambda (beans) 
			 (generate-n-cards (car beans) (cdr beans)))
		     BeanTypes)))


;; Print a summary of the game at the end of a round
(defun print-round-summary (game)
  (format t "~%~%END OF ROUND ~s~%" (game-rounds game))
  (loop for p in (game-players game)
	do (progn
	     (format t "Player ~s: coins ~s, hand ~s, fields ~s~%~%"
		     (player-name p) (player-coins p) (player-hand p)
		     (player-fields p))))
  (format t "~%"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; API -- functions that players are intended to use in their code
;; to manage and inspect their fields


;; Plant a card in a numbered field
;; If the player tries to illegally plant (in a 3rd field if they
;; only have 2 fields, or in a field that contains some other bean
;; type), they AUTOMATICALY FORFEIT THE GAME and are eliminated
;; from the tournament.
(defun plant (card player n)
  (cond ((>= n (player-numfields player))
	 (error "Player ~s tried to plant a card in field ~s but has only ~s fields: FORFEIT!~%"
	   (player-name player) n (player-numfields player)))
	((not (bean-fits card (nth n (player-fields player))))
	 (error "Player ~s tried to plant a ~s in field ~s that contains ~s: FORFEIT!~%"
		(player-name player) card n 
		(car (nth n (player-fields player)))))
	(t
	 (format t "Planting ~s in ~s's ~s field~%"
		 card (player-name player) n)
	 (push card (nth n (player-fields player))))
	))


;; Harvest a specified field (which = 0, 1, or 2) and reap the
;; earned coins (if any)
(defun harvest (player which game)
  (let ((new-coins (harvest-rate (nth which (player-fields player))))
	(bean-type (car (nth which (player-fields player)))))
    (format t "Harvest ~s from player ~s field ~s: earns ~s coins~%"
	    bean-type (player-name player) which new-coins)
    (incf (player-coins player) new-coins)
    ;; Move the earned coins (beans) to the player's coins stack
    ;; and update the game summary statistics for coins
    (loop for i from 1 to new-coins
	  do (progn
	       (push (pop (nth which (player-fields player))) 
		     (player-coin-stack player))
	       (incf (cdr (assoc (car (player-coin-stack player))
				 (game-coin-stats game))))))
    ;; Move the remaining beans to the discard pile and update the
    ;; discard statistics
    (loop while (nth which (player-fields player))
	  do (progn
	       (push (pop (nth which (player-fields player))) 
		     (game-discards game))
	       (incf (cdr (assoc (car (game-discards game))
				 (game-discard-stats game))))))
    ))


;; Check to see whether a bean can be planted in a field legally
;; (field is empty or already contains this bean type)
(defun bean-fits (bean field)
  (or (not field) (eq bean (car field))))


;; Return a list of the fields (0, 1, and/or 2) that can
;; legally be harvested left -- must be a multiple, or a 
;; singleton where all fields are singletons
(defun legal-fields-to-harvest (fields &aux (harvest nil))
  (if (find-if #'is-multiple? fields)
      ;; some multiple -> collect only multiples
      (loop for i in '(0 1 2)
	    do (if (is-multiple? (nth i fields)) (push i harvest)))
    ;; no multiples -> collect all non-empty fields
    (loop for i in '(0 1 2)
	  do (if (is-planted? (nth i fields)) (push i harvest))))
  harvest)


;; Plant the card at the front of the deck.
(defun plant-card-at-front (player game)
  (funcall (player-fn player "PLANT-CARD") player
	   (pop (player-hand player)) game))


;; Return the number of coins (0-4) that would be
;; earned if the specified field was harvested.
;; Uses BeanConversion to look up redemption rate for
;; this bean type.
(defun harvest-rate (field)
  (if (null field) (return-from harvest-rate 0))
  (let ((conversion (assoc (car field) BeanConversion)))
    (cond ((not conversion) 
	   (error "Something went wrong... no conversion for ~s~%"
		  (car field)))
	  ((and (fifth conversion) (>= (length field) (fifth conversion)))
	   4)
	  ((and (fourth conversion) (>= (length field) (fourth conversion)))
	   3)
	  ((and (third conversion) (>= (length field) (third conversion)))
	   2)
	  ((and (second conversion) (>= (length field) (second conversion)))
	   1)
	  (t 0))
    ))


;; Buy a third bean field
;; Three coins are removed from the front of the player's coin-stack
;; and returned to the discards pile.
;; If a player attempts to buy a third bean field when they already
;; have one, or when they don't have three coins, nothing happens.
(defun buy-third-bean-field (player game)
  (when (and (>= (player-coins player) 3) (< (player-numfields player) 3))
    (format t "Player ~s is buying a third bean field~%" (player-name player))
    (decf (player-coins player) 3)
    (incf (player-numfields player))
    (push (pop (player-coin-stack player)) (game-discards game))
    (incf (cdr (assoc (car (game-discards game))
		      (game-discard-stats game))))
    (push (pop (player-coin-stack player)) (game-discards game))
    (incf (cdr (assoc (car (game-discards game))
		      (game-discard-stats game))))
    (push (pop (player-coin-stack player)) (game-discards game))
    (incf (cdr (assoc (car (game-discards game))
		      (game-discard-stats game))))
    ))


;; Various quickie functions to test field properties
(defun is-singleton? (field)
  (eql (length field) 1))

(defun is-multiple? (field)
  (> (length field) 1))

(defun is-empty? (field) 
  (null field))

(defun is-planted? (field)
  (car field))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UTILITY FUNCTIONS - Other functions that may be useful to students
;; in their own code (and that are also used by the game engine)
;; (Note: these are not imported by default, so you either have
;; to refer to them in the user:: package explicitly, or add them
;; to the IMPORT list in the player code file.)
;;

;; Generate N cards of type beanType (used by generate-and-shuffle())
(defun generate-n-cards (beanType number)
  (loop for i from 1 to number
	collect beanType))


;; Remove the nth item in a list.  Used by randomize().
(defmacro remove-nth (n l)
  `(if (eql ,n 0) 
       (cdr ,l) 
     (append (subseq ,l 0 ,n) (subseq ,l (+ ,n 1) (length ,l)))))


;; Return a randomized-order copy of a list of items
;; Used by generate-and-shuffle().
(defun randomize (l)
  (let ((newList) (i))
    (loop while l
	  do (progn
	       (setf i (random (length l)))
	       (push (nth i l) newList)
	       (setf l (remove-nth i l))))
    newList))


;; Return an assoc list with 0 count for every bean type.
;; (used to initialize stats for empty stacks -- discards and coins)
(defun empty-bean-stats ()
  (mapcar #'(lambda (beanCount) (setf (cdr beanCount) 0) beanCount)
	  (copy-tree BeanTypes)))





