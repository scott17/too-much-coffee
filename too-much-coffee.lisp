;; Team TOO MUCH COFFEE
;; Player Phase I


;; Package Definition

(defpackage :too-much-coffee)
(in-package :too-much-coffee)


;; Imports

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


;; Global variables for tracking deck statistics.

(defvar *drawn-since-shuffle* 0)
(defvar *rounds-since-shuffle* 0)
(defvar *shuffles* 0)


;; Required Functions

(defun plant-card (player card game)
  "Plants the required card."

  ; Before planting, update our own deck statistics.
  ; (Used in #'value)
  (if (not (equal *shuffles*
                  (game-shuffles game)))
    ; If our shuffle count and the real shuffle count are not equal,
    ; the deck has been shuffled. We need to reset the number of rounds
    ; since the last shuffle and the number of cards drawn.
    (progn
      (setf *shuffles* (game-shuffles game))
      (setf *rounds-since-shuffle* 0)
      ; TODO do we want to assume that no cards have been drawn when
      ; we detect a new shuffle, or do we want to assume that a full
      ; round has already completed? AT THE BEGINNING of the game, we
      ; of course assume that no rounds have happened. Do we assume 1 here?
      ; if so, *rounds-since-shuffle* 1
      (setf *drawn-since-shuffle* 0))
    ; Deck has not been shuffled. Increase the number of rounds that
    ; has occurred.
    (incf *rounds-since-shuffle*))
  ; Increment the number of cards that has been drawn since
  ; the last shuffle by five cards for every player that is
  ; in the game. This is an estimate, not an exact value.
  (setf *drawn-since-shuffle* (+ *drawn-since-shuffle*
                                 (* *rounds-since-shuffle*
                                    (length (game-players game))
                                    5)))

  ; First attempt to plant the card into a field containing the same card type.
  (let ((same-field (assoc card (player-fields player))))
    (if (not (equal same-field nil))
      (progn
        (plant card player (position same-field (player-fields player)))
        (return-from plant-card))))

  ; Then, attempt to plant it into an empty field.
  (loop for n in '(0 1 2) do
        (progn
          (if (and (is-empty? (nth n (player-fields player)))
                   (or (equal (player-numfields player) 3)
                       (not (equal n 2))))
            (progn
              (plant card player n)
              (return-from plant-card)
              ))))

  ; Finally, harvest the least-value field and plant the card there.
  (let ((harvest-choice (choose-harvest player game)))
    (harvest player harvest-choice game)
    (plant card player harvest-choice)))

(defun get-first-card (player)
  (car (player-hand player)))

(defun optionally-plant-card (player game)
  ;checks for at risk
  (if (at-risk? player (get-first-card player) game)
    ;if we are at risk, return from the function
    (return-from optionally-plant-card)
    ;if not, perform a series of loops
    (progn
      ;check to see if there is a matching field
      (loop for i from 0 to (- 1 (player-numfields player)) do
            (if (eq field (player-hand player))
              (progn
                (plant card player i)
                ; (return-from optionally-plant-card))
                nil))
            ;see if there is an empty field, if so plant
            (loop for i from 0 to (- 1 (player-numfields player)) do
                  (if (eq (nth i (player-fields player)) nil)
                    (progn
                      (plant card player i)
                      ; (return-from optionally-plant-card))
                      nil))
                  ; the last step is harvesting the least valuable field, which is what harvest-field does
                  ; what was I on about?
                  ; (harvest player (choose-harvest player game) game)
                  )))))



(defun handle-face-up-cards (player game)
  ;These first two are guarenteed to work
  ;They are stupid
  ;This will be changed later before the final submit
  ;(plant-card player (pop (player-faceup player)) game)
  ;(plant-card player (pop (player-faceup player)) game)


  (progn 
    ; First get the two cards 
    (setq first-card (pop (player-faceup player))
          second-card (pop (player-faceup player)))
    ; Plant the cards in matching fields if they exist
    (let ((same-field (assoc first-card (player-fields player))))
      (if (not (equal same-field nil))
        (progn
          (plant-card player first-card game)
          (setq first-card nil))))
    (let ((same-field (assoc second-card (player-fields player))))
      (if (not (equal same-field nil))
        (progn
          (plant-card player second-card game)
          (setq second-card nil))))
    ; If we planted both of the cards we are done
    (if (and (equal first-card nil) (equal second-card nil))
      (return-from handle-face-up-cards))
    ; If both aren't planted yet
    (if (and (not (equal first-card nil)) (not (equal second-card nil)))
      ; If the first is more valuable than the second,
      ;   plant the second card first and then the first card.
      ;   You want the most valuable card planted last to ensure that
      ;   it will be in the fields
      (if (> (value player first-card game) (value player second-card game))
        (progn
          (plant-card player second-card game)
          (plant-card player first-card game)
          (return-from handle-face-up-cards))
        ;else
        ; Plant the first card first and the second last
        (progn
          (plant-card player first-card game)
          (plant-card player second-card game)
          (return-from handle-face-up-cards))))
    ; Finally, if the first isn't planted yet,
    ;  plant it.
    (if (not (equal first-card nil))
      (plant-card player first-card game)
      ; Else, plant the second card
      (plant-card player second-card game))

    ))

(defun generate-trades (player &aux (trades nil) desired-cards)
	)
	
(defun make-new-trades (player loc bad goods value)
  (loop for card in goods
	collect (make-instance 'trade :from-player player
				      :from-card bad
				      :from-pos loc
				      :from-score value
				      :to-card card
				      :info nil)))
					  
					  

;; This function takes the player object and a list of
;; proposed trades (objects of class "TRADE") and scores
;; each of the trades, pushing a list
;;    (PLAYER SCORE (CARD POSITION) (CARD POSITION) ...)
;; on the front of the TRADE-INFO slot for each trade offered.
;; 
;; Score: 1 means a fully acceptable trade; 0 means a nonviable or
;; unacceptable trade; scores in between are interpreted as
;; strength of preference for the given trade.
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
;; Our version of this uses our new version of the value function.
;;    The value function uses probabilities and the current game state
;;    to determine the approximate value of a card.

(defun evaluate-trades (player trades)
	)
	

;; Utility Functions

(defun choose-harvest (player game &aux least-valuable-field)
  "Determines the least valuable field that is legal to harvest."
  (loop for f in (legal-fields-to-harvest (player-fields player)) do
        (if (or
              (equal least-valuable-field nil)
              (< (value player (car (nth f (player-fields player))) game)
                 (value player (car (nth least-valuable-field (player-fields player))) game)))
          (setf least-valuable-field f)))
  least-valuable-field)


(defun value (player card game)
  (let* (
         ; Total size of the current max deck
         (shuffle-deck-size (loop for i in (game-current-deck game) sum (cdr i)))
         ; Maximum number of 'card in current deck
         (max-of-type (assoc card (game-current-deck game)))
         ; Minimum number of 'card in current deck
         (min-of-type (- max-of-type *drawn-since-shuffle*))
         ; Deck size right now
         ; Set to 5 (number of cards we will draw) if the deck is empty
         ; after our turn. Avoids divide-by-zero
         (actual-deck-size (- shuffle-deck-size *drawn-since-shuffle*))
         (actual-deck-size (if (<= actual-deck-size 0)
                             5 ; Number of cards left in deck
                             actual-deck-size))

         ; EDGE CASE - if there is one card in the deck at the beginning
         ; of our turn, the deck will be shuffled in the middle in our hand.
         ; Currently we don't handle this and assume that there are still
         ; 5 cards in the deck. TODO maybe handle later.

         ; Number of beans held of this type
         (beans-held (length (assoc card (player-fields player))))
         ; Coin conversion
         (conversion (assoc card BeanConversion))
         ; Number of beans to earn the next coin
         (beans-to-next-tier (cond
                               ((and (second conversion) (< beans-held (second conversion)))
                                (- (second conversion) beans-held))
                               ((and (third conversion) (< beans-held (third conversion)))
                                (- (third conversion) beans-held))
                               ((and (fourth conversion) (< beans-held (fourth conversion)))
                                (- (fourth conversion) beans-held))
                               ((and (fifth conversion) (< beans-held (fifth conversion)))
                                (- (fifth conversion) beans-held))
                               (t 0)))

         ; Probability of getting the coin given that we draw
         ; beans-to-next-tier of the current requested bean.
         (prob-get-coin (reduce #'* (loop for i from 0 to (- beans-to-next-tier 1) collect
                                          (let* (
                                                 ; Maximum probability of drawing the bean
                                                 (max-prob (/ (- max-of-type i) (- actual-deck-size i)))
                                                 ; Minimum probability of drawing the bean
                                                 (min-prob (/ (- min-of-type i) (- actual-deck-size i)))
                                                 ; Average probability of drawing the bean
                                                 (avg-prob (/ (+ max-prob min-prob) 2))
                                                 )
                                            avg-prob))))
         )
    prob-get-coin))



(defun at-risk? (player card game)
  ; Check if card matches the type of any field. If not, return, no risk.
  (if (not (assoc card (player-fields player)))
    (return-from at-risk? nil))

  ; Check if there is an empty field.
  (loop for f in '(0 1 2) do
        (if (and (or (equal (player-numfields player) 3)
                     (not (equal f 2)))
                 (is-empty? (nth f (player-fields player))))
          (return-from at-risk? nil)))

  ; Pretend to plant the card.
  (let*
    ((pretend-fields (copy-list (player-fields player)))
     (matching-field (position (assoc card pretend-fields) pretend-fields)))

    (push card (nth matching-field pretend-fields))

    ; Find the most valuable field.
    (setf most-valuable-field nil)
    (loop for f in '(0 1 2) do
          (if (and (not (equal (nth f pretend-fields) nil))
                   (or (equal most-valuable-field nil)
                       (> (value player (car (nth f pretend-fields)) game)
                          (value player (car (nth most-valuable-field pretend-fields)) game))))
            (setf most-valuable-field f)))

    ; Find the other fields.
    (setf other-fields '(0 1 2))
    (setf other-fields (remove most-valuable-field other-fields))
    (if (not (equal (player-numfields player) 3))
      (setf other-fields (remove 2 other-fields)))

    ; Are the other fields singletons?
    (setf singletons t)
    (loop for f in other-fields do
          (if (not (is-singleton? (nth f pretend-fields)))
            (setf singletons nil)))

    ; If not all singletons, no risk.
    (if (not singletons)
      (return-from at-risk? nil))

    ; 2 in one field 1 in current
    ; RISK
    ; if we reveal two cards, we must harvest the field we planted

    ; ASSUME RISK otherwise
    ; fix it later
    t))
