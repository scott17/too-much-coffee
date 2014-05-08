;; Team TOO MUCH COFFEE
;; Player Phase II


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
		  user::trade-from-player user::trade-from-card user::trade-from-pos
		  user::trade-from-score
		  user::trade-to-player user::trade-to-card user::trade-to-pos
		  user::trade-to-score user::trade-info
		  user::plant user::harvest user::bean-fits
		  user::legal-fields-to-harvest user::plant-card-at-front
		  user::harvest-rate user::buy-third-bean-field
		  user::is-singleton? user::is-multiple? user::is-empty?
		  user::is-planted? user::trade user::viable-trade))


;; Required Functions

(defun plant-card (player card game)
  ; "Plants the required card."
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

	
;; Generates trades based on the cards that match our fields
;;  We will favor trades that match and trade away face-up cards
;;   that match none of our fields
(defun generate-trades (player &aux (trades nil) desired-cards)
	)

;; Generates trades based on a list of good cards we want,
;;   the player, a bad card we own, the value of such a trade
;;   and the location of the bad card
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
	;;For each of the trades in trade
	(loop for trade in trades
		;;If the trade is not a donation
		do (if (trade-to-card trade)
				;;viable is a list of indices in our hand where we have to card
				(let ((viable (viable-trade player trade)))
					;;If the trade is viable and we have the to bean type in our field
					 (if (and viable (assoc (trade-from-card trade) (player-fields player)))
						;;If the from card matches one of our fields
						 (if (assoc (trade-to-card trade) (player-fields player))
							;;If it does, accept the trade, but trade the last
							(push (list player 1 (last viable))
								(trade-info trade))
							;;If it does not, accept the trade, but trade the first
							(push (list player 1 (car viable))
								(trade-info trade)))
						;;If the to card matches none of our fields, do not trade.
						 (push (list player 0) (trade-info trade))
						)
				)
			;;If the trade is a donation and the to card matches one of our fields
			(if (assoc (trade-from-card trade) (player-fields player))
				;;Accept the donation if a match
				(push (list player 1) (trade-info trade))
				;;Decline the donation if it doesnt match
				(push (list player 0) (trade-info trade)))
		)
	)
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

; Value to force a useless bean to a very small heuristic value.
; Avoids divide-by-zero errors if we hold enough beans to
; eliminate the possibility of a higher coin tier.
(defconstant *USELESS-BEAN* 1000000)

(defun value (player card game)
  "Calculates the value of a card using our heuristic"
  (let*
    ; Calculate the number of cards of this type that are not
    ; in a field or held in a player's hand.
    ((cards-in-play (+ (cdr (assoc card BeanTypes))
                       (- (cdr (assoc card (game-coin-stats game))))
                       (- (cdr (assoc card (game-discard-stats game))))
                       (- 1)))

     ; If we have a field containing this card type, this stores the field.
     ; Otherwise, nil (length of nil is 0).
     (held-field (assoc card (player-fields player)))
     ; Card to coin conversion for this bean.
     (conversion (assoc card BeanConversion))

     ; Number of beans held (0 if held-field is nil, meaning no field has this type).
     (beans-held (length held-field))

     ; Number of beans needed to get to the next tier of coins. If there
     ; is no higher tier of coins, *USELESS-BEAN* to avoid divide-by-zero
     ; and make the bean useless relative to others (harvested early).
     (beans-to-next-tier (cond
                           ((and (second conversion) (< beans-held (second conversion)))
                            (- (second conversion) beans-held))
                           ((and (third conversion) (< beans-held (third conversion)))
                            (- (third conversion) beans-held))
                           ((and (fourth conversion) (< beans-held (fourth conversion)))
                            (- (fourth conversion) beans-held))
                           ((and (fifth conversion) (< beans-held (fifth conversion)))
                            (- (fifth conversion) beans-held))
                           (t *USELESS-BEAN*)))

     ; Number of coins earned if we harvest the field.
     (coins-earned (if (not (equal held-field nil))
                     (harvest-rate held-field)
                     1)))

    ; Heuristic calcuation.
    (* (/ cards-in-play beans-to-next-tier) coins-earned)))

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
