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


;; Required Functions

(defun plant-card (player card game)
  "Plants the required card."
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
              (return-from plant-card)))))

  ; Finally, harvest the least-value field and plant the card there.
  (let ((harvest-choice (choose-harvest player game)))
    (harvest player harvest-choice game)
    (plant card player harvest-choice)))

(defun optionally-plant-card (player game)
  ;checks for at risk
  (if (at-risk? player (nth i (player-hand player)))
    ;if we are at risk, return from the function
    (return-from)
    ;if not, perform a series of loops
    (progn
      ;check to see if there is a matching field
      (for i in (length (player-fields player))
        (if (eq field ( (player-hand player)))
          (progn
            (plant card player i)
            (return-from))))
      ;see if there is an empty field, if so plant
      (for i in (length (player-fields player))
        (if (eq (nth i (player-fields player)) nil)
          (progn
            (plant card player i)
            (return-from))))
      ;the last step is harvesting the least valuable field, which is what harvest-field does
      (harvest player (choose-harvest player game) game))))



(defun handle-face-up-cards (player game)
    ;guarenteed to work
    ;stupid
    ;will be changed later before the final submit
    (plant-card player (pop (player-faceup player)) game)
    (plant-card player (pop (player-faceup player)) game)
  )


;; Utility Functions

(defun choose-harvest (player game &aux least-valuable-field)
  "Determines the least valuable field that is legal to harvest."
  (loop for f in (legal-fields-to-harvest (player-fields player)) do
        (if (or
              (equal least-valuable-field nil)
              (< (value player (car (nth f (player-fields player))) game)
                 (value player (car (nth least-value-field (player-fields player))) game)))
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
    ((cards-in-play (+ (cdr (assoc card) BeanTypes)
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
     (coins-earned (harvest-rate held-field)))

    ; Heuristic calcuation.
    (* (/ cards-in-play beans-to-next-tier) coins-earned)))

(defun at-risk? (player card game)
    ; Always returns true
    ; Just here to get the program to work
    ; Will be changed before the final submit
    (= 1 1)
  )
