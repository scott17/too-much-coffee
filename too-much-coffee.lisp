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
  )

(defun optionally-plant-card (player game)
  )

(defun handle-face-up-cards (player game)
  )


;; Utility Functions

(defun choose-harvest (player game)
  )

(defconstant *USELESS-BEAN* 1000000)

(defun value (player card game)
  (let* ((cards-in-play (+ (cdr (assoc card) BeanTypes)
                           (- (cdr (assoc card (game-coin-stats game))))
                           (- (cdr (assoc card (game-discard-stats game))))
                           (- 1)))

         (held-field (assoc card (player-fields player)))
         (conversion (assoc card BeanConversion))

         (beans-held (length held-field))
         (beans-to-next-tier (cond
                               ((and (second convesion) (< (second conversion) beans-held))
                                (- (second conversion) beans-held))
                               ((and (third conversion) (< (third conversion) beans-held))
                                (- (third conversion) beans-held))
                               ((and (fourth conversion) (< (fourth conversion) beans-held))
                                (- (fourth conversion) beans-held))
                               ((and (fifth conversion) (< (fifth conversion) beans-held))
                                (- (fifth conversion) beans-held))
                               (t *USELESS-BEAN*)))

         (coins-earned (harvest-rate held-field)))

    (* (/ cards-in-play beans-to-next-tier) coins-earned)))

(defun at-risk? (player card game)
  )
