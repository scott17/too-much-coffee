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

(defun value (player card game)
  )

(defun at-risk? (player card game)
  )
