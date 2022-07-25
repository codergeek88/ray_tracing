#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 2
;; plane.rkt module
;; Kishan Patel
;;
;; This module contains the implementation of the plane object
;; CHECK TESTS IN plane-test.rkt

;; load custom definitions
;;
(require "../include/cs151-core.rkt")

;; load image library definitions
;;
(require "../include/cs151-image.rkt")

;; load testing infrastructure
;;
(require typed/test-engine/racket-tests)

;; project modules
;;
(require "util.rkt")
(require "math-util.rkt")
(require "material.rkt")
(require "hit.rkt")
(require "object.rkt")

;; CHECK TESTS IN plane-test.rkt
(: make-plane : Float3 Float3 Material -> Object)
;; (make-plane pt perp material) makes a plane object.  The plane
;; contains the point pt and its orientation is defined by the
;; vector perp, which is perpendicular to the plane.  The third
;; argument specifies the plane's surface material.
;; Note that the perpendicular vector does not have to be unit length.
(define (make-plane pt perp material)
  (Object
   (lambda ([ray : Ray] [min-t : Float])
     (local
       {(define P (Ray-origin ray))
        (define D (Ray-dir ray))}
       (if (< -0.0001 (fl3-dot D perp) 0.0001)
           miss
           (local
             {(define t (/ (fl3-dot (fl3- pt P) perp) (fl3-dot D perp)))
              (define norm (fl3-normalize perp))
              (define inter (fl3-scale+ P t D))
              (define s (fl3-dot (fl3- pt P) perp))
              (define inf-hit (Hit 'OUT +inf.0 fl3-zero fl3-zero flat-black))}
             (if (< t min-t)
                 (if (< (fl3-dot D perp) 0)
                     (list inf-hit)
                     miss)
                 (if (< s 0)
                     (list (Hit 'IN t inter norm material) inf-hit)
                     (list (Hit 'OUT t inter norm material))))))))))
;; CHECK TESTS IN plane-test.rkt

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide make-plane)
