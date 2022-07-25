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

(: select-axis : Float3 Axis -> Float3)
;; permute the components of the Float3 so that the component
;; corresponding to the specified axis is swapped with the
;; first (x) component.
(define (select-axis v axis)
  (match v
    [(Float3 x y z)
     (match axis
       ['X v]
       ['Y (Float3 y x z)]
       ['Z (Float3 z y x)])]))

(check-within (select-axis (Float3 1.0 2.0 3.0) 'X)
              (Float3 1.0 2.0 3.0)
              0.0001)
(check-within (select-axis (Float3 1.0 2.0 3.0) 'Y)
              (Float3 2.0 1.0 3.0)
              0.0001)
(check-within (select-axis (Float3 1.0 2.0 3.0) 'Z)
              (Float3 3.0 2.0 1.0)
              0.0001)





(test)




