#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 1
;; sphere.rkt module
;; Kishan Patel
;; TEST FUNCTIONS ARE IN examples.rkt
;;
;; This module contains the implementation of the sphere object
;;

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

(: intersection : Float3 Float3 Float -> Float3)
;; calculate R(t) = P + tD (point of intersection)
(define (intersection P D t) (fl3-scale+ P t D))

(: surface-normal : Float3 Float3 Float -> Float3)
;; calculates the surface normal given a point of intersection and the center
;; of the sphere
(define (surface-normal int center radius)
  (fl3-scale (/ 1 radius) (fl3- int center)))

(: make-sphere : Float3 Float Material -> Object)
;; make a sphere object with the given center, radius, and material
(define (make-sphere center radius material)
  (local
    {(define r-2 (* radius radius))
     (: sphere-hit-test : Ray Float -> Hit-List)
     ;; hit test for sphere object
     (define (sphere-hit-test ray min-t)
       (local
         {(define P (Ray-origin ray))
          (define V (fl3- P center))
          (define D (Ray-dir ray))
          (define a 1)
          (define b (* 2 (fl3-dot V D)))
          (define c (- (fl3-dot V V) r-2))
          (define d (- (* b b) (* 4 a c)))}
         (if (<= d 0) '()
             (local
               {(define root-d (fl-sqrt d))
                (define t2 (/ (+ (- b) root-d) 2))}
               (cond
                 [(< t2 min-t) '()]
                 [else
                  (local
                    {(define t1 (/ (- (- b) root-d) 2))
                     (define Q1 (intersection P D t1))
                     (define Q2 (intersection P D t2))}
                    (cond
                      [(< t1 min-t t2) (list (Hit 'OUT t2 Q2
                                                  (surface-normal Q2 center
                                                                  radius)
                                                  material))]
                      [else (list (Hit 'IN t1 Q1 (surface-normal Q1 center
                                                                 radius)
                                       material)
                                  (Hit 'OUT t2 Q2 (surface-normal Q2 center
                                                                  radius)
                                       material))]))])))))}
    (Object sphere-hit-test)))

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide make-sphere)
