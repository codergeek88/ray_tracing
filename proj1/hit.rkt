#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 1
;; hit.rkt module
;; Kishan Patel
;; TEST FUNCTIONS ARE IN examples.rkt
;;
;; This module contains the definition of the Hit and Hit-List types
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
(require "color.rkt")
(require "material.rkt")

;; The Parity of a ray's intersection with an object is either `IN for a
;; ray entering an object, or `OUT for a ray leaving an object.
(define-type Parity (U 'IN 'OUT))

;; A (Hit ...) records an intersection (aka "hit") between a Ray R
;; and an object.
(define-struct Hit
  ([parity : Parity]        ;; specifies if the hit is an entry or exit
   [t : Float]              ;; value of t such that R(t) is the point
                            ;; where the ray R intersects the object
   [pt : Float3]            ;; The point of intersection (i.e., R(t))
   [norm : Float3]          ;; Unit-length normal vector at pt
   [material : Material]))  ;; The material of the object at pt

(: entry? : Hit -> Boolean)
;; determines if the the hit is an entry
(define (entry? hit)
  (match hit
    [(Hit 'IN _ _ _ _) #t]
    [(Hit _ _ _ _ _) #f]))

(: hit< : Hit Hit -> Boolean)
;; determines if the first hit is closer than the second
(define (hit< hit1 hit2)
  (< (Hit-t hit1) (Hit-t hit2)))

(: first-entry : Hit-List -> (Option Hit))
;; takes in a hit list and returns either the first entry hit or an empty list
;; if there are no entries
(define (first-entry hit-list)
  (match hit-list
    ['() 'None]
    [(cons hit hr)
     (if (entry? hit) (Some hit)
         (first-entry hr))]))

(: get-reflection : Ray Hit -> (Option Reflect-Info))
;; given a ray and an entry hit record for it, get the optional reflection
;; info for the hit record's material.   Return 'None for exit hits.
(define (get-reflection ray hit)
  (local
    {(define scatter-fcn (Material-scatter (Hit-material hit)))}
    (if (entry? hit) (scatter-fcn ray (Hit-pt hit) (Hit-norm hit))
        'None)))

;; a Hit-List is a list of Hit records sorted by increasing t value that
;; describes how a ray interacts with an object.  We require that the parities
;; of adjacent hits be opposite.  In other words, a ray cannot leave
;; (resp. enter) the object unless it is inside (resp. outside) the object.
;;
(define-type Hit-List (Listof Hit))

;; We use the empty list to represent a miss
;;
(define miss : Hit-List '())

;; ==== Testing support ====

(: valid-hit-list? : Hit-List -> Boolean)
;; test that a hit list is valid.  I.e., that its t values are increasing and
;; that parities alternate.  This function can be used to check the correctness
;; of other functions, but otherwise does no useful work.
(define (valid-hit-list? hits)
  (local
    {(: checker : Parity Real Hit-List -> Boolean)
     ;; check the hits, where p1 is the parity and t1 is the t value of the
     ;; preceeding hit
     (define (checker p1 t1 hits)
       (match hits
         [(cons (Hit p2 t2 _ _ _) hitr)
          (and (<= t1 t2) (not (symbol=? p1 p2)) (checker p2 t2 hitr))]
         ['() #t]))}
    (match hits
      [(cons (Hit p t _ _ _) hitr) (checker p t hitr)]
      ['() #t])))

;; A (Hit-Info ...) struct records the geometric information about a
;; hit record.  This structure can be used to implement check-within
;; tests, since it does not involve materials, which cannot be compared.
(define-struct Hit-Info
  ([parity : Parity]        ;; specifies if the hit is an entry or exit
   [t : Float]              ;; value of t such that R(t) is the point
                            ;; where the ray R intersects the object
   [pt : Float3]            ;; The point of intersection (i.e., R(t))
   [norm : Float3]))        ;; Unit-length normal vector at pt

(: hit->info : Hit -> Hit-Info)
;; extract the hit-info from a Hit record
(define (hit->info hit)
  (match hit
    [(Hit p t pt n _) (Hit-Info p t pt n)]))

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide Parity
         (struct-out Hit)
         Hit-List
         (struct-out Hit-Info))
(provide entry?
         hit<
         get-reflection
         miss
         first-entry
         valid-hit-list?
         hit->info)
