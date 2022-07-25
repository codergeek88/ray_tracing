#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 1
;; trace.rkt module
;; Kishan Patel
;;
;; Ray casting and recursive ray tracing
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
(require "camera.rkt")
(require "material.rkt")
(require "hit.rkt")
(require "object.rkt")


(: cast-ray-in-world : Object -> Ray -> RGB)
;; takes an object and a ray and then tests the ray for intersection with the
;; object to determine an RGB value
(define (cast-ray-in-world obj)
  (lambda ([ray : Ray])
    (local
      {(define hit-list (hit-test obj ray 0.0))
       (define f-entry (first-entry hit-list))}
      (match f-entry
        ['None (ray->rgb ray)]
        [(Some hit)
         (match (get-reflection ray hit)
           ['None (ray->rgb ray)]
           [(Some reflect-info) (Reflect-Info-aten reflect-info)])]))))

(: trace-ray-in-world : Object Natural -> Ray -> RGB)
;; Given a world and a maximum tracing depth, this function returns
;; a function that will recursively trace a ray through the world
;; to compute a color
(define (trace-ray-in-world obj max-depth)
  (lambda ([ray : Ray])
    (local
      {(: trace-ray-helper : Ray Natural -> RGB)
      ;; recursive helper function to trace ray
      (define (trace-ray-helper ray depth-lim)
        (if (<= depth-lim 0) rgb-black
            (match (first-entry (hit-test obj ray 0.001))
              ['None (ray->rgb ray)]
              [(Some hit)
               (match (get-reflection ray hit)
                 ['None rgb-black]
                 [(Some reflect-info)
                  (match reflect-info
                    [(Reflect-Info aten reflect-ray)
                     (rgb* aten (trace-ray-helper reflect-ray
                                                  (- depth-lim 1)))])])])))}
      (trace-ray-helper ray max-depth))))

(: ray-tracer : Camera Object Natural -> Image)
;; Given a camera, world object, and max depth, render a scene
;; using the given depth limit.
(define (ray-tracer cam world max-depth)
  (foreach-pixel cam
                 (make-pixel-renderer
                  (antialias-pixel->rgb cam
                                        (trace-ray-in-world world max-depth))
                  gamma-rgb->color)))


;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide cast-ray-in-world
         trace-ray-in-world
         ray-tracer)
