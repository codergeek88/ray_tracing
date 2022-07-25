#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 2
;; plane-test.rkt
;;
;; Tests for make-plane
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
(require "plane.rkt")
(require "camera.rkt")
(require "sphere.rkt")
(require "color.rkt")
(require "trace.rkt")

;; Check tests
(define inf-hit-info (Hit-Info 'OUT fl-max fl3-zero fl3-zero))



(define hit-test1 (Object-hit-test (make-plane (Float3 1.3 -0.8 -1.0)
                                               (Float3 0.0 0.0 -1.0)
                                               normal-material)))
(define hit-info1
  (map hit->info (hit-test1 (make-ray fl3-zero (Float3 0.0 0.0 -1.0)) 0.0)))

(check-within hit-info1
              (list
               (Hit-Info 'OUT 1.0 (Float3 0.0 0.0 -1.0) (Float3 0.0 0.0 -1.0)))
              0.001)



(define hit-test2 (Object-hit-test (make-plane (Float3 0.0 0.0 1.0)
                                               (Float3 0.0 0.0 1.0)
                                               normal-material)))
;; parallel to plane
(check-expect (hit-test2 (make-ray fl3-zero (Float3 0.6 0.3 0.0)) 0.0) '())



(define hit-test3 (Object-hit-test (make-plane (Float3 0.0 0.0 -1.0)
                                               (Float3 0.0 0.0 1.0)
                                               normal-material)))

;; starts from outside half-space and never intersects plane
(check-expect (hit-test3 (make-ray fl3-zero (Float3 0.6 0.3 0.9)) 0.0) '())




(define hit-test4 (Object-hit-test (make-plane (Float3 1.3 -0.8 -1.0)
                                               (Float3 0.0 0.0 1.0)
                                               normal-material)))
(define hit-info4
  (map hit->info (hit-test4 (make-ray fl3-zero (Float3 0.0 0.0 -1.0)) 0.0)))

;; starts from outside half-space and intersects plane
(check-within hit-info4
              (list
               (Hit-Info 'IN 1.0 (Float3 0.0 0.0 -1.0) (Float3 0.0 0.0 1.0))
               inf-hit-info)
              0.001)



(define hit-test5 (Object-hit-test (make-plane (Float3 1.3 -0.8 -1.0)
                                               (Float3 0.0 0.0 1.0)
                                               normal-material)))
(define hit-info5
  (map hit->info (hit-test5 (make-ray (Float3 0.0 0.0 -2.0)
                                      (Float3 -1.2 2.7 -0.6))
                            0.0)))

;; starts from inside half-space and never intersects plane
(check-within hit-info5 (list inf-hit-info) 0.001)


(test)



;; Eyeball tests
(define sphere-6 (make-sphere (Float3 0.0 -100.5 -2.0) 100.0
                              (lambertian-material (RGB 0.8 0.8 0.0))))



(time (ray-tracer
       (make-camera 400 200 100 (Float3 0.0 0.25 2.0) fl3-zero
                    (Float3 0.0 1.0 0.0) 120.0)
       (Scene
        (list->object
         (list
          (make-plane (Float3 0.0 0.0 -2.5)
                      (Float3 1.0 0.0 1.0)
                      (metal-material (RGB 0.9 0.9 0.9) 0.05))
          (make-sphere fl3-zero
                       0.5
                       (lambertian-material (RGB 0.8 0.3 0.3)))
          sphere-6))
        ray->rgb)
       10))



"== AA-100 ray-tracer plane test: two spheres and two planes"
(time (ray-tracer
       (make-camera 400 200 100 (Float3 0.0 0.25 2.0) fl3-zero
                    (Float3 0.0 1.0 0.0) 120.0)
       (Scene
        (list->object
         (list
          (make-plane (Float3 0.0 0.0 -2.5)
                      (Float3 1.0 0.0 1.0)
                      (metal-material (RGB 0.9 0.9 0.9) 0.05))
          (make-plane (Float3 0.0 0.0 -2.5)
                      (Float3 -1.0 0.0 1.0)
                      (metal-material (RGB 0.9 0.9 0.9) 0.05))
          (make-sphere fl3-zero
                       0.5
                       (lambertian-material (RGB 0.8 0.3 0.3)))
          sphere-6))
        ray->rgb)
       10))
