#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 2
;; material.rkt module
;; Kishan Patel
;;
;; This module contains the definition of the Hit and Material types
;; CHECK TESTS FOR diffuse-emissive-material IN examples.rkt

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

;; An Emit-Fun is a function that takes a Ray and surface normal and returns
;; the RGB value for the light being emitted by a material.  The surface normal
;; allows the light to be directional.
(define-type Emit-Fun (Ray Float3 -> RGB))

;; A (Reflect-Info rgb vec) value represents the information about a ray's hit
;; on the surface of an object.  The rgb value is the attenuation factor and
;; the ray is the reflection/scatter ray for the surface
(define-struct Reflect-Info
  ([aten : RGB]             ;; attenuation factor
   [reflect-ray : Ray]))    ;; ray that is scattered or reflected from surface

;; A Scatter-Fun is a function that takes a Ray, intersection position,
;; and surface normal, and returns optional reflection information for
;; the given inputs.  The function returns `None when there is not color
;; contribution and some reflection info otherwise.
(define-type Scatter-Fun (Ray Float3 Float3 -> (Option Reflect-Info)))

;; A Material is a struct containing an emit function and a scatter
;; function.  Materials represent information about the surface
;; properties of an object.
(define-struct Material
  ([emit : Emit-Fun]
   [scatter : Scatter-Fun]))

(: no-emission : Emit-Fun)
;; emit function for materials that do not emit light
(define (no-emission ray norm) rgb-black)

(: make-reflect-info : RGB Ray -> (Option Reflect-Info))
;; helper function that builds some hit info
(define (make-reflect-info rgb ray) (Some (Reflect-Info rgb ray)))

(: flat-material : RGB -> Material)
;; flat shading with the given color
(define (flat-material rgb)
  (Material
   no-emission
   (lambda ([ray : Ray] [pt : Float3] [norm : Float3])
     (make-reflect-info rgb (Ray fl3-zero fl3-zero)))))

(: normal-material : Material)
;; A material that returns a color that is computed from the surface normal
(define normal-material
  (Material
   no-emission
   (lambda ([ray : Ray] [pt : Float3] [norm : Float3])
     (match norm
       [(Float3 x y z)
        (make-reflect-info (RGB (* 0.5 (+ x 1.0))
                                (* 0.5 (+ y 1.0))
                                (* 0.5 (+ z 1.0)))
                           (Ray fl3-zero fl3-zero))]))))

(: lambertian-material : RGB -> Material)
;; A Lambertian surface of the given color
(define (lambertian-material aten)
  (Material
   no-emission
   (lambda ([ray : Ray] [int : Float3] [norm : Float3])
     (Some (Reflect-Info aten (make-ray
                               int (fl3+ norm (random-point-in-sphere))))))))

(: metal-material : RGB Float -> Material)
;; a metallic surface
(define (metal-material aten fuzz)
  (Material
   no-emission
   (lambda ([ray : Ray] [int : Float3] [norm : Float3])
     (local
       {(define S (fl3-scale+ (fl3-reflect (Ray-dir ray) norm)
                              fuzz (random-point-in-sphere)))}
       (if (<= (fl3-dot S norm) 0)
           'None
           (Some (Reflect-Info aten (make-ray int S))))))))

(define flat-black : Material (flat-material rgb-black))

;; CHECK TESTS IN examples.rkt
(: diffuse-emissive-material : RGB -> Material)
;; return specified RGB value from emit function and 'None for scatter function
(define (diffuse-emissive-material rgb)
  (Material
   (lambda ([ray : Ray] [v : Float3]) rgb)
   (lambda ([ray : Ray] [v : Float3] [w : Float3]) 'None)))
;; CHECK TESTS IN examples.rkt

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide (struct-out Reflect-Info)
         (struct-out Material))

(provide no-emission
         flat-material
         flat-black
         normal-material
         lambertian-material
         metal-material
         diffuse-emissive-material)
