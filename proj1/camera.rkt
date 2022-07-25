#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 1
;; camera.rkt module
;; Kishan Patel
;; TEST FUNCTIONS ARE IN examples.rkt
;;
;; This module implements the Camera abstraction
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
(require "math-util.rkt")
(require "color.rkt")

;; the representation of a Camera for the ray tracer.  The camera
;; is assumed to be at the origin looking down the -Z axis.
(define-struct Camera
  [(wid : Natural)           ;; width of image
   (ht : Natural)            ;; height of image
   (n-samples : Natural)     ;; number of samples per pixel
   (focal-len : Float)])     ;; distance to image plane (in -Z direction)

;; A Pixel-Renderer is a function that takes the row and column of a pixel
;; and produces a Racket Image-Library Color
(define-type Pixel-Renderer (Natural Natural -> Color))

(: gen-colors : Camera Pixel-Renderer -> (Listof Color))
;; generates list of colors from camera and pixel-renderer
;; to later convert into image
(define (gen-colors cam pr)
  (local
    {(define n (Camera-wid cam))
     (define m (Camera-ht cam))
     (: gen-colors-aux : Natural Natural (Listof Color) -> (Listof Color))
     ;; auxillary function to build list of colors
     (define (gen-colors-aux i j acc)
       (cond
         [(and (= i 0) (= j 0)) (cons (pr i j) acc)]
         [(and (> i 0) (= j 0) (>= n 1)) (gen-colors-aux (- i 1) (- n 1)
                                                (cons (pr i j) acc))]
         [(> j 0) (gen-colors-aux i (- j 1)
                               (cons (pr i j) acc))]
         [else acc]))}
    (if (and (>= m 1) (>= n 1))
        (gen-colors-aux (- m 1) (- n 1) '())
        '())))

(: foreach-pixel : Camera Pixel-Renderer -> Image)
;; generates image by calling pixel-renderer on each pixel in the camera's view
(define (foreach-pixel cam pr)
  (color-list->bitmap (gen-colors cam pr) (Camera-wid cam) (Camera-ht cam)))

(: make-pixel-renderer : (Natural Natural -> RGB) (RGB -> Color) ->
   Pixel-Renderer)
;; compose a function that maps pixel coordinates to RGB values with
;; an RGB to Image-Library Color converter
(define (make-pixel-renderer pixel->rgb rgb->color)
  (lambda ([row : Natural] [col : Natural]) (rgb->color (pixel->rgb row col))))

(: ray-for-pixel : Camera -> (Natural Natural -> Ray))
;; takes in a camera
;; returns a function for generating a ray for a pixel
;;   specified by its row and column
(define (ray-for-pixel cam)
  (local
    {(define w (->fl (Camera-wid cam)))
     (define h (->fl (Camera-ht cam)))
     (define view-z (- (Camera-focal-len cam)))
     (define pw (/ 2 w))
     (define x-0 (- (/ pw 2) 1))
     (define y-0 (- (/ h w) (/ pw 2)))}
     (lambda ([i : Natural] [j : Natural])
       (local
         {(define x-coord (+ x-0 (* j pw)))
          (define y-coord (- y-0 (* i pw)))}
         (make-ray fl3-zero (Float3 x-coord y-coord view-z))))))

(define cam-400x200x1 (Camera 400 200 1 1.0))
(check-within ((ray-for-pixel cam-400x200x1) 0 0) (make-ray fl3-zero
                                                            (Float3 -0.667 0.333
                                                                    -0.667))
              0.01)

(: pixel->rgb : Camera (Ray -> RGB) -> Natural Natural -> RGB)
;; given a camera and a ray-tracing function, return a function that
;; traces the ray for the given pixel
(define (pixel->rgb cam rt)
  (local
    {(define ray-for-pixel-cam (ray-for-pixel cam))}
    (lambda ([i : Natural] [j : Natural])
      (rt (ray-for-pixel-cam i j)))))

(: rays-for-pixel : Camera -> (Natural Natural -> (Listof Ray)))
;; given a camera, return a function that maps pixel coordinates in
;; the image plane to a list of rays from the camera through the pixel.
;; The number of rays is determined by the n-samples field of the
;; Camera.
(define (rays-for-pixel cam)
  (local
    {(define w (->fl (Camera-wid cam)))
     (define h (->fl (Camera-ht cam)))
     (define view-z (- (Camera-focal-len cam)))
     (define pw (/ 2 w))
     (define x-ulc -1)
     (define y-ulc (/ h w))
     (define n (Camera-n-samples cam))

     (: random-ray : Natural Natural -> Ray)
     ;; generate a random ray in a pixel of given row and column
     (define (random-ray i j)
       (local
         ;; define row and column offset 
         {(define i-off (random))
          (define j-off (random))
          ;; calculate head of vector
          (define head (Float3 (+ x-ulc (* (+ j j-off) pw))
                               (- y-ulc (* (+ i i-off) pw))
                               view-z))}
         (make-ray fl3-zero head)))}

     (lambda ([i : Natural] [j : Natural])
       (local
         {(: build-random-rays : Natural (Listof Ray) -> (Listof Ray))
          ;; build list of random rays through recursion
          (define (build-random-rays k acc)
            (cond
              [(>= k n) acc]
              [else (build-random-rays (+ k 1) (cons (random-ray i j) acc))]))}
         (build-random-rays 0 '())))))

(: n-samples-scalar : Natural -> Float)
;; determine n-samples scalar by which to scale RGB sum
(define (n-samples-scalar n-samples)
  (if (= n-samples 0) 1.0 (/ 1 (->fl n-samples))))

(: antialias-pixel->rgb : Camera (Ray -> RGB) -> Natural Natural -> RGB)
;; given a camera and a ray-tracing function, return a function that
;; traces a list of rays for the given pixel and returns their average
(define (antialias-pixel->rgb cam rt)
  (local
    {(define s (n-samples-scalar (Camera-n-samples cam)))
     (define rays-for-pixel-cam (rays-for-pixel cam))}
     (lambda ([i : Natural] [j : Natural])
       (rgb-scale s
                  (foldl
                   (lambda ([ray : Ray] [rgb-acc : RGB])
                     (rgb+ (rt ray) rgb-acc))
                   rgb-black
                   (rays-for-pixel-cam i j))))))


;;;;; TESTING

(: ray->rgb : Ray -> RGB)
;; a function for testing ray generation.  It maps a ray to a color in
;; the white-to-blue range based on the Y component of the ray's direction
;; vector.
(define (ray->rgb ray)
  (match ray
    [(Ray _ dir)
     (local
       {(define t : Float (* 0.5 (+ 1.0 (Float3-y dir))))}
       (rgb-lerp rgb-white t (RGB 0.5 0.7 1.0)))]))

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide (struct-out Camera))

(provide foreach-pixel
         make-pixel-renderer
         pixel->rgb
         antialias-pixel->rgb
         ray->rgb)
