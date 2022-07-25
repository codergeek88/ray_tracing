#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 2
;; camera.rkt module
;; Kishan Patel
;; TESTS ARE IN camera-tests.rkt
;; This module implements the Camera abstraction
;;

;; load custom definitions
;;
(require "../include/cs151-core.rkt")

;; load image library definitions
;;
(require "../include/cs151-image.rkt")

;; project modules
;;
(require "math-util.rkt")
(require "color.rkt")

;; the representation of a Camera for the ray tracer.
(define-struct Camera
  [(wid : Natural)           ;; width of image
   (ht : Natural)            ;; height of image
   (n-samples : Natural)     ;; number of samples per pixel
   (origin : Float3)         ;; where the camera is located
   (ulc : Float3)            ;; upper-left-corner of image plane
   (h-vec : Float3)          ;; horizontal pixel-wide vector parallel to image
                             ;; pointing right
   (v-vec : Float3)])        ;; vertical pixel-wide vector parallel to image
                             ;; pointing down

(: simple-camera : Natural Natural Natural Float -> Camera)
;; make a camera that is equivalent to a Project 1 camera (for
;; testing purposes)
(define (simple-camera wid ht ns flen)
  (local
    {(define pw : Float (/ 2.0 (->fl wid)))}
    (Camera wid ht ns
            fl3-zero
            (Float3 -1.0
                    (/ (->fl ht) (->fl wid))
                    (- flen))
            (Float3 pw 0.0 0.0)
            (Float3 0.0 (- pw) 0.0))))

;; make-camera TESTS ARE IN camera-tests.rkt
(: make-camera : Natural Natural Natural Float3 Float3 Float3 Float -> Camera)
;; make a camera.  The arguments are (in order):
;;   - width of image
;;   - height of image
;;   - number of samples per pixel
;;   - origin of camera in the world
;;   - point that the camera is looking at
;;   - up vector
;;   - horizontal field of view (in degrees)
(define (make-camera wid ht ns pos look-at up fov)
  (local
    {(define D (fl3-normalize (fl3- look-at pos)))
     (define R (fl3-normalize (fl3-cross D up)))
     (define U-new (fl3-normalize (fl3-cross R D)))
     (define pw (/ 2 (->fl wid)))
     (define H (fl3-scale pw R))
     (define V (fl3-scale (- pw) U-new))
     (define theta (degrees->radians (/ fov 2)))
     (define flen (/ 1 (tan theta)))
     (define C (fl3-scale+ pos flen D))
     (define ulc (fl3-
                  (fl3-scale+ C (/ (->fl ht) wid) U-new)
                  R))}
    (Camera wid ht ns pos ulc H V)))
;; make-camera TESTS ARE IN camera-tests.rkt
  

;; A Pixel-Renderer is a function that takes the row and column of a pixel
;; and produces a Racket Image-Library Color
(define-type Pixel-Renderer (Natural Natural -> Color))

(: foreach-pixel : Camera Pixel-Renderer -> Image)
;; given a camera and a pixel renderer, generate an image.
;;
(define (foreach-pixel cam pixel-renderer)
  (match cam
    [(Camera wid ht _ _ _ _ _)
     (if (or (= wid 0) (= ht 0))
         empty-image
         (local
           {(: for-rows : Natural (Listof Color) -> (Listof Color))
            ;; iterate over the rows of the image from bottom to top
            (define (for-rows row pixels)
              (if (= 0 row)
                  pixels
                  (for-cols (- row 1) wid pixels)))
            (: for-cols :  Natural Natural (Listof Color) -> (Listof Color))
            ;; iterate over the columns of a row from right to left
            (define (for-cols row col pixels)
              (if (= 0 col)
                  (for-rows row pixels)
                  (for-cols
                   row
                   (- col 1)
                   (cons (pixel-renderer row (- col 1)) pixels))))}
           (color-list->bitmap
            (for-rows ht '())
            wid ht)))]))

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
     (define v-vec (Camera-v-vec cam))
     (define h-vec (Camera-h-vec cam))
     (define origin (Camera-origin cam))
     (define ul-center (fl3-scale+ (Camera-ulc cam) 0.5 (fl3+ v-vec h-vec)))}
     (lambda ([i : Natural] [j : Natural])
       (make-ray origin
                 (fl3-
                  (fl3-scale+
                   (fl3-scale+ ul-center (->fl i) v-vec)
                   (->fl j)
                   h-vec)
                  origin)))))

(: rays-for-pixel : Camera -> (Natural Natural -> (Listof Ray)))
;; given a camera, return a function that maps pixel coordinates in
;; the image plane to a list of rays from the camera through the pixel.
;; The number of rays is determined by the n-samples field of the
;; Camera.
(define (rays-for-pixel cam)
  (local
    {(define w (->fl (Camera-wid cam)))
     (define h (->fl (Camera-ht cam)))
     (define ulc (Camera-ulc cam))
     (define origin (Camera-origin cam))
     (define n (Camera-n-samples cam))

     (: random-ray : Natural Natural -> Ray)
     ;; generate a random ray in a pixel of given row and column
     (define (random-ray i j)
       (local
         ;; define row and column offset 
         {(define i-off (random))
          (define j-off (random))
          ;; calculate head of vector
          (define head (fl3-scale+
                        (fl3-scale+ ulc (+ i i-off) (Camera-v-vec cam))
                        (+ j j-off)
                        (Camera-h-vec cam)))}
         (make-ray origin (fl3- head origin))))}

     (lambda ([i : Natural] [j : Natural])
       (local
         {(: build-random-rays : Natural (Listof Ray) -> (Listof Ray))
          ;; build list of random rays through recursion
          (define (build-random-rays k acc)
            (cond
              [(>= k n) acc]
              [else (build-random-rays (+ k 1) (cons (random-ray i j) acc))]))}
         (build-random-rays 0 '())))))

(: pixel->rgb : Camera (Ray -> RGB) -> Natural Natural -> RGB)
;; given a camera and a ray-tracing function, return a function that
;; traces the ray for the given pixel
(define (pixel->rgb cam rt)
  (local
    {(define ray-for-pixel-cam (ray-for-pixel cam))}
    (lambda ([i : Natural] [j : Natural])
      (rt (ray-for-pixel-cam i j)))))


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

(provide Camera)

(provide make-camera
         simple-camera
         foreach-pixel
         make-pixel-renderer
         pixel->rgb
         antialias-pixel->rgb
         ray->rgb)
