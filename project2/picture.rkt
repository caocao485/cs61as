#lang racket

(require (rename-in graphics/turtles
           (split turtle-split)))
(#%require sicp-pict)

(provide (all-defined-out))

;; Code for integrating Racket's turtle graphics library

(define turtle-x -1)
(define turtle-y -1)
(define turtle-pen-down #f)

(define (cs)
  (turtles #t)
  (clear)
  (set! turtle-x (/ turtle-window-size 2))
  (set! turtle-y (/ turtle-window-size 2))
  (set! turtle-pen-down #f))

(define (penup)
  (set! turtle-pen-down #f))

(define (pendown)
  (set! turtle-pen-down #t))

(define (setxy x y)
  (let ((relative-x (- x turtle-x))
        (relative-y (* -1 (- y turtle-y))))
  (begin (if turtle-pen-down
             (draw-offset relative-x relative-y)
             (move-offset relative-x relative-y))
         (set! turtle-x x)
         (set! turtle-y y))))

(define (draw-line v1 v2)
  (penup)
  (setxy (- (* (xcor-vect v1) turtle-window-size) (/ turtle-window-size 2))
         (- (* (ycor-vect v1) turtle-window-size) (/ turtle-window-size 2)))
  (pendown)
  (setxy (- (* (xcor-vect v2) turtle-window-size) (/ turtle-window-size 2))
         (- (* (ycor-vect v2) turtle-window-size) (/ turtle-window-size 2))))

(define (export filename)
  (save-turtle-bitmap (string->path filename) 'png))

;; Code for the picture language

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
  (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
      (right (right-split painter (- n 1))))
  (let ((top-left (beside up up))
        (bottom-right (below right right))
        (corner (corner-split painter (- n 1))))
    (beside (below painter top-left)
      (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
    (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (identity x) x)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
         (edge1-frame frame))
         (scale-vect (ycor-vect v)
         (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
  ((frame-coord-map frame) (start-segment segment))
  ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
  (painter
   (make-frame new-origin
         (sub-vect (m corner1) new-origin)
         (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
         (make-vect 0.0 1.0)
         (make-vect 1.0 1.0)
         (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
        (make-vect 0.5 0.5)
        (make-vect 1.0 0.5)
        (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
         (make-vect 1.0 0.0)
         (make-vect 1.0 1.0)
         (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
         (make-vect 0.0 0.0)
         (make-vect 0.65 0.35)
         (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
     (transform-painter painter1
            (make-vect 0.0 0.0)
            split-point
            (make-vect 0.0 1.0)))
    (paint-right
     (transform-painter painter2
            split-point
            (make-vect 1.0 0.0)
            (make-vect 0.5 1.0))))
      (lambda (frame)
  (paint-left frame)
  (paint-right frame)))))

;; End of picture language code


;;
;; Your code goes below
;;

;; Exercise 1

(define (up-split painter n)
   (if(= n 0)
     painter
     (let ((smaller (up-split painter (- n 1))))
       (below painter (beside smaller smaller)))))

;; Exercise 2

(define (split major minor)
  (lambda(painter n)
    (if(= n 0)
       painter
       (let ((smaller ((split major minor) painter (- n 1))))
         (major painter(minor smaller smaller)))))
  )

;; Exercise 3

(define (make-vect major minor)
  (cons major minor))

(define xcor-vect
  (lambda(vect)
    (car vect)))

(define ycor-vect
  (lambda(vect)
    (cdr vect)))

(define (add-vect v1 v2)
  (make-vect(+(xcor-vect v1)(xcor-vect v2))
            (+(ycor-vect v1)(ycor-vect v2))))

(define (sub-vect v1 v2)
  (add-vect v1 (scale-vect  -1 v2)))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(define (rotate-vect v angle)
  (let ((c (cos angle))
        (s (sin angle)))
    (make-vect
     (- (* c (xcor-vect v))(* s(ycor-vect v)))
     (+ (* c (ycor-vect v))(* s(xcor-vect v))))))

;; Execise 4

; First definition of make-frame

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame
  (lambda(frame)
    (car frame)))

(define edge1-frame
  (lambda(frame)
    (cadr frame)))

(define edge2-frame
  (lambda(frame)
    (caddr frame)))

; Second definition of make-frame

(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin-frame-2
  (lambda(frame)
    (car frame)))

(define edge1-frame-2
  (lambda(frame)
    (cadr frame)))

(define edge2-frame-2
   (lambda(frame)
    (cddr frame)))

;; Exercise 5

(define make-segment
  (lambda(start end)
    (cons start end)))

(define start-segment
  (lambda(segment)
    (car segment)))

(define end-segment
  (lambda(segment)
    (cdr segment)))

;; Exercise 6
(define outline-painter
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 1.0 0))
    (make-segment (make-vect 1.0 0) (make-vect 1.0 1.0))
    (make-segment (make-vect 1.0 1.0) (make-vect 0 1.0 ))
    (make-segment (make-vect 0 1.0 ) (make-vect 0 0)
                  ))))

(define x-painter
    (segments->painter
     (list (make-segment (make-vect 1.0 1.0) (make-vect 0 0))
           (make-segment (make-vect 0 1.0 ) (make-vect 1.0 0)))))

(define diamond-painter
  (segments->painter
      (list (make-segment (make-vect 0.5 0) (make-vect 1.0 0.5))
            (make-segment (make-vect 1.0 0.5) (make-vect 0.5 1.0))
            (make-segment (make-vect  0.5 1.0) (make-vect  0 0.5))
            (make-segment (make-vect  0 0.5) (make-vect  0.5 0)))))

(define wave-painter
  (segments->painter (list 
                       (make-segment (make-vect .25 0) (make-vect .35 .5)) 
                       (make-segment (make-vect .35 .5) (make-vect .3 .6)) 
                       (make-segment (make-vect .3 .6) (make-vect .15 .4)) 
                       (make-segment (make-vect .15 .4) (make-vect 0 .65)) 
                       (make-segment (make-vect 0 .65) (make-vect 0 .85)) 
                       (make-segment (make-vect 0 .85) (make-vect .15 .6)) 
                       (make-segment (make-vect .15 .6) (make-vect .3 .65)) 
                       (make-segment (make-vect .3 .65) (make-vect .4 .65)) 
                       (make-segment (make-vect .4 .65) (make-vect .35 .85)) 
                       (make-segment (make-vect .35 .85) (make-vect .4 1)) 
                       (make-segment (make-vect .4 1) (make-vect .6 1)) 
                       (make-segment (make-vect .6 1) (make-vect .65 .85)) 
                       (make-segment (make-vect .65 .85) (make-vect .6 .65)) 
                       (make-segment (make-vect .6 .65) (make-vect .75 .65)) 
                       (make-segment (make-vect .75 .65) (make-vect 1 .35)) 
                       (make-segment (make-vect 1 .35) (make-vect 1 .15)) 
                       (make-segment (make-vect 1 .15) (make-vect .6 .45)) 
                       (make-segment (make-vect .6 .45) (make-vect .75 0)) 
                       (make-segment (make-vect .75 0) (make-vect .6 0)) 
                       (make-segment (make-vect .6 0) (make-vect .5 .3)) 
                       (make-segment (make-vect .5 .3) (make-vect .4 0)) 
                       (make-segment (make-vect .4 0) (make-vect .25 0))
                       (make-segment (make-vect 0.40 0.7) (make-vect 0.46 0.7))
                       (make-segment (make-vect 0.53 0.7) (make-vect 0.59 0.7))
                       )))

;; Exercise 7

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0 1.0)
                     (make-vect 0.0 0)
                     (make-vect 1.0 1.0)))

;; Exercise 8

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-below
     (transform-painter painter1
            (make-vect 0.0 0.0)
            (make-vect 1.0 0.0)
            split-point))
    (paint-up
     (transform-painter painter2
            split-point
            (make-vect 1.0 0.5)
            (make-vect 0.0 1.0))))
      (lambda (frame)
  (paint-below frame)
  (paint-up frame)))))

(define (below-2 painter1 painter2)
  (rotate270 (beside (rotate90 painter2)(rotate90 painter1))))

;; Exercise 9

; Modify wave-painter above (Exercise 6)

; Modify me!
(define (corner-split-2 painter n)
  (if (= n 0) 
       painter 
       (beside
        (below painter (up-split painter (- n 1)))
        (below (right-split painter (- n 1))
               (corner-split painter (- n 1)))))) 

; Modify me!
(define (square-limit-2 painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity
                                   flip-horiz)))
    (combine4 (corner-split painter n))))

;; End of project
;; Don't touch anything below this

(define full-frame
  (make-frame (make-vect 0.5 0.5)
              (make-vect 1 0)
              (make-vect 0 1)))
(cs)

(define zero-frame
  (make-frame (make-vect 0 0)
              (make-vect 1 0)
              (make-vect 0 1)))
(define diag-painter
  (segments->painter
   (list (make-segment (make-vect 1 0) (make-vect 0 1))
         (make-segment (make-vect 0 0) (make-vect 1 1)))))

;;(x-painter full-frame)
  
 (define outline 
   (let ((segments '( 
                     ((0 . 0) . (0 .  1)) 
                     ((0 . 1) .  (1 .  1)) 
                     ((1 . 1) .  (1 .  0)) 
                     ((1  . 0) .  (0 . 0)) 
                    ))) 
     (segments->painter segments)))
(clear)

;;(outline-painter full-frame)
;;(x-painter full-frame)
;;(diamond-painter full-frame)
;;(wave-painter full-frame)

;;((corner-split wave-painter 4)full-frame)

;;((square-limit-2 wave-painter 4)full-frame)


(paint einstein)