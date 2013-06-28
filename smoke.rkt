#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require (only-in racket/draw read-bitmap bitmap-dc%))

(define HEIGHT 640)
(define WIDTH 480)
(define START-SIZE .5)
(define END-SIZE 4)
(define ROTATE-STEP .2)
(define END-LIFE 100)
(define SIZE-STEP (/ (- END-SIZE START-SIZE) END-LIFE))
(define SMOKE-INTERVAL 3)
(define BACKGROUND (rectangle WIDTH HEIGHT "solid" "black"))

(define SMOKE-IMGS
  (for/hash ([i 100]) 
    (values i (read-bitmap (string-append "imgs/" (number->string i) ".png")))))

;(define SMOKE-IMG (read-bitmap "smoke.png"))

(define (fade img pct)
  (define p (bytes 0 0 0 0))
  (for* ([x (image-width img)] [y (image-height img)])
    (send img get-argb-pixels x y 1 1 p)
    (define p-list (bytes->list p))
    (define new-bytes (list->bytes 
                       (cons (inexact->exact (round (* pct (car p-list))))
                             (cdr p-list))))
    (send img set-argb-pixels x y 1 1 new-bytes))
  img)

;; A Smoke is a 
(struct Smoke (life x xvel y yvel angle size))

(define (mk-random-init-smoke) 
  (Smoke 0                ; life
         (/ WIDTH 2)      ; x
         (* 0.5 (- (random) 0.5)) ; xvel
         0                ; y
         (* 1.5 (random))         ; yvel
         (* 359 (random)) ; angle
         START-SIZE       ; size
         ))

;; A World is a (W ticks [Listof Smoke])
(struct W (t smokes))

(define (render w)
  (define smokes (W-smokes w))
  (foldl (λ (s img) 
           (place-image 
;            (rotate (Smoke-angle s)
                    (scale (Smoke-size s) 
                           (hash-ref SMOKE-IMGS 
                                     (round (- 99 (* 100 (/ (Smoke-life s) END-LIFE))))))
            (Smoke-x s)
            (- HEIGHT (Smoke-y s))
            img))
         BACKGROUND
         smokes))

(define (tick-angle angle)
  (define new-angle (+ angle ROTATE-STEP))
  (if (> new-angle 360)
      (- new-angle 360)
      new-angle))

(define (tick-smoke s)
  (match-define (Smoke life x xvel y yvel angle size) s)
  (Smoke (add1 life)
         (+ x xvel) xvel
         (+ y yvel) yvel 
         (tick-angle angle)
         (+ size SIZE-STEP)))

(define (live-smoke? s) (< (Smoke-life s) END-LIFE))

(define (tick w) 
  (define ticks (add1 (W-t w)))
  (define smokes (filter live-smoke? (map tick-smoke (W-smokes w))))
  (if (zero? (modulo ticks SMOKE-INTERVAL))
      (W ticks (cons (mk-random-init-smoke) smokes))
      (W ticks smokes)))

(big-bang (W 0 null)
          (to-draw render)
          (on-tick tick))

