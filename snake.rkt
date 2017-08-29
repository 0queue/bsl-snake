;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(define-struct pt [x y])

(define-struct model [apple snake direction width height score])

(define SIM-SPEED .1)
(define SIZE 10)
(define WIDTH 30)
(define HEIGHT 30)
(define FIELD (empty-scene (* SIZE WIDTH) (* SIZE HEIGHT)))

(define START-MODEL
  (make-model 
   (make-pt (/ WIDTH 2) (/ HEIGHT 2))
   (cons (make-pt 1 1) 
         (cons (make-pt 1 2) 
               (cons (make-pt 1 3) empty)))
   (make-pt 1 0)
   WIDTH
   HEIGHT
   0))

(define (put-pt scene point color)
  (underlay/xy scene
               (* SIZE (pt-x point))
               (* SIZE (pt-y point))
               (rectangle SIZE SIZE "solid" color)))

(define (put-snake-head scene point)
  (put-pt scene point "blue"))

(define (put-snake-body scene l)
  (if (cons? l)
      (put-pt
       (put-snake-body scene (rest l))
       (first l)
       "red")
      scene))

(define (put-snake scene l)
  (put-snake-head (put-snake-body scene (rest l)) (first l)))

(define (put-apple scene apple)
  (put-pt scene apple "green"))

(define (render state)
  (put-snake (put-apple FIELD (model-apple state)) (model-snake state)))

(define (move-forward state)
  (make-model
   (model-apple state)
   (cons
    (make-pt (+ (pt-x (model-direction state)) (pt-x (first (model-snake state)))) 
             (+ (pt-y (model-direction state)) (pt-y (first (model-snake state))))) 
    (model-snake state))
   (model-direction state)
   (model-width state)
   (model-height state)
   (model-score state)))

(define (pt= p1 p2)
  (and (= (pt-x p1) (pt-x p2)) 
       (= (pt-y p1) (pt-y p2))))

(define (remove-last l)
  (if (empty? (rest l))
      empty
      (cons (first l) (remove-last (rest l)))))

(define (check-apple apple snake w h)
  (if (check-snake apple snake)
      (new-apple snake w h)
      apple))

(define (new-apple snake w h)
  (check-apple (make-pt (random w) (random h)) snake w h))
  
(define (move-apple state)
  (if (pt= (model-apple state) (first (model-snake state)))
      (make-model
       (new-apple (model-snake state) (model-width state) (model-height state))
       (model-snake state)
       (model-direction state)
       (model-width state)
       (model-height state)
       ( + 1 (model-score state)))
      (make-model
       (model-apple state)
       (remove-last (model-snake state))
       (model-direction state)
       (model-width state)
       (model-height state)
       (model-score state))))

(define (ticker state)
  (move-apple (move-forward state)))

(define (check-field-bounds p w h)
  (or (> 0 (pt-x p))
      (< (- w 1) (pt-x p))
      (> 0 (pt-y p))
      (< (- h 1) (pt-y p))))

(define (check-snake head body)
  (cond
    [(empty? body) false]
    [(pt= head (first body)) true]
    [else (check-snake head (rest body))]))

(define (stopper state)
  (or
   (check-field-bounds (first (model-snake state))
                       (model-width state)
                       (model-height state))
   (check-snake (first (model-snake state))
                (rest (model-snake state)))))

(define (new-dir state a-key)
  (cond
    [(key=? a-key "left") (make-pt -1 0)]
    [(key=? a-key "right") (make-pt 1 0)]
    [(key=? a-key "up") (make-pt 0 -1)]
    [(key=? a-key "down") (make-pt 0 1)]
    [else (model-direction state)]))

(define (keyer state a-key)
  (make-model
   (model-apple state)
   (model-snake state)
   (new-dir state a-key)
   (model-width state)
   (model-height state)
   (model-score state)))

(define (score-screen state)
  (overlay (text (string-append "SCORE: " (number->string (model-score state))) 36 "black")
           (render state)))

(define (main start)
  (big-bang start
            [to-draw render]
            [on-tick ticker SIM-SPEED]
            [stop-when stopper score-screen]
            [on-key keyer]))