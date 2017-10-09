#lang racket/base

(require racket/list
         racket/class
         racket/math
         racket/gui/base
         plot
         pict
         pict/color
         pict/shadow)


(define black (make-object color% "black"))
(define red (make-object color% "crimson"))
(define blue (make-object color% "dodgerblue"))
(define green (make-object color% "seagreen"))
(define white (make-object color% "white"))
(define font (make-object font% 10 'default))
(define brush (new brush% (style 'transparent)))
(define line-pen (new pen% (width 1)))
(define dot-pen-1 (new pen% (width 5) (color red)))
(define dot-pen-2 (new pen% (width 5) (color blue)))
(define dot-pen-highlight (new pen% (width 10) (color green)))
(define text-color black)
(define main-background-color white)
(define secondary-background-color green)
(define border-color black)
(define drk-border-color    (scale-color 0.9 border-color))
(define darken-main-background-color (scale-color 0.9 main-background-color))
(define darken-secondary-background-color (scale-color 0.9 secondary-background-color))
(define lighten-main-background-color (scale-color 2.0 main-background-color))
(define lighten-secondary-background-color (scale-color 2.0 secondary-background-color))

(define poten-canvas%
  (class canvas%
    (init-field parent initial-angle notify-change)
    (super-new (parent parent)
               (min-width 38)
               (min-height 38)
               (stretchable-width #f)
               (stretchable-height #f)
               (style '(transparent)))

    (define dc (send this get-dc))
    (define factor-fast 0.05)
    (define factor-normal 0.005)
    (define factor-slow 0.001)
    (define factor factor-normal)
    (define angle initial-angle)
    (define hovered? #f)
    (define y 0)
    (define frozen-y #f)
    (define frozen-angle #f)

    (send dc set-smoothing 'smoothed)
    (send dc set-font font)

    (define (get-pict clicked? hover? angle)
      (inset
       (shadow
        (rotate
         (ct-superimpose
          (disk 30
                #:color (cond (clicked? lighten-main-background-color)
                              (hover? darken-main-background-color)
                              (else main-background-color))
                #:border-color (if hover?
                                   drk-border-color
                                   border-color))
          (cc-superimpose (blank 15)
                          (disk 7
                                #:color (cond (clicked? lighten-secondary-background-color)
                                              (hover? darken-secondary-background-color)
                                              (else secondary-background-color)))))
         angle)
        6)
       6))

    (define/override (on-char e)
      (case (send e get-key-code)
        ((control) (set! factor factor-fast))
        ((shift)   (set! factor factor-slow))
        ((release)
         (case (send e get-key-release-code)
           ((control shift) (set! factor factor-normal)))))
      (send this refresh))

    (define/override (on-event e)
      (when (send e entering?)
        (set! hovered? #t))
      (when (send e leaving?)
        (set! hovered? #f))
      (when (send e moving?)
        (set! y (send e get-y)))
      (unless (send e get-left-down)
        (set! frozen-y #f)
        (set! frozen-angle #f))
      (when (and (not frozen-y) (not frozen-angle) (send e get-left-down))
        (set! frozen-y (send e get-y))
        (set! frozen-angle angle))
      (when (and frozen-y frozen-angle (send e get-left-down))
        (set-angle (+ frozen-angle (* factor (- y frozen-y)))))
      (when (send e get-right-down)
        (set! frozen-y #f)
        (set! frozen-angle #f)
        (set-angle initial-angle))
      (send this refresh))

    (define/override (on-paint)
      (define pict (get-pict frozen-y hovered? angle))
      (define cx (/ (pict-width pict) 2.0))
      (define cy (/ (pict-height pict) 2.0))
      (draw-pict pict dc (- 19 cx) (- 19 cy)))

    (define/public (get-angle) angle)

    (define/public (set-angle theta)
      (set! angle theta)
      (notify-change theta))))

(define (value->angle x)
  (- (* x (/ (* 2 pi) 10.0))))

(define (angle->value theta)
  (- (* theta (/ 10.0 (* 2 pi)))))

(define poten%
  (class vertical-panel%
    (init-field parent label notify-change initial-value)
    (super-new (parent parent)
               (min-width 100))

    (define (angle-changed theta)
      (define x (angle->value theta))
      (unless (= x value)
        (set-value! x)))

    (define name-message (new message%
                              (parent this)
                              (label label)))
    (define canvas (new poten-canvas%
                        (parent this)
                        (initial-angle (value->angle initial-value))
                        (notify-change angle-changed)))
    (define value-message (new message%
                               (parent this)
                               (font font)
                               (label (string-append (if (not (negative? initial-value)) "+" "") (real->decimal-string initial-value)))
                               (auto-resize #t)))

    (define value initial-value)

    (define/public (get-value) value)

    (define/public (set-value! x)
      (set! value x)
      (send value-message set-label (string-append (if (not (negative? x)) "+" "") (real->decimal-string x)))
      (send canvas set-angle (value->angle x))
      (send canvas refresh)
      (notify-change x))))

(define make-ticks
  (let ((tk (linear-ticks)))
    (lambda (lo hi)
      (map (lambda (t)
             (list (pre-tick-value t) (tick-label t) (pre-tick-major? t)))
           (ticks-generate tk lo hi)))))

(define perceptron-plot%
  (class canvas%
    (init-field parent
                (source-points-1 #f)
                (source-points-2 #f)
                (source-function #f)
                (source-highlight #f)
                (x-min -1)
                (x-max +1)
                (y-min -1)
                (y-max +1)
                (samples 20))

    (super-new (parent parent)
               (style null)
               (min-width 50)
               (min-height 50))

    (send this set-canvas-background main-background-color)

    (define dc (send this get-dc))
    (send dc set-smoothing 'smoothed)
    (send dc set-font font)
    (send dc set-brush brush)
    (send dc set-text-foreground text-color)

    (define width   (send this get-width))
    (define height  (send this get-height))
    (define x-span  (- x-max x-min))
    (define y-span  (- y-max y-min))
    (define x-proj  (/ width x-span))
    (define y-proj  (/ height (- y-span)))
    (define step    (/ x-span samples))
    (define xs      (range x-min (+ x-max step) step))
    (define x-ticks (make-ticks x-min x-max))
    (define y-ticks (make-ticks y-min y-max))

    (define mouse-x #f)
    (define mouse-y #f)

    (define (set-bounds! x-min* x-max* y-min* y-max*)
      (unless (and (= x-min x-min*)
                   (= x-max x-max*))
        (set! x-span  (- x-max* x-min*))
        (set! step    (/ x-span samples))
        (set! x-proj  (/ width x-span))
        (set! xs      (range x-min* (+ x-max* step) step))
        (set! x-ticks (make-ticks x-min* x-max*)))
      (unless (= x-min x-min*)
        (set! x-min x-min*))
      (unless (= x-max x-max*)
        (set! x-max x-max*))
      (unless (and (= y-min y-min*)
                   (= y-max y-max*))
        (set! y-span  (- y-max* y-min*))
        (set! y-proj  (/ height (- y-span)))
        (set! y-ticks (make-ticks y-min* y-max*)))
      (unless (= y-min y-min*)
        (set! y-min y-min*))
      (unless (= y-max y-max*)
        (set! y-max y-max*))
      (send this refresh))

    (define (pan-view! dx dy)
      (define step-x (* dx x-span))
      (define step-y (* dy y-span))
      (set-bounds! (- x-min step-x)
                   (- x-max step-x)
                   (- y-min step-y)
                   (- y-max step-y)))

    (define (zoom-view! dx dy)
      (define step-x (* dx x-span))
      (define step-y (* dy y-span))
      (set-bounds! (+ x-min step-x)
                   (- x-max step-x)
                   (+ y-min step-y)
                   (- y-max step-y)))

    (define/public (set-samples! samples*)
      (unless (or (= samples samples*)
                  (not (exact-positive-integer? samples*)))
        (set! step    (/ x-span samples*))
        (set! xs      (range x-min (+ x-max step) step))
        (set! samples samples*)
        (send this refresh)))

    (define/public (get-samples)
      samples)

    (define/override (on-char e)
      (case (send e get-key-code)
        ;; camera pan
        ((up)    (pan-view! +0.00 +0.05))
        ((down)  (pan-view! +0.00 -0.05))
        ((left)  (pan-view! -0.05 +0.00))
        ((right) (pan-view! +0.05 +0.00))
        ;; camera zoom
        ((#\+ #\=) (cond ((send e get-meta-down)    (zoom-view! +0.05 +0.00))
                         ((send e get-control-down) (zoom-view! +0.00 +0.05))
                         (else                      (zoom-view! +0.05 +0.05))))
        ((#\- #\_) (cond ((send e get-meta-down)    (zoom-view! -0.05 +0.00))
                         ((send e get-control-down) (zoom-view! +0.00 -0.05))
                         (else                      (zoom-view! -0.05 -0.05))))
        ;; sampling control
        ((#\[ #\{) (set-samples! (if (<= samples 2) samples (- samples 1))))
        ((#\] #\}) (set-samples! (+ samples 1)))))

    (define/override (on-event e)
      (cond ((send e dragging?)
             (define x (send e get-x))
             (define y (send e get-y))
             (pan-view! (/ (- x mouse-x) 1000) (/ (- mouse-y y) 1000))
             (set! mouse-x x)
             (set! mouse-y y))
            ((send e button-down?)
             (set! mouse-x (send e get-x))
             (set! mouse-y (send e get-y)))
            ((send e button-up?)
             (set! mouse-x #f)
             (set! mouse-y #f))))

    (define/override (on-paint)
      ;; paint axis
      (send dc set-pen line-pen)
      (when (<= x-min 0 x-max)
        (send dc draw-line
              (* x-proj (- x-min))
              (* y-proj (- y-min y-max))
              (* x-proj (- x-min))
              0)
        (unless (< height 150)
          (for-each (lambda (t)
                      (send dc draw-text (second t)
                            (* x-proj (- x-min))
                            (* y-proj (- (first t) y-max))))
                    y-ticks)))
      (when (<= y-min 0 y-max)
        (send dc draw-line
              0
              (* y-proj (- y-max))
              (* x-proj (- x-max x-min))
              (* y-proj (- y-max)))
        (unless (< width 150)
          (for-each (lambda (t)
                      (send dc draw-text (second t)
                            (* x-proj (- (first t) x-min))
                            (* y-proj (- y-max))))
                    x-ticks)))
      
      (when source-function
        (define path (new dc-path%))
        (send dc set-pen line-pen)
        (send path move-to
              0
              (* y-proj (- (source-function x-min) y-max)))
        (for ((x xs))
          (send path line-to
                (* x-proj (- x x-min))
                (* y-proj (- (source-function x) y-max))))
        (send dc draw-path path))

      (when source-points-1
        (send dc set-pen dot-pen-1)
        (for ((point source-points-1))
          (send dc draw-point
                (* x-proj (- (car point) x-min))
                (* y-proj (- (cdr point) y-max)))))

      (when source-points-2
        (send dc set-pen dot-pen-2)
        (for ((point source-points-2))
          (send dc draw-point
                (* x-proj (- (car point) x-min))
                (* y-proj (- (cdr point) y-max)))))

      (when source-highlight
        (send dc set-pen dot-pen-highlight)
        (for ((point (source-highlight)))
          (send dc draw-point
                (* x-proj (- (car point) x-min))
                (* y-proj (- (cdr point) y-max))))))

    (define/override (on-size new-width new-height)
      (unless (= new-width width)
        (set! x-proj (/ new-width x-span))
        (set! width new-width))
      (unless (= new-height height)
        (set! y-proj (/ new-height (- y-span)))
        (set! height new-height))
      (send this refresh))))

(define w1 1.0)
(define w2 1.0)
(define b 1.0)
(define highlighted #f)

(define (line x)
  (/ (- (+ b (* w1 x))) w2))

(define setosa
 '((5.1 . 3.5) (4.9 . 3.0) (4.7 . 3.2) (4.6 . 3.1) (5.0 . 3.6) (5.4 . 3.9) (4.6 . 3.4) (5.0 . 3.4)
   (4.4 . 2.9) (4.9 . 3.1) (5.4 . 3.7) (4.8 . 3.4) (4.8 . 3.0) (4.3 . 3.0) (5.8 . 4.0) (5.7 . 4.4)
   (5.4 . 3.9) (5.1 . 3.5) (5.7 . 3.8) (5.1 . 3.8) (5.4 . 3.4) (5.1 . 3.7) (4.6 . 3.6) (5.1 . 3.3)
   (4.8 . 3.4) (5.0 . 3.0) (5.0 . 3.4) (5.2 . 3.5) (5.2 . 3.4) (4.7 . 3.2) (4.8 . 3.1) (5.4 . 3.4)
   (5.2 . 4.1) (5.5 . 4.2) (4.9 . 3.1) (5.0 . 3.2) (5.5 . 3.5) (4.9 . 3.6) (4.4 . 3.0) (5.1 . 3.4)
   (5.0 . 3.5) (4.5 . 2.3) (4.4 . 3.2) (5.0 . 3.5) (5.1 . 3.8) (4.8 . 3.0) (5.1 . 3.8) (4.6 . 3.2)
   (5.3 . 3.7) (5.0 . 3.3)))

(define other
 '((7.0 . 3.2) (6.4 . 3.2) (6.9 . 3.1) (5.5 . 2.3) (6.5 . 2.8) (5.7 . 2.8) (6.3 . 3.3) (4.9 . 2.4)
   (6.6 . 2.9) (5.2 . 2.7) (5.0 . 2.0) (5.9 . 3.0) (6.0 . 2.2) (6.1 . 2.9) (5.6 . 2.9) (6.7 . 3.1)
   (5.6 . 3.0) (5.8 . 2.7) (6.2 . 2.2) (5.6 . 2.5) (5.9 . 3.2) (6.1 . 2.8) (6.3 . 2.5) (6.1 . 2.8)
   (6.4 . 2.9) (6.6 . 3.0) (6.8 . 2.8) (6.7 . 3.0) (6.0 . 2.9) (5.7 . 2.6) (5.5 . 2.4) (5.5 . 2.4)
   (5.8 . 2.7) (6.0 . 2.7) (5.4 . 3.0) (6.0 . 3.4) (6.7 . 3.1) (6.3 . 2.3) (5.6 . 3.0) (5.5 . 2.5)
   (5.5 . 2.6) (6.1 . 3.0) (5.8 . 2.6) (5.0 . 2.3) (5.6 . 2.7) (5.7 . 3.0) (5.7 . 2.9) (6.2 . 2.9)
   (5.1 . 2.5) (5.7 . 2.8) (6.3 . 3.3) (5.8 . 2.7) (7.1 . 3.0) (6.3 . 2.9) (6.5 . 3.0) (7.6 . 3.0)
   (4.9 . 2.5) (7.3 . 2.9) (6.7 . 2.5) (7.2 . 3.6) (6.5 . 3.2) (6.4 . 2.7) (6.8 . 3.0) (5.7 . 2.5)
   (5.8 . 2.8) (6.4 . 3.2) (6.5 . 3.0) (7.7 . 3.8) (7.7 . 2.6) (6.0 . 2.2) (6.9 . 3.2) (5.6 . 2.8)
   (7.7 . 2.8) (6.3 . 2.7) (6.7 . 3.3) (7.2 . 3.2) (6.2 . 2.8) (6.1 . 3.0) (6.4 . 2.8) (7.2 . 3.0)
   (7.4 . 2.8) (7.9 . 3.8) (6.4 . 2.8) (6.3 . 2.8) (6.1 . 2.6) (7.7 . 3.0) (6.3 . 3.4) (6.4 . 3.1)
   (6.0 . 3.0) (6.9 . 3.1) (6.7 . 3.1) (6.9 . 3.1) (5.8 . 2.7) (6.8 . 3.2) (6.7 . 3.3) (6.7 . 3.0)
   (6.3 . 2.5) (6.5 . 3.0) (6.2 . 3.4) (5.9 . 3.0)))

(define dataset
  (append (map (lambda (inputs) (cons inputs +1)) setosa)
          (map (lambda (inputs) (cons inputs -1)) other)))

(define (sign s)
  (cond ((> s 0) +1)
        ((< s 0) -1)
        (else     0)))

(define (x1 inputs) (car inputs))
(define (x2 inputs) (cdr inputs))
(define (entry-inputs entry) (car entry))
(define (entry-outcome entry) (cdr entry))

(define (predict inputs)
  (sign (+ (* w1 (x1 inputs))
           (* w2 (x2 inputs))
           b)))

(define (train!)
  (define (iterate entries misclassified)
    (if (null? entries)
        (if (zero? misclassified)
            (stop!)
            (iterate dataset 0))
        (let* ((entry (first entries))
               (inputs (entry-inputs entry))
               (outcome (entry-outcome entry)))
          (set! highlighted inputs)
          (send p refresh)
          (cond ((not (= (predict inputs) outcome))
                 (send c1 set-value! (+ w1 (* outcome (x1 inputs))))
                 (send c2 set-value! (+ w2 (* outcome (x2 inputs))))
                 (send c3 set-value! (+ b outcome))
                 (unless stop-algorithm?
                   (sleep 0.05)
                   (iterate (rest entries) (+ misclassified 1))))
                (else
                 (unless stop-algorithm?
                   (iterate (rest entries) misclassified)))))))
  (iterate dataset 0))

(define frame (new frame%
                   [label "Perceptron"]
                   [width 800]
                   [height 600]))
(define panel (new horizontal-panel%
                   [parent frame]
                   [spacing 10]
                   [border 10]))

(define controls (new vertical-panel%
                      [parent panel]
                      [spacing 10]
                      [border 0]
                      [stretchable-width #f]))

(define p (new perceptron-plot%
               (parent panel)
               (source-function line)
               (source-points-1 setosa)
               (source-points-2 other)
               (source-highlight (lambda () (if highlighted (list highlighted) null)))
               (x-min 3) (x-max 9)
               (y-min 1) (y-max 5)))

(define c1 (new poten%
                (parent controls)
                (label "w1")
                (notify-change (lambda (c)
                                 (set! w1 c)
                                 (send p refresh)))
                (initial-value w1)))

(define c2 (new poten%
                (parent controls)
                (label "w2")
                (notify-change (lambda (c)
                                 (set! w2 c)
                                 (send p refresh)))
                (initial-value w2)))

(define c3 (new poten%
                (parent controls)
                (label "b")
                (notify-change (lambda (c)
                                 (set! b c)
                                 (send p refresh)))
                (initial-value b)))

(define b-reset (new button%
                     (parent controls)
                     (label "reset")
                     (stretchable-width #t)
                     (callback (lambda (b e) (reset!)))))

(define b-play (new button%
                    (parent controls)
                    (label "play")
                    (stretchable-width #t)
                    (callback (lambda (b e) (play!)))))

(define b-stop (new button%
                    (parent controls)
                    (enabled #f)
                    (label "stop")
                    (stretchable-width #t)
                    (callback (lambda (b e) (stop!)))))

(define stop-algorithm? #f)

(define (reset!)
  (send c1 set-value! 1.0)
  (send c2 set-value! 1.0)
  (send c3 set-value! 1.0)
  (set! highlighted #f)
  (set! stop-algorithm? #f)
  (send b-stop enable #f)
  (send b-play enable #t))

(define (stop!)
  (set! stop-algorithm? #t)
  (send b-stop enable #f)
  (send b-reset enable #t)
  (send b-play enable #t))

(define (play!)
  (set! stop-algorithm? #f)
  (send b-stop enable #t)
  (send b-reset enable #f)
  (send b-play enable #f)
  (thread train!))

(send frame show #t)
               
    
