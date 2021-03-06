#lang slideshow/widescreen

(require slideshow/text)

#| Plan
who we are: 1m
the project & why it exists: 1m
technical details:
  - racket-cord: 1m
  - evaluator: 3m
challenges: 2m
fun stuff: 2m
questions: 2m
future plans: 1m/if time
|#

(define (title)
  (slide
   (big (t "R16"))
   (t "A Discord Bot for Community-Driven, Interactive Code Evaluation")
   (blank-line)
   (t "Racketfest Amateur Night")
   (t "March 27, 2021")
   (t "Presented by: Vincent Lee (williewillus), Alwinfy, Benedek Szilvasby (Eutro)")))

(define (outline)
  "todo")

(define (who-we-are)
  "todo")

(define (about-the-project)
  "todo")

(define (tech-racket-cord)
  "todo")

(define (tech-evaluator)
  "todo")

(define (challenges)
  "todo")

(define (demos)
  "todo")

(define (future-plans)
  "todo")

(define (questions)
  "todo")

(define (main)
  (title)
  (outline)
  (who-we-are)
  (about-the-project)
  (tech-racket-cord)
  (tech-evaluator)
  (challenges)
  (demos)
  (future-plans)
  (questions))

(module* main #f
  (main))
