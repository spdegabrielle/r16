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
   (t "Presented by: Vincent Lee (williewillus), Alwinfy, Benedek Szilvasy (Eutro)")))

(define (outline)
  "todo")

(define (who-we-are)
  (slide
   #:title "Vincent Lee (williewillus)"
   (item "New grad from Univ. of Texas at Austin")
   (item "Favorite languages: Clojure and Rust"))
  (slide
   #:title "Alwinfy"
   (item "TODO"))
  (slide
   #:title "Benedek Szilvasy (Eutro)"
   (item "TODO")))

(define (about-the-project)
  (slide
   #:title "What Is R16?"
   (item "In one sentence: A Discord Bot for Communal Code Evaluation")
   'next
   (item "Focus is on" (it "tricks") ", short saved snippets of code")
   'next
   (item "Users can register code as a trick, then recall and execute that code on separate input")
   (bitmap "helloworld.png"))
  (slide
   #:title "Inspiration"
   (item "A very similar preexisting bot called K9, written by tterrag")
   (item "Written in Java, trick language is Clojure")
   (item "TODO")))

(define (tech-racket-cord)
  "todo")

(define (tech-evaluator)
  "todo")

(define (challenges)
  (slide
   #:title "Challenges and Limitations"
   (item "One OS thread: easy to lock up the bot with heavy compute, even with time limits")
   (item "Sandbox works" (it "too") "well")))

(define (demos)
  "todo")

(define (future-plans)
  (slide
   #:title "Future Plans"
   (item "Continue modernization of racket-cord")
   (item "Persistent evaluation contexts")))

(define (questions)
  (slide (big (t "Questions?")))
  (slide (big (t "Thanks!"))
         (item "Code: https://git.sr.ht/~williewillus/r16")))

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
