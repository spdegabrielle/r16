#lang slideshow/widescreen

(require slideshow/text slideshow/play racket/gui)


(define (title)
  (slide
   (big (t "R16"))
   (t "A Discord Bot for Community-Driven, Interactive Code Evaluation")
   (blank-line)
   (t "Racketfest Amateur Night")
   (t "March 27, 2021")
   (t "Presented by: Vincent Lee (williewillus), Alwinfy, Benedek Szilvasy (Eutro)")))

(define (who-we-are)
  (slide
   #:title "Vincent Lee (williewillus)"
   (item "2020 grad from Univ. of Texas at Austin")
   (item "Favorite languages: Clojure and Rust")
   (item "https://www.vincent-lee.net"))
  (slide
   #:title "Alwinfy"
   (item "Makes PRs to random projects")
   (item "Favorite langs: Haskell, most Lisp dialects, Rust btw")
   (item "Hacks in Vim"))
  (slide
   #:title "Benedek Szilvasy (Eutro)"
   (item "TODO eutro")))

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
   (item "Explored Racket as an alternative due to strong built-in sandboxing model"))
  (slide
   #:title "Stats"
   (item "Lines of Code (March 13)")
   (bitmap "codecount.png")))

(define (tech-racket-cord)
  (slide
   #:title "Tech: Racket-Cord"
   (item "The underlying Discord library used by R16")
   (item "Not updated since 2017, many parts have rotted")
   (item "Rewrote the websocket gateway management code")
   (item "Work (slowly) continuing to fix it up")))

(define (tech-evaluator)
  (slide
   #:title "Tech: Evaluator"
   (item "Leverage Racket's powerful sandboxing capabilities (custodians, namespaces, inspectors)")
   (item "Uses " (tt "racket/sandbox") " evaluator with some convenience functions bound into the sandbox")))

(define (challenges)
  (slide
   #:title "Challenges and Limitations"
   (item "One OS thread: easy to lock up the bot with heavy compute, even with time limits")
   (item "Significant time needed to fix racket-cord")
   ; TODO more?
   ))

(define (demos)
  (slide
   #:title "Demo: instant-tshirt"
   (item "Author: Alwinfy")
   'next (item "Full power of" (tt "pict") "and Racket's other functional drawing libraries is available")
   'next (bitmap "tshirt.png"))

  (slide
   #:title "Demo: fractal"
   (item "Author: FreeFull")
   'next (item "Mathematical beauty right there in your Discord #botspam channel")
   'next (item "Use eval to turn any mathematical formula into an \"escape-time fractal\"")
   (scale (bitmap "fractals.png") 0.4 0.4))

  (slide
   #:title "Demo: brainf*ck"
   (item "Author: Eutro")
   'next (item "Your standard interpreter, but that's not the interesting part...")
   'next (item "Other tricks can specify the interpreter as part of their #lang, allowing language-based composition")
   (bitmap "bf.png"))

  (slide
   #:title "Demo: doots"
   (item "Author: Alwinfy")
   'next (item "Primitive sound synthesizer for 8-bit melodies")
   'next (item "Uses a custom 8-bit \"DSL\"")
   'next (scale (bitmap "doots2.png") 0.5 0.5)
   'next (clickback (frame (t "Play audio"))
    (thunk (play-sound "doot.wav" #t)))))

(define (future-plans)
  (slide
   #:title "Future Plans"
   (item "Continue modernization of racket-cord")
   (item "Persistent evaluation contexts")
   (subitem "Reuse the same evaluator state across multiple invocations")
   (subitem "Could be used for chat-based games")))

(define (questions)
  (slide (big (t "Questions?")))
  (slide (big (t "Thanks!"))
         (item "Code: https://git.sr.ht/~williewillus/r16")))

(define (main)
  (title)
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
