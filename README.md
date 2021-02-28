# r16

R16 is a "trick bot" for Discord. It saves snippets of code, which can then be recalled and executed on user-provided input.

## History

The concept of a "trick bot" is not new. The authors were introduced to it by [K9](https://github.com/tterrag1098/K9),
a similar Discord bot written in Clojure.

Despite the authors being avid Clojure fans, K9 had several known security problems due to leaky sandboxing in the libraries
it used. On the other hand, Racket's sandboxing utilities are much more tightly integrated into the runtime environment.
Additionally, the standard Racket installation comes with many more convenient libraries trick authors might want to use.

Thus, R16 was born.

## Name
The R16 name is simply the name "K9", but with "K" shifted to "R" (for "Racket") and the "9" shifted by the same
number to "16".

## Usage
1. Clone this repository
2. `raco pkg install --user --auto --type dir <clone directory>`
3. Put your bot token in a file called `token` in the current working directory
4. `racket r16.rkt`

Tricks will be saved in the current working directory in an unspecified format.
