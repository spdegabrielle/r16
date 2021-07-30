# r16

R16 is a bot for interactive, community-driven code evaluation.
It saves snippets of code, which can then be recalled and executed on user-provided input.

Please see the [documentation](https://docs.racket-lang.org/r16/index.html) for details.

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
3. Initialize a config file (see the docs)
4. `racket <clone_dir>/main.rkt -c config.json`

Alternatively, you can `raco pkg install r16` and run the bot from the install directory, using `raco pkg update` to update.

NOTE: Currently privilege checking is broken (racket-cord bug). Therefore, tricks will only be deletable by their creators (unless you edit the save data manually).
