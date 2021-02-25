# r16

R16 is a "trick bot" for Discord. It saves snippets of code, which can then be recalled and executed on user-provided input.

It is inspired by K9's trick system, but uses Racket instead of Clojure due to the former's stronger sandboxing capabilities.

NOTE: The list of administrators is currently hardcoded. Please change it in r16.rkt before running your own instance.

## Usage
1. Clone this repository
2. `raco pkg install --user --auto --type dir <clone directory>`
3. Put your bot token in a file called `token` in the clone directory (ignored by Git)
4. `racket r16.rkt`
