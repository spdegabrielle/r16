#lang scribble/manual

@(require (for-label racket/base (only-in racket/math natural?) racket/contract))

@title{R16 -- Community-Driven Interactive Code Evaluation}

R16 is a "trick bot". It saves snippets of code, which can then be recalled and executed on user-provided input.

As of the time of writing, there exists one frontend, for Discord. More are planned, so
this manual is structured by frontend.

@section{Configuration}

The bot is run by invoking its @tt{main.rkt} file, and stores its configuration in a JSON
file that is read on startup. See the @tt{-h} output for precise details about
command-line flags.

The configuration file must contain a top level object with the following keys and
associated values:

@itemlist[
@item{@tt{storage}: A string defining the folder in which to store the bot's save data.}
@item{@tt{frontend}: A nested object. Within that object, the @tt{module} key must have a
string value that is a module path as in @racket[dynamic-require] which identifies the frontend to run.
Frontends may require more specific configuration within this object, see the documentation
of your chosen frontend for more details.}
]

A working default configuration for the Discord frontend:
@codeblock|{
{
  "storage": "/home/r16bot/r16_save_data",
  "frontend": {
    "module": "r16/frontends/discord",
    "bot_token": "<your bot token here>"
  }
}
}|

@section{The Trick Environment}

Tricks are stored as plain text, and invoked in a sandbox using @racketmodname[racket/sandbox].

The following values are available in the sandbox's namespace:

@itemlist[
@item{All symbols exported by @racketmodname[threading], for convenience.}
@item{All items listed below.}
@item{Any items made available by the specific frontend, see the documentation of your chosen
frontend for details.}
]

@defproc[(call-trick [name (or/c symbol? string?)]
                     [argument any/c]) any/c]{
Invokes another trick named by @racket[name] and return its result.
If @racket[argument] is @racket[#f], then an empty string is passed to the subtrick. Otherwise, @racket[(~a argument)] is passed.
}

@defthing[string-args string?]{
Text of the message after the bot command, as a string.
}

@defthing[trick-name string?]{
The name of the currently running trick, as a string. For ad-hoc evaluations, the string is empty.
}

@defproc[(read-args) (or/c (listof any/c) #f)]{
Function that returns @racket[string-args], but as a list of datums read by @racket[read]. If there is a read failure, @racket[#f] is returned.
}

@defthing[parent-context (or/c (hash/c symbol? any/c) #f)]{
If the currently executing trick is a top-level trick invocation, @racket[#f]. Otherwise,
a mapping of all the provided symbols (such as @racket[string-args], etc.) in the context
of the parent that executed @racket[call-trick].
}

@section{Discord Frontend}
This frontend provides integration with the Discord chat service.

Commands to the bot are issued by prefixing Discord messages with a configurable prefix
value. The rest of the message is then treated as a command. For more details, send the
message @tt{<bot_prefix> help} in your Discord server.

The frontend also has a configurable trick shorthand prefix. Messages of the form
@tt{<trick_prefix>foo bar ...} are equivalent to @tt{<bot_prefix> call foo bar ...}.

@subsection{Discord Configuration}
The @tt{frontend} object in the configuration file can have the following keys and values:
@itemlist[
@item{@tt{module} must be the string @code{"r16/frontends/discord"}.}
@item{@tt{bot_token} must be a string containing your Discord bot token.}
@item{@tt{bot_prefix} is a string specifying the bot's trigger prefix. If not present, defaults to @code{"!rkt "}.}
@item{@tt{trick_prefix} is a string specifying the bot's shorthand prefix. If not present, defaults to @code{"!!"}.}
]

@subsection{Trick Environment Extensions}

In additional to the bindings described above, the following items are available in the
trick environment.

@defproc[(delete-caller) void?]{
Delete the message that invoked this sandbox.
}

@defproc[(emote-lookup [name string?]) (or/c string? #f)]{
Function that returns the ID for emote with name @racket[name], or @racket[#f] if it doesn't exist.
}

@defproc[(emote-image [id string?]) (or/c bytes? #f)]{
Function that returns the PNG data of the emote with ID @racket[id], or @racket[#f] if it doesn't exist.
}

@defproc[(make-attachment [payload bytes?]
                          [name (or/c string? bytes?)]
                          [mime (or/c symbol? string? bytes?)]) any/c]{
Creates an attachment with payload @racket[payload], filename @racket[name], and MIME-type @racket[mime].
This object must be returned from the trick to be sent to Discord.
If more than one attachment is returned, an unspecified one is sent.
}

@defthing[message-contents string?]{
Full text of the message that invoked this trick.
}

@defproc[(read-storage [type (or/c 'guild 'channel 'user)]) any/c]{
Reads "trick-local storage" @racket[type] and return its result, or @racket[#f] if the result is uninitialized.

A trick's "trick-local storage" can be per-guild, per-channel, or per-user.

This will always return @racket[#f] for the eval command.
}

@defproc[(write-storage [type (or/c 'guild 'channel 'user)]
                        [data any/c]) boolean?]{
Writes @racket[data] to the trick's "trick-local storage," overwriting any existing value, and returns whether the write succeeded. All data supported by @racket[write] can be written.

A trick's "trick-local storage" can be per-guild, per-channel, or per-user; each type of storage has its own limitation on size:
@tabular[#:sep @hspace[1]
  `(,(list @bold{Type} @bold{Size Limit})
          ("guild"     "64 KiB")
          ("channel"   "8 KiB")
          ("user"      "2 KiB"))]
}

This will always be a no-op when invoked from the eval command.

@defproc[(attachment-data [attachment any/c]) bytes?]{
Get the payload of an attachment created with @racket[make-attachment].
}

@defproc[(open-attachment [index natural? 0]) (or/c input-port? #f)]{
Opens the @racket[index]th attachment of the message that invoked this sandbox, as an input port.

Returns @racket[#f] if the message doesn't have an @racket[index]th attachment, or
if the attachment couldn't be opened for any other reason.
}

@defproc[(open-reply-attachment [index natural? 0]) (or/c input-port? #f)]{
Same as @racket[open-attachment], except fetching the attachment of the message that the invoking message replied to.
}

@defthing[attachment-count natural?]{
The number of files attached to the message that invoked this sandbox.
}

@defthing[reply-attachment-count natural?]{
The number of files attached to the message that the invoking message replied to.
}
