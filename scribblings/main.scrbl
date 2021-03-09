#lang scribble/manual

@(require (for-label racket/base))

@title{R16 - A Discord Trick Bot}

R16 is a "trick bot" for Discord. It saves snippets of code, which can then be recalled and executed on user-provided input.

@section{The Trick Environment}

Tricks are stored as plain text, and invoked in a sandbox from @racket[racket/sandbox].

The following symbols are available in the trick context:

All symbols from the @racket[threading-lib] package are available for convenience.

@defproc[(make-attachment [payload bytes?]
                          [name (or/c string? bytes?)]
                          [mime (or/c symbol? string? bytes?)]) any/c]{
Creates an attachment with payload @racket[payload], filename @racket[name], and MIME-type @racket[mime].
This opaque object must be returned from the trick to be sent to Discord.
If more than one attachment is returned, an unspecified one is sent.                      
} 

@defproc[(call-trick [name (or/c symbol? string?)]
                     [argument any/c]) any/c]{
Invokes another trick named by @racket[name] and return its result.
If @racket[argument] is @racket[#f], then an empty string is passed to the subtrick. Otherwise, @racket[(~a argument)] is passed.
}

@defthing[message-contents string?]{
Full text of the message that invoked this trick.
}

@defthing[string-args string?]{
Text of the message after the bot command, as a string.
}

@defproc[(read-args) (or/c (listof any/c) #f)]{
Function that returns @racket[string-args], but as a list of datums read by @racket[read]. If there is a read failure, @racket[#f] is returned.
}

@defproc[(emote-lookup [name string?]) (or/c string? #f)]{
Function that returns the ID for emote with name @racket[name], or @racket[#f] if it doesn't exist.
}

@defproc[(emote-image [id string?]) (or/c bytes? #f)]{
Function that returns the PNG data of the emote with ID @racket[id], or @racket[#f] if it doesn't exist.
}

@defproc[(delete-caller) void?]{
Thunk that deletes the message that invoked this sandbox.
} 

@defthing[parent-context (or/c (hash/c symbol? any/c) #f)]{
Mapping of all the above symbols for the trick calling this one, or @racket[#f] if this trick is the top level invocation.
} 
