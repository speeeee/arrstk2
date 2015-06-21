<unnamed> - A stack-based language focused on support for building languages on.
====

#What is it?

<unnamed> is a stack-based language meant to be used for building domain-specific languages.  It is based off of the creation of *modes*, which are structures that hold rules, which are checked against any code written when the mode is active.  The checking happens at compile-time, so run-time is usually not affected by the complexity of different modes, but the length of time it takes to compile may vary. 

#What is it made in?

ULCL is made in Racket.  It compiles to C, though right now only works as a REPL.

#Documentation

Documentation is in DOCS.md.  The documentation is unfinished as of now, but will be improved in the future.

#Status

Not currently working on it; may come back.