[![Build Status](https://travis-ci.org/v--/sicp.svg?branch=master)](https://travis-ci.org/v--/sicp)

# SICP exercises

This is my attempt at doing all exercises from the book [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sicp/) ([PDF version](https://github.com/sarabander/sicp-pdf)).

Each exercise is done using only the theory up to the corresponding point in the book. Exceptions for this rule are allowed for the verification of the solutions, but not for the solutions themselves.

If you discover an error or simply have a suggestion, feel free to open an issue or pull request.

# Used tools

The `sicp` language from Racket's [SICP Collections](https://docs.racket-lang.org/sicp-manual/) is used as an implementation language.

[`rackunit`](https://docs.racket-lang.org/rackunit/) is used for solution verification.

Finally, some modules from Racket's standard library are occasionally imported in order provide missing definitions.
The [`support`](https://github.com/v--/sicp/blob/master/support) directory contains some more helpers, written Racket.

To run the tests, refer to the [Travis CI](https://github.com/v--/sicp/blob/master/.travis.yml) configuration file.

# Travis? Really?

Since most exercises are verified with unit tests, I set up a Travis CI project. Because I can.
