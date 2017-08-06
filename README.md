[![Build Status](https://travis-ci.org/v--/sicp.svg?branch=master)](https://travis-ci.org/v--/sicp)

# SICP exercises

This is my attempt at doing all exercises from the book [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sicp/) ([PDF version](https://github.com/sarabander/sicp-pdf)).

Each exercise is done using only the theory up to the corresponding point in the book. Exceptions for this rule are allowed for the verification of the solutions, but not for the solutions themselves. That being said, any incompatibilities between Scheme and Racket are resolved in favor of Racket.

If you discover an error or simply have a suggestion, feel free to open an issue or pull request.

# Study group

We formed a study group ([weekly meeting notes](https://github.com/dimitaruzunov/sicp/tree/master/notes), [Slack channel](https://sicp-sofia.slack.com/)) with the goal to discuss the theory, exercises, implementation details and personal impressions.

# Project structure

All exercises are implemented using a custom language named `sicp`, defined in the [`sicp`](https://github.com/v--/sicp/blob/master/sicp) subfolder.

[`rackunit`](https://docs.racket-lang.org/rackunit/) is used for solution verification.

Some modules from Racket's standard library are occasionally imported in order provide missing definitions.
The [`support`](https://github.com/v--/sicp/blob/master/support) directory contains some more helpers, written Racket.

`make test` runs all the tests, while `make test=$file` runs only the tests in `$file`.

The "Original definitions" sections are definitions that are provided in the book and required by the corresponding exercise.

# Travis? Really?

Since most exercises are verified with unit tests, I set up a Travis CI project. Because I can.
