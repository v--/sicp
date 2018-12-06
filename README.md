[![Build Status](https://travis-ci.org/v--/sicp.svg?branch=master)](https://travis-ci.org/v--/sicp)

# This repository is dead

As painful as it is for me to abandon something that I put so much effort into, the SICP exercises were simply sucking too much of my time without providing much benefit. I started doing the exercises as part of a study group of a few mildly experienced fellow developers, but it didn't take much time for the people to start realizing that parts of the book, especially the exercises, were often too verbose for somebody that was not completely novice to programming.

# SICP exercises

This is my attempt at comprehensively doing all exercises from the book [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sicp/) ([PDF version](https://github.com/sarabander/sicp-pdf)). It took me a lot of time to reach the end of chapter two before abandoning the all-exercises train.

Each exercise is done using only the theory up to the corresponding point in the book. Exceptions for this rule are allowed for the verification of the solutions, but not for the solutions themselves. That being said, any incompatibilities between Scheme and Racket are resolved in favor of Racket.

If you discover an error or simply have a suggestion, feel free to open an issue or pull request.

# Study group

We formed a study group ([weekly meeting notes](https://github.com/dimitaruzunov/sicp/tree/master/notes), [Slack channel](https://sicp-sofia.slack.com/)) with the goal to discuss the theory, the exercises, various implementation details and personal impressions. It quickly disbanded, however.

# Project structure

All exercises are implemented using a custom language named `sicp`, defined in the [`sicp`](https://github.com/v--/sicp/blob/master/sicp) subfolder. ("custom language" is a bit of an overstatement, it's simply Racket with additional global definitions.)

[`rackunit`](https://docs.racket-lang.org/rackunit/) is used for solution verification.

Some modules from Racket's standard library are occasionally imported in order provide missing definitions.
The [`support`](https://github.com/v--/sicp/blob/master/support) directory contains some more helpers, written Racket.

`make test` runs all the tests, while `make test=$file` runs only the tests in `$file`.

The "Original definitions" sections are definitions that are provided in the book and required by the corresponding exercise.

If there are tests in the exercise description (before the solution), that means that the desired result was provided in the description and (maybe) modified into a proper test.

# Picture language

Section 2.2.4 requires basic drawing operations. To keep things simple and testable, I have tweaked
the given procedures to render to monochrome matrices. See [the basic painter implementation](https://github.com/v--/sicp/blob/master/support/picture-lang.rkt) for more information.

# Travis? Really?

Since most exercises are verified with unit tests, I set up a Travis CI project. Because I can.
