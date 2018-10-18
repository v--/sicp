#lang sicp

; Exercise 2.76
;
; As a large system with generic operations evolves, new types of data objects or new operations may
; be needed. For each of the three strategies -- generic operations with explicit dispatch,
; data-directed style, and message-passing-style -- describe the changes that must be made to a
; system in order to add new types or new operations. Which organization would be most appropriate
; for a system in which new types must often be added? Which would be most appropriate for a system
; in which new operations must often be added?

; Solution
; The main reason to prefer one approach to another is to abstract away the actual complexity of
; having many data types and/or operations.
;
; Explicit dispatch:
;   Adding data types:
;     Requires exposing all new procedures under new unique names.
;   Adding operations:
;     Requires exposing all new procedures under new unique names.
;   Notes:
;     + Explicit and insanely simple to reason about.
;     - Requires us to take extra care not to mix up or forget to implement all new procedures upon
;       adding either a new data type or a new operation.
;     - Requires us to know how the current data object is represented at all times, thus preventing
;       us from using truly generic operations.
;
; Data-directed dispatch:
;   Adding data types:
;     Requires implementing an "installer" procedure for putting the new operations in the table.
;   Adding operations:
;     Requires adding procedures for each operation to the table.
;   Notes:
;     + Standardizes a way of adding new data types and new operations, thus reducing boilerplate code
;     - Most often requires explicitly type-tagging the data
;
; Message passing:
;   Adding data types:
;     Requires adding a new procedure representing the data type.
;   Adding operations:
;     Requires going through all data types and adding more functionality into the existing
;     procedures.
;   Notes:
;     + Makes adding new data types a matter of writing a single procedure.
;     - The procedures representing the data types easily become large and messy.
;     - Makes adding new operations cumbersome.
;
; Verdict: adding new operations should be easiest when using data-directed dispatch and adding
; new data types should be easiest when using message passing. The data-directed approach
; seems to be a safe bet for most use cases.
