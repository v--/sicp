#lang sicp

(require support/operation-table)

; Exercise 2.74
;
; Insatiable Enterprises, Inc., is a highly decentralized conglomerate company consisting of a large
; number of independent divisions located all over the world. The company's computer facilities have
; just been interconnected by means of a clever network-interfacing scheme that makes the entire
; network appear to any user to be a single computer. Insatiable's president, in her first attempt
; to exploit the ability of the network to extract administrative information from division files,
; is dismayed to discover that, although all the division files have been implemented as data
; structures in Scheme, the particular data structure used varies from division to division.
; A meeting of division managers is hastily called to search for a strategy to integrate the files
; that will satisfy headquarters' needs while preserving the existing autonomy of the divisions.
;
; Show how such a strategy can be implemented with data-directed programming. As an example, suppose
; that each division's personnel records consist of a single file, which contains a set of records
; keyed on employees' names. The structure of the set varies from division to division. Furthermore,
; each employee's record is itself a set (structured differently from division to division) that
; contains information keyed under identifiers such as address and salary. In particular:
;
; a.  Implement for headquarters a get-record procedure that retrieves a specified employee's record
;   from a specified personnel file. The procedure should be applicable to any division's file.
;   Explain how the individual divisions' files should be structured. In particular, what type
;   information must be supplied?
;
; b.  Implement for headquarters a get-salary procedure that returns the salary information from a
;   given employee's record from any division's personnel file. How should the record be structured
;   in order to make this operation work?
;
; c.  Implement for headquarters a find-employee-record procedure. This should search all the
;   divisions' files for the record of a given employee and return the record. Assume that this
;   procedure takes as arguments an employee's name and a list of all the divisions' files.
;
; d.  When Insatiable takes over a new company, what changes must be made in order to incorporate
;   the new personnel information into the central system?

; Solution

; a.
(define (get-record id file)
  ((get 'get-record (type-tag file)) id (contents file)))

; b.
(define (get-salary record)
  ((get 'get-salary (type-tag record)) (contents record)))

; c.
(define (get-employee-record name division-files)
  (if (null? division-files)
      (error "No such employee")
      (let ([record (get-record name (car division-files))])
        (if (null? record)
            (get-employee-record name (cdr division-files))
            record))))

; d. We must install procedures for get-record and get-salary the new division

(module+ test
  (require rackunit)
  ; For the tests assume two divisions:
  ;  * division-a with mock data
  ;  * division-b with hash table-based files and records

  (define fred-a (attach-tag
                   'division-a-record
                   'mock-record))

  (define file-a (attach-tag
                   'division-a-file
                   'mock-file))

  (define fred-b (attach-tag
                   'division-b-record
                   (make-hash (list [cons 'salary 1000]))))

  (define file-b (attach-tag
                   'division-b-file
                   (make-hash (list [cons 'fred fred-b]))))

  ; a.
  (put 'get-record
       'division-a-file
       (lambda (id file) 'mock-record))

  (put 'get-record
       'division-b-file
       (lambda (id file) (hash-ref file id null)))

  (check-equal? (get-record 'fred file-a)
                'mock-record)

  (check-equal? (get-record 'fred file-b)
                fred-b)

  ; b.
  (put 'get-salary
       'division-a-record
       (lambda (record) 'mock-salary))

  (put 'get-salary
       'division-b-record
       (lambda (record) (hash-ref record 'salary null)))

  (check-equal? (get-salary fred-a)
                'mock-salary)

  (check-equal? (get-salary fred-b)
                1000)

  ; c.

  (check-equal? (get-employee-record 'fred (list file-b file-a))
                fred-b)

  (check-equal? (get-employee-record 'not-fred (list file-b file-a))
                'mock-record))
