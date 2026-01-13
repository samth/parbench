# recspecs Bug Report: File Corruption with Multiple @expect Forms

## Summary

When using `RECSPECS_UPDATE=1` to update multiple `@expect` forms in the same file, recspecs corrupts the file by miscalculating positions/spans, causing code and expectation strings to be merged incorrectly.

## Environment

- Racket 9.0
- recspecs (installed from package catalog, checksum 5858e3b70d804000765baed8c9b6b3c15094a544)
- Linux

## Reproduction Steps

1. Create a test file with multiple `@expect` forms that use output filters:

```racket
#lang at-exp racket
(require rackunit rackunit/text-ui recspecs)

(define (normalize s)
  (regexp-replace* #px"[0-9]+" s "N"))

(define tests
  (test-suite "multi-expect-complex"
    (test-case "help output"
      (parameterize ([recspecs-output-filter string-trim])
        @expect[(system "echo 'usage: prog [options]'; echo '  --help Show help'; echo '  --version Show version'" #:set-pwd? #f)]{wrong help
}))
    (test-case "list output"
      (parameterize ([recspecs-output-filter string-trim])
        @expect[(system "echo 'Available items:'; echo '  item1, item2, item3'" #:set-pwd? #f)]{wrong list
}))
    (test-case "run output"
      (parameterize ([recspecs-output-filter normalize])
        @expect[(system "echo 'Running...'; echo '  task1'; echo 'Result: 42'" #:set-pwd? #f)]{wrong run
}))))

(module+ test
  (run-tests tests))
```

2. Run with update mode:
```bash
RECSPECS_UPDATE=1 raco test repro.rkt
```

3. The file is corrupted:

```racket
#lang at-exp racket
(require rackunit rackunit/text-ui recspecs)

(define (normalize s)
  (regexp-replace* #px"[0-9]+" s "N"))

(define tests
  (test-suite "multi-expect-complex"
    (test-case "help output"
      (parameterize ([recspecs-output-filter string-trim])
        @expect[(system "echo 'usage: prog [options]'; echo '  --help Show help'; echo '  --version Show version'" #:set-pwd? #f)]{usage: prog [options]
  --help Show help
  --version Show version
}))
    (test-case "list output"
      (parameterize ([recspecs-output-filter string-trim])
        @expect[(system "echo 'Available Available items:
  item1, item2, item3cho '  item1, item2, item3'" #:set-pwd? #f)]{wrong list
}))
    (test-case "run output"
      (parameterize ([recspecs-output-filter normalize])
        @expRunning...
  taskN
Result: N
em "echo 'Running...'; echo '  task1'; echo 'Result: 42'" #:set-pwd? #f)]{wrong run
}))))

(module+ test
  (run-tests tests))
```

## Observed Corruption

1. **First expectation**: Updated correctly
2. **Second expectation**: Code corrupted - `"echo 'Available` became `"echo 'Available Available items:\n  item1, item2, item3cho '  item1`
3. **Third expectation**: Severely corrupted - `@expect` became `@exp` with the expected output inserted into the middle of the Racket code

## Expected Behavior

Each `@expect` form should be updated independently without affecting other parts of the file.

## Root Cause Hypothesis

When multiple expectations are updated in sequence, the position/span information for subsequent expectations becomes stale after the first update changes the file length. The updates appear to be applied using the original positions without accounting for how earlier updates shifted the file contents.

## Workaround

Update expectations one at a time using `RECSPECS_UPDATE_TEST`:

```bash
RECSPECS_UPDATE_TEST="help output" raco test repro.rkt
RECSPECS_UPDATE_TEST="list output" raco test repro.rkt
RECSPECS_UPDATE_TEST="run output" raco test repro.rkt
```

## Note

The bug does NOT occur with simple `@expect` forms that don't use `parameterize` with `recspecs-output-filter`. A minimal test with just `(displayln "hello")` works correctly. The bug appears related to the combination of:
- Multiple `@expect` forms
- Use of `parameterize` with output filters
- Longer/multi-line expected outputs

## Root Cause (from code analysis)

In `recspecs-lib/main.rkt`, the `update-file` function (lines 141-168):

```racket
(define (update-file path pos span new-str)
  (define bs (file->bytes path))
  (define start (sub1 pos))
  (define before (subbytes bs 0 start))
  ...
  (call-with-output-file path #:exists 'truncate/replace ...))
```

The problem:
1. `pos` and `span` are determined at **compile time** from the original file
2. Each `run-expect` call (line 251) immediately calls `update` when the test fails
3. After the first update, the file on disk has changed length
4. Subsequent expectations still use their original `pos`/`span` values
5. These positions now point to the wrong locations in the modified file

## Proposed Fix

**Option A: Apply updates in reverse order (simplest)**

Collect all pending updates during test execution, then apply them sorted by position descending (from end of file to start). This way, earlier updates don't shift the positions of later ones.

```racket
;; Add a pending-updates parameter
(define pending-updates (make-parameter '()))

;; In run-expect, instead of immediately calling update:
(cond
  [(and path (update-mode? name) (not equal?))
   (pending-updates (cons (list path pos span actual update) (pending-updates)))
   (commit-expectation! e)
   (printf "Queued update for ~a\n" path)]
  ...)

;; Add a function to apply all updates
(define (apply-pending-updates!)
  (define updates (pending-updates))
  ;; Group by file, sort each group by position descending
  (define by-file (group-by car updates))
  (for ([file-updates (in-list by-file)])
    (define sorted (sort file-updates > #:key cadr))
    (for ([u (in-list sorted)])
      (match-define (list path pos span actual update-fn) u)
      (update-fn path pos span actual)
      (printf "Updated expectation in ~a\n" path)))
  (pending-updates '()))

;; Call apply-pending-updates! at end of test run (e.g., via exit handler or test-suite wrapper)
```

**Option B: Adjust positions after each update**

Track the cumulative offset from all previous updates and adjust positions accordingly. More complex but allows immediate feedback.

**Option C: Use markers instead of positions**

Instead of byte positions, use unique markers or content hashes to locate the expectation string to replace. More robust but requires more significant changes.
