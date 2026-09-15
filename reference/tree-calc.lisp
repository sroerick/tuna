;;;; tree-calc.lisp — Tuna's Common Lisp reference twin.
;;;;
;;;; Faithful port of the upstream triage-calculus `apply` (verbatim
;;;; structure of reference/tree-calculus/implementation/ocaml/lib/tree.ml
;;;; and of tuna's interpreter/lib/eval.ml) plus the same fuel /
;;;; size_cap budgeting, so that step totals and exhaustion statuses are
;;;; directly comparable with the OCaml side.
;;;;
;;;; Counting convention (AGENTS.md rule 4):
;;;;   - a *step* is one firing of a triage rule — the three Fork cases
;;;;     fork(leaf,x), fork(stem,_), fork(fork,_);
;;;;   - the two wrapper applications (apply Leaf b = Stem b,
;;;;     apply (Stem a) b = Fork (a, b)) are application, not steps;
;;;;   - strategy is leftmost-innermost in exactly the order the OCaml
;;;;     reference evaluates: inner a1, then a2, then the outer.
;;;;
;;;; Usage: sbcl --script reference/tree-calc.lisp <corpus-file>
;;;; Reads the corpus file (see scripts/diff-corpus/*.corpus), evaluates
;;;; program against args under fuel/size_cap, prints one line:
;;;;   <status> <result-or--> <steps>
;;;; where status is normal | fuel_exhausted | size_exhausted.

(defpackage #:tree-calc
  (:use #:cl))
(in-package #:tree-calc)

(declaim (optimize (speed 3) (safety 1)))

;;; ------------------------------------------------------------------
;;; Tree representation (tagged conses):
;;;   Leaf     = 0 (the fixnum 0)
;;;   Stem a   = (1 . a)
;;;   Fork a b = (2 a . b)  i.e. (cons 2 (cons a b))
;;; ------------------------------------------------------------------

(defconstant +leaf+ 0)

(declaim (inline leaf-p stem-p fork-p stem-child fork-left fork-right))
(defun leaf-p (x) (eql x 0))
(defun stem-p (x) (and (consp x) (eql (car x) 1)))
(defun fork-p (x) (consp x))
(defun stem-child (x) (cdr x))
(defun fork-left (x) (cadr x))
(defun fork-right (x) (cddr x))

(declaim (inline tree-size))
(defun tree-size (x)
  (declare (type (or fixnum cons) x))
  (cond
    ((leaf-p x) 1)
    ((stem-p x) (1+ (tree-size (stem-child x))))
    (t (+ 1 (tree-size (fork-left x)) (tree-size (fork-right x))))))

;;; ternary encode/decode (preorder arity encoding)
(defun encode (x)
  (cond
    ((leaf-p x) "0")
    ((stem-p x) (concatenate 'string "1" (encode (stem-child x))))
    (t (concatenate 'string "2" (encode (fork-left x)) (encode (fork-right x))))))

(defun decode (s)
  (let ((i 0) (n (length s)))
    (labels ((walk ()
               (when (>= i n) (error "truncated ternary"))
               (let ((c (char s i)))
                 (incf i)
                 (case c
                   (#\0 +leaf+)
                   (#\1 (cons 1 (walk)))
                   (#\2 (cons 2 (cons (walk) (walk))))
                   (otherwise (error "bad char ~A at ~D" c (1- i)))))))
      (let ((r (walk)))
        (unless (>= i n) (error "trailing chars at ~D" i))
        r))))

;;; ------------------------------------------------------------------
;;; Budget state: fuel remaining, steps taken, size cap.
(defstruct budget fuel steps size-cap)

(define-condition fuel-out () ())
(define-condition size-out () ())

(defun check-size (b x)
  (when (> (tree-size x) (budget-size-cap b)) (signal 'size-out)))

(defun fire (b)
  (when (zerop (budget-fuel b)) (signal 'fuel-out))
  (decf (budget-fuel b))
  (incf (budget-steps b)))

;;; Verbatim port of upstream apply (same match arms, same evaluation
;;; order), instrumented with fire/check-size exactly where eval.ml
;;; has them.
(defun apply2 (b a c)
  (cond
    ;; Leaf: application, not a step
    ((leaf-p a)
     (let ((r (cons 1 c)))
       (check-size b r)
       r))
    ;; Stem: application, not a step
    ((stem-p a)
     (let ((r (cons 2 (cons (stem-child a) c))))
       (check-size b r)
       r))
    ;; triage rule fork(leaf,x) -> x
    ((leaf-p (fork-left a))
     (fire b)
     (fork-right a))
    ;; triage rule fork(stem,_): inner a1, then a2, then the outer
    ((stem-p (fork-left a))
     (fire b)
     (let ((l (apply2 b (stem-child (fork-left a)) c)))
       (let ((r (apply2 b (fork-right a) c)))
         (apply2 b l r))))
    ;; triage rule fork(fork,_)
    (t
     (fire b)
     (let ((a1 (fork-left (fork-left a)))
           (a2 (fork-right (fork-left a)))
           (a3 (fork-right a)))
       (cond
         ((leaf-p c) a1)
         ((stem-p c) (apply2 b a2 (stem-child c)))
         (t (let ((l (apply2 b a3 (fork-left c))))
              (apply2 b l (fork-right c)))))))))

;;; Eval: fold application left to right over args.
;;; NOTE: mirror of eval.ml, including where check_size is called.
(defun run (fuel size-cap program args)
  (let ((b (make-budget :fuel fuel :steps 0 :size-cap size-cap))
        (acc program))
    (handler-case
        (progn
          (check-size b program)
          (dolist (arg args)
            (check-size b arg))
          (dolist (arg args)
            (setq acc (apply2 b acc arg))
            (check-size b acc))
          (list "normal" (encode acc) (budget-steps b)))
      (fuel-out ()
        (list "fuel_exhausted" "-" (budget-steps b)))
      (size-out ()
        (list "size_exhausted" "-" (budget-steps b))))))

;;; ------------------------------------------------------------------
;;; Corpus parsing. Format (one entry per file):
;;;   name <slug>
;;;   program <ternary>
;;;   arg <ternary>            (zero or more lines)
;;;   fuel <n>
;;;   size_cap <n>
;;;   expect_status <status>
;;;   expect_result <ternary or ->
;;;   expect_steps <n>
(defun parse-corpus (path)
  (let ((name "") (program nil) (args '()) (fuel 1000) (size-cap 1000)
        (estatus "") (eresult "") (esteps -1))
    (with-open-file (in path)
      (loop
        for line = (read-line in nil nil)
        while line
        do (let* ((line (string-trim " " line)))
             (unless (or (zerop (length line)) (char= (char line 0) #\#))
               (let* ((sp (position #\space line))
                      (key (if sp (subseq line 0 sp) line))
                      (val (if sp (string-trim " " (subseq line (1+ sp))) "")))
                 (case (intern (string-upcase key) :keyword)
                   (:name (setq name val))
                   (:program (setq program (decode val)))
                   (:arg (push (decode val) args))
                   (:fuel (setq fuel (parse-integer val)))
                   (:size_cap (setq size-cap (parse-integer val)))
                   (:expect_status (setq estatus val))
                   (:expect_result (setq eresult val))
                   (:expect_steps (setq esteps (parse-integer val)))
                   (t (error "unknown corpus directive ~A" key))))))))
    (setq args (nreverse args))
    (list name program args fuel size-cap estatus eresult esteps)))

(defun main ()
  (let ((path (or (second sb-ext:*posix-argv*)
                  (error "usage: sbcl --script tree-calc.lisp <corpus-file>"))))
    (destructuring-bind (name program args fuel size-cap estatus eresult esteps)
        (parse-corpus path)
      (declare (ignore name estatus eresult esteps))
      (destructuring-bind (status result steps)
          (run fuel size-cap program args)
        (format t "~A ~A ~A~%" status result steps)))))

(main)
