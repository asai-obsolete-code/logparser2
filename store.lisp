(in-package :ros.script.plot)
(declaim (optimize (debug 3) (speed 0)))
(lispn:define-namespace parser)

;;;; parsers

(defpattern last (n &rest subpatterns)
  (check-type n integer)
  (assert (= n (length subpatterns)))
  (with-gensyms (it)
    `(guard1 (,it :type list) (listp ,it)
             (last ,it ,n)
             (list ,@subpatterns))))

(defpattern read (n)
  `(access #'read-from-string ,n))

(defpattern split (regex &rest subpatterns)
  (with-gensyms (it)
    `(guard1 (,it :type string) (stringp ,it)
             (ppcre:split ,regex ,it)
             (list ,@subpatterns))))
(defpattern split* (regex &rest subpatterns)
  (with-gensyms (it)
    `(guard1 (,it :type string) (stringp ,it)
             (ppcre:split ,regex ,it)
             (list* ,@subpatterns))))

(defmacro defparser (name args &body body)
  `(setf (symbol-parser ',name) (sb-int:named-lambda ,name ,args ,@body)))

(defvar *local*)                        ; a hashtable
(defun local (name &optional default)
  (ensure-gethash name *local* default))
(defun (setf local) (newval name &optional default)
  (ensure-gethash name *local* default)
  (setf (gethash name *local*) newval))

(defparser expansion (line)
  (match line
    ((ppcre "Expanded ([0-9]*) state" (read num))
     num)
    ((ppcre "(?:PROBE|BFS)_Expanded=([0-9]*)" (read num))
     (incf (local :expansion 0) num) ;; take the sum
     nil)
    ;; mp: num decisions
    ((ppcre "SAT \\(([0-9]*) decisions (?:[0-9]*) conflicts\\)" (read decisions))
     decisions)))

(defparser evaluation (line)
  (match line
    ((ppcre "Evaluated ([0-9]*) state" (read num))
     num)
    ((ppcre "(?:PROBE|BFS)_Evaluated=([0-9]*)" (read num))
     (incf (local :evaluation 0) num)
     nil)
    ((ppcre "Horizon (?:[0-9]*): ([0-9]*) variables" (read num))
     (maxf (local :evaluation 0) num)
     nil)))

(defparser search (line)
  (match line
    ((ppcre "Actual search time: ([.0-9]*) " (read num))
     num)
    ((ppcre "Time: ([.0-9]+)" (read num))
     num)
    ((ppcre "Time: <1 msec")
     0.001)
    ((ppcre "total time ([.0-9]*)" (read num))
     num)))

(defparser generation (line)
  (match line
    ((ppcre "Generated ([0-9]*) state" (read num))
     num)))

(defparser plan-length (line)
  (match line
    ((ppcre "Plan length: ([0-9]*) step" (read n))
     n)
    ((ppcre "Plan cost: (?:[.0-9]*), steps: ([0-9]*)" (read num))
     num)
    ((ppcre "([0-9]*) actions in the plan." (read num))
     num)))

(defparser plan-cost (line)
  (match line
    ((ppcre "Plan cost: *([0-9]*)" (read n))
     n)
    ;; limit.sh returns the plan length instead of cost, but we ignore it here
    ;; ((ppcre "Plan cost: ([.0-9]*)" (read num))
    ;;  (floor num))
    ))

#+nil
(defparser seed (line)
  (match line
    ((ppcre "\"--seed\" \"([0-9]*)\"" (read seed))
     seed)))

(defparser macros (line)
  (match line
    ((ppcre "with ([0-9]*) macros" (read n))
     n)))

(defparser macros2 (line)
  "macros with length > 2"
  (match line
    ((ppcre "... ([0-9]*) remaining." (read n))
     ;; (t=0)    Filtering null macros.
     ;; (t=0)    ... 2815 remaining.
     ;; (t=0)    Filtering macros with length 1.
     ;; (t=0)    ... 2815 remaining.
     ;; the last one is the intended value
     (setf (local :macros2 0) n)
     nil)))

(defparser usedmacros (line)
  "Plan 0 Decoding action JUNK-BOARD-MOVE-DOWN-FAST44223"
  (match line
    ((ppcre "Decoding action (.*)" (read name))
     ;; (push name (local :usedmacros-names nil))
     (incf (local :usedmacros 0))
     nil)))

(defparser memory (line)
  "in kB"
  (match line
    ((ppcre "MAXMEM_RSS (-?[0-9]*)" (read mem))
     (maxf (local :memory -1) mem)
     nil)
    ((ppcre "total size ([.0-9]*) MB" (read mem))
     (maxf (local :memory -1) (floor (* 1000 mem)))
     nil)))

;;;; main

;; better to do in SQL level, but sqlite is not good for this purpose
(defvar *lock* (bt:make-lock "db-lock"))
(defun ensure-dao (name &rest args)
  (bt:with-lock-held (*lock*)
    (let ((retry 0))
      (unwind-protect
          (or (block nil
                (tagbody
                  :start
                  (return
                    (handler-case
                        (values (apply #'create-dao name args) nil)
                      (dbi.error:<dbi-database-error> (c)
                        (case (slot-value c 'dbi.error::error-code)
                          (:busy (incf retry) (go :start))
                          (otherwise (go :start2))))))
                  :start2
                  (return
                    (handler-case
                        (values (apply #'find-dao name args) t)
                      (dbi.error:<dbi-database-error> (c)
                        (case (slot-value c 'dbi.error::error-code)
                          (:busy (incf retry) (go :start2))
                          (otherwise (error "unknown error!"))))))))
              (error "should not return nil!"))
        (when (> retry 0)
          (format t "~&~a retries" retry))))))

(defun ensure-dao/write (name &rest args)
  "runs the duplicate checking, but do not write the results"
  (or (values (apply #'find-dao name args) t)
      (values (apply #'make-instance name args) nil)
      (error "insert-dao returns nil!")))

(defun parse (file)
  (apply #'reinitialize-instance
         (apply #'ensure-dao/write (parse-pathname file))
         (parse-output file)))

(defun parse-output (file)
  (let ((*local* (make-hash-table)))
    (append (iter outer
                  (for line in-file file using #'read-line)
                  (iter (for (key fn) in-hashtable *parser-table*)
                        (in outer
                            (when-let ((it (funcall fn line)))
                              (collect (make-keyword key))
                              (collect it)))))
            (iter outer
                  (for line in-file (make-pathname :type "err" :defaults file) using #'read-line)
                  (iter (for (key fn) in-hashtable *parser-table*)
                        (in outer
                            (when-let ((it (funcall fn line)))
                              (collect (make-keyword key))
                              (collect it)))))
            (hash-table-plist *local*))))

(defmacro initargs (&rest args)
  `(list ,@(mappend (lambda (x) (list (make-keyword x)
                                      (if (find-class x nil)
                                          `(ensure-dao ',x :name ,x)
                                          x)))
                    args)))

(defpattern ipcyear (pattern)
  `(ppcre "ipc([0-9]*)" (read ,pattern)))
(defpattern problem (pattern)
  `(ppcre "p([0-9]*)" (read ,pattern)))

(defun parse-pathname (file)
  (ematch file
    ;; FD-based 
    ((pathname :directory (last 3
                                (split "-" "fig3" (read length) tag)
                                (split* "-" (ipcyear ipcyear) _)
                                domain)
               :name      (split* "\\." (problem problem)
                                  (ppcre "(..1)([el])([0-9]*)" heuristics algorithm (read seed)) _))
     (list* 'fig3 (initargs tag domain problem ipcyear heuristics algorithm length seed)))
    ;; mp, probe
    ((pathname :directory (last 3
                                (split "-" "fig3" (read length) tag)
                                (split* "-" (ipcyear ipcyear) _)
                                domain)
               :name      (split* "\\." (problem problem)
                                  (ppcre "(mp|probe)([0-9]*)" algorithm (read seed)) _))
     (list* 'fig3 (initargs tag domain problem ipcyear algorithm length seed)))
    ((pathname :directory (last 3
                                (split "-" "fig2" tag)
                                (split* "-" (ipcyear ipcyear) _)
                                domain)
               :name      (split* "\\." (problem problem)
                                  (ppcre "(..1)([el])" heuristics algorithm) _))
     (list* 'fig2 (initargs tag domain problem ipcyear heuristics algorithm)))
    ((pathname :directory (last 3
                                (split "-" "fig2" tag)
                                (split* "-" (ipcyear ipcyear) _)
                                domain)
               :name      (split* "\\." (problem problem)
                                  (ppcre "(mp|probe)" algorithm) _))
     (list* 'fig2 (initargs tag domain problem ipcyear algorithm)))))

(defun set-pragma ()
  (mito.logger:with-sql-logging
    (execute-sql
     (sxql:pragma "synchronous" 0))
    (execute-sql
     (sxql:pragma "journal_mode" "memory"))))

(defun call-with-error-decoration (decoration fn)
  (handler-bind ((error (lambda (c)
                          (pprint-logical-block (*error-output* nil :prefix decoration)
                            (pprint-indent :block 2)
                            (signal c)))))
    (funcall fn)))

(defun main (&rest files)
  (my-connect "db.sqlite")
  (set-pragma)
  (mapcar #'ensure-table-exists '(tag domain algorithm heuristics fig2 fig3))
  (setf *kernel* (make-kernel 8))
  (let ((results (time (pmapcar (lambda (file)
                                  (call-with-error-decoration
                                   (format nil "~&while parsing metadata for ~a:" file)
                                   (lambda () (parse (pathname file)))))
                                files))))
    (format t "~%for creation.")
    (let ((t1 (get-internal-real-time)))
      (time
       (with-transaction *connection*
         (map nil #'save-dao results)))
      (let ((duration (float (/ (- (get-internal-real-time) t1) internal-time-units-per-second))))
        (format t "~%~a seconds for ~a inserts: ~a inserts/sec.~%"
                duration (length results) (/ (length results) duration))))))
