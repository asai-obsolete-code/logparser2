#!/bin/sh
#|-*- mode:lisp -*-|#
#| <Put a one-line description here>
exec ros -Q -- $0 "$@"
|#

(load "common.lisp")
(in-package :ros.script.plot)
(cl-syntax:use-syntax :cl-interpol)

(defun correct (num)
  (if (plusp num)
      num
      10e8))

(defun setup (title path)
  (ensure-directories-exist path :verbose t)
  (gp-setup :terminal '(:pdf :enhanced :size (5.5 3.6)
                        :dashed
                        ;; :background :rgb "gray80"
                        ;; :monochrome
                        :font "Times New Roman, 12")
            :size :square
            :view '(:equal :xy)
            :output path
            :pointsize 0.45
            :logscale :xy
            :format '(xy "10^%T")
            :title title
            :xlabel "Without Macro"
            :ylabel "With Macro"))

(defun db-symbol-id (name from)
  (second
   (first
    (retrieve-by-sql
     (select :id
       (from from)
       (where (:= :name name)))))))

(defun db-symbol-name (id from)
  (second
   (first
    (retrieve-by-sql
     (select :name
       (from from)
       (where (:= :id id)))))))

(defun main (&rest args)
  (declare (ignorable args))
  (my-connect "db.sqlite")
  (mito.logger:with-sql-logging
    (iter (with base = (db-symbol-id "base" :tag))
          (with macro = (db-symbol-id "macro" :tag))
          (for measure in '(:evaluation :expansion :generation))
          (iter (for (_ alg) in
                     (retrieve-by-sql
                      (select :*
                        (from :algorithm))))
                (iter (for (_ h) in
                           (or (retrieve-by-sql
                                (select :*
                                  (from :heuristics)))
                               '((:heuristics nil))))
                      (when-let ((data (retrieve-by-sql
                                        (select (list (make-keyword (symbolicate :base. measure))
                                                      (make-keyword (symbolicate :macro. measure)))
                                          (from (:as :fig2 :base) (:as :fig2 :macro))
                                          (where
                                           (:and (:= :base.problem :macro.problem)
                                                 (:= :base.domain_id :macro.domain_id)
                                                 (:= :base.heuristics_id h)
                                                 (:= :macro.heuristics_id h)
                                                 (:= :base.algorithm_id alg)
                                                 (:= :macro.algorithm_id alg)
                                                 (:= :base.tag_id base)
                                                 (:= :macro.tag_id macro)))))))
                        (print (subseq data 0 10))
                        (funcall
                         ;; bt:make-thread
                         (lambda ()
                           (with-plots (s)
                             (setup (format nil "~a ~a ~a"
                                            (db-symbol-name alg :algorithm)
                                            (db-symbol-name h :heuristics)
                                            measure)
                                    (format nil "~a/~a/~a.pdf"
                                            measure
                                            (db-symbol-name alg :algorithm)
                                            (db-symbol-name h :heuristics)))
                             (plot "x" :title "y=x")
                             (plot (lambda ()
                                     (iter (for (_2 x _3 y) in data)
                                           (format s "~&~a ~a" (correct x) (correct y)))))))))))))
  ;; (mapcar #'bt:join-thread (bt:all-threads))
  )


