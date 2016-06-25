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

(defun main (&rest args)
  (declare (ignorable args))
  (my-connect "db.sqlite")
  (ensure-directories-exist "eval3/")
  (iter (for (_ config) in
             (retrieve-by-sql
              (select :configuration
                (from :fig2)
                (group-by :configuration))))
        (iter (for length in '(2 5 8))
              (with-plots (*standard-output*)
                (gp-setup :terminal '(:pdf :enhanced :size (5.5 3.6)
                                      :dashed
                                      ;; :background :rgb "gray80"
                                      ;; :monochrome
                                      :font "Times New Roman, 12")
                          :size :square
                          :view '(:equal :xy)
                          :output #?"eval3/${length}-${config}.pdf"
                          :pointsize 0.45
                          :logscale :xy
                          :format '(xy "10^%T")
                          :title #?"${config} Evaluation"
                          :xlabel "Without Macro"
                          :ylabel "With Macro")
                (plot "x" :title "y=x")
                (when-let ((data (retrieve-by-sql
                                  (select (:base.evaluation (:avg :mangle.evaluation))
                                    (from (:as :fig2 :base) (:as :fig3 :mangle))
                                    (where
                                     (:and (:= :base.problem :mangle.problem)
                                           (:= :base.domain :mangle.domain)
                                           (:= :base.configuration config)
                                           (:= :mangle.configuration config)
                                           (:= :base.tag "base")
                                           (:= :mangle.tag "mangle")
                                           (:= :mangle.length length)))
                                    (group-by :mangle.problem :mangle.domain :mangle.configuration)))))
                  (plot (lambda ()
                          (iter (for (_2 x _3 y) in data)
                                (format t "~&~a ~a" (correct x) (correct y))))))))))


