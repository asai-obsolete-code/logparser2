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
  (ensure-directories-exist "exp/")
  (iter (for (_ config) in
             (retrieve-by-sql
              (select :configuration
                (from :fig2)
                (group-by :configuration))))
        (with-plots (*standard-output*)
          (gp-setup :terminal '(:pdf :enhanced :size (5.5 3.6)
                                :dashed
                                ;; :background :rgb "gray80"
                                ;; :monochrome
                                :font "Times New Roman, 12")
                    :size :square
                    :view '(:equal :xy)
                    :output #?"exp/fig2-exp-${config}.pdf"
                    :pointsize 0.45
                    :logscale :xy
                    :format '(xy "10^%T")
                    :title #?"${config} Expansion"
                    :xlabel "Without Macro"
                    :ylabel "With Macro")
          (plot "x" :title "y=x")
          (when-let ((data (retrieve-by-sql
                            (select (:base.expansion :macro.expansion)
                              (from (:as :fig2 :base) (:as :fig2 :macro))
                              (where
                               (:and (:= :base.problem :macro.problem)
                                     (:= :base.domain :macro.domain)
                                     (:= :base.configuration config)
                                     (:= :macro.configuration config)
                                     (:= :base.tag "base")
                                     (:= :macro.tag "macro")))))))
            (plot (lambda ()
                    (iter (for (_2 x _3 y) in data)
                          (format t "~&~a ~a" (correct x) (correct y)))))))))


