;; (ql:register-local-projects)
(ql:quickload '(:eazy-gnuplot :iterate :alexandria :trivia :trivia.ppcre :mito :lisp-namespace :lparallel
                :cl-syntax-interpol) :silent t)

(defpackage :ros.script.plot
  (:use :cl :eazy-gnuplot :iterate :alexandria :trivia :trivia.ppcre :mito :lparallel :sxql :dbi
        :cl-interpol))

(in-package :ros.script.plot)

(setf *auto-migration-mode* t)

(defclass experiment ()
  ((tag :col-type :text :initarg :tag) ;; arbitrary tag string
   (problem :col-type :text :initarg :problem)
   (domain :col-type :text :initarg :domain)
   (ipcyear :col-type :text :initarg :ipcyear)
   (ipctrack :col-type :text :initarg :ipctrack)
   (algorithm :col-type :text :initarg :algorithm)
   (heuristics :col-type :text :initarg :heuristics)
   (default :col-type :text :initarg :default)
   (queue :col-type :text :initarg :queue)
   ;;
   (search :col-type :float :initarg :search :initform -1)
   ;; (wall :col-type :float :initarg :wall :initform -1)
   (memory :col-type :integer :initarg :memory :initform -1)
   ;;
   (expansion :col-type :integer :initarg :expansion :initform -1)
   (evaluation :col-type :integer :initarg :evaluation :initform -1)
   (generation :col-type :integer :initarg :generation :initform -1)
   (plan-length :col-type :integer :initarg :plan-length :initform -1)
   (plan-cost   :col-type :integer :initarg :plan-cost :initform -1)
   )
  (:metaclass dao-table-class))

(defclass macro (experiment)
     ((macros :col-type :integer :initarg :macros :initform 0)
      (macros2 :col-type :integer :initarg :macros2 :initform 0)
      (usedmacros :col-type :integer :initarg :usedmacros :initform 0))
  (:metaclass dao-table-class))

(defclass fig2 (macro)
     ()
  (:metaclass dao-table-class))

(defclass fig3 (macro)
     ((length :col-type :integer :initarg :length)
      (seed :col-type :integer :initarg :seed))
  (:metaclass dao-table-class))

(defun my-connect (name)
  (declare (ignorable name))
  (connect-toplevel :sqlite3 :database-name name))
