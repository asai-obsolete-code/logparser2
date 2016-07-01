;; (ql:register-local-projects)
(ql:quickload '(:eazy-gnuplot :iterate :alexandria :trivia :trivia.ppcre :mito :lisp-namespace :lparallel
                :cl-syntax-interpol) :silent t)

(defpackage :ros.script.plot
  (:use :cl :eazy-gnuplot :iterate :alexandria :trivia :trivia.ppcre :mito :lparallel :sxql :dbi
        :cl-interpol))

(in-package :ros.script.plot)

(setf *auto-migration-mode* t)

(defclass db-symbol ()
  ((name :col-type :text :initarg :name))
  (:metaclass dao-table-class))
(defclass tag (db-symbol) ()
  (:metaclass dao-table-class))
(defclass problem (db-symbol) ()
  (:metaclass dao-table-class))
(defclass domain (db-symbol) ()
  (:metaclass dao-table-class))
(defclass ipcyear (db-symbol) ()
  (:metaclass dao-table-class))
(defclass ipctrack (db-symbol) ()
  (:metaclass dao-table-class))
(defclass algorithm (db-symbol) ()
  (:metaclass dao-table-class))
(defclass heuristics (db-symbol) ()
  (:metaclass dao-table-class))
(defclass default (db-symbol) ()
  (:metaclass dao-table-class))
(defclass queue (db-symbol) ()
  (:metaclass dao-table-class))

(defclass experiment ()
  ((tag :col-type (or tag :null) :initarg :tag) ;; arbitrary tag string
   (problem :col-type (or problem :null) :initarg :problem)
   (domain :col-type (or domain :null) :initarg :domain)
   (ipcyear :col-type (or ipcyear :null) :initarg :ipcyear)
   (ipctrack :col-type (or ipctrack :null) :initarg :ipctrack)
   (algorithm :col-type (or algorithm :null) :initarg :algorithm)
   (heuristics :col-type (or heuristics :null) :initarg :heuristics)
   (default :col-type (or default :null) :initarg :default)
   (queue :col-type (or queue :null) :initarg :queue)
   ;;
   (search :col-type :float :initarg :search :initform -1)
   ;; (wall :col-type :float :initarg :wall :initform -1)
   (memory :col-type :integer :initarg :memory :initform -1)
   ;;
   (expansion :col-type :integer :initarg :expansion :initform -1)
   (evaluation :col-type :integer :initarg :evaluation :initform -1)
   (generation :col-type :integer :initarg :generation :initform -1)
   (plan-length :col-type :integer :initarg :plan-length :initform -1)
   (plan-cost   :col-type :integer :initarg :plan-cost :initform -1))
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
