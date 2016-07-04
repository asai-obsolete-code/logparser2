(in-package :ros.script.plot)
(defun correct (num &optional (max 10e8))
  (if (plusp num)
      num
      max))

(defun setup (title path &key (spacing 1))
  (ensure-directories-exist path :verbose t)
  (print path *error-output*)
  (let ((terminal :pdf))
    (gp-setup :terminal `(,terminal :enhanced
                                    :size
                                    (3.3 3.3)
                                    ;; (800 800)
                                    ;; :dashed
                                    ;; :background :rgb ,(if improved "white" "gray90")
                                    ;; :monochrome
                                    :font "Times New Roman, 11")
              :size :square
              :view '(:equal :xy)
              :key `(:bottom :right :spacing ,spacing :samplen 1)
              :output (make-pathname :defaults path :type (string-downcase terminal))
              :pointsize 0.3
              :logscale :xy
              :format '(xy "10^%T")
              :mxtics :default
              :mytics :default
              :title title
              :xlabel "Without Macro"
              :ylabel "With Macro")))

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

(setf (fdefinition '%id) #'db-symbol-id)
(setf (fdefinition '%n) #'db-symbol-name)

(defun replace-newline (string)
  (format nil "~{~a~^\\n~}" (ppcre:split "
" (string-trim '(#\Newline) string))))

(defvar *stream*)

(defun main (&rest args)
  (declare (ignorable args))
  (my-connect "db.sqlite")
  (futures))

(deftype data () 'list) 
(defun data-p (data) (listp data))
(defun data-improved (data)
  (count-if (lambda-match
              ((list* _ (and x (plus)) _ (and (plus) (< x)) _) t)) data))
(defun data-both (data)
  (count-if (lambda-match
              ((list* _ (and x (plus)) _ (plus) _) t)) data))
(defun data-nomacro (data)
  (count-if (lambda-match
              ((list* _ (plus) _ (minus) _) t)) data))
(defun data-macro (data)
  (count-if (lambda-match
              ((list* _ (minus) _ (plus) _) t)) data))
(defun data-sum-x (data)
  (iter (for datum in data)
        (match datum
          ((list* _ (and x (plus)) _ (plus) _) (summing x)))))
(defun data-sum-y (data)
  (iter (for datum in data)
        (match datum
          ((list* _ (plus) _ (and y (plus)) _) (summing y)))))

(defmacro with-forced-lex (bindings &body body)
  `(let ,(mapcar (lambda (x) (list x x)) bindings)
     ,@body))

(define-symbol-macro base (%id "base" :tag))
