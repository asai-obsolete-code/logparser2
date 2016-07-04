(in-package :ros.script.plot)
(defun correct (num)
  (if (plusp num)
      num
      10e8))

(defun setup (title path &optional (improved t) (spacing 8))
  (ensure-directories-exist path :verbose t)
  (gp-setup :terminal `(:svg :enhanced
                        :size #+pdf(5.5 3.6) (800 800)
                        ;; :dashed
                        :background :rgb ,(if improved "white" "gray90")
                        ;; :monochrome
                        :font "Times New Roman, 12")
            :size :square
            :view '(:equal :xy)
            :key `(:bottom :right :spacing ,spacing)
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

(setf (fdefinition '%id) #'db-symbol-id)
(setf (fdefinition '%n) #'db-symbol-name)

(defun replace-newline (string)
  (format nil "~{~a~^\\n~}" (ppcre:split "
" (string-trim '(#\Newline) string))))

(defvar *stream*)

(defun main (&rest args)
  (declare (ignorable args))
  (my-connect "db.sqlite")
  (setf *kernel* (make-kernel (read-from-string (uiop:run-program "grep processor /proc/cpuinfo | wc -l" :output :string))))
  (mito.logger:with-sql-logging
    (mapcar #'force (futures))))

(deftype data () 'list) 
(defun data-p (data) (listp data))
(defun data-improved (data)
  (count-if (lambda-match
              ((list _ (and x (plus)) _ (and (plus) (< x))) t)) data))
(defun data-both (data)
  (count-if (lambda-match
              ((list _ (and x (plus)) _ (plus)) t)) data))
(defun data-nomacro (data)
  (count-if (lambda-match
              ((list _ (plus) _ (minus)) t)) data))
(defun data-macro (data)
  (count-if (lambda-match
              ((list _ (minus) _ (plus)) t)) data))
(defun data-sum-x (data)
  (iter (for datum in data)
        (match datum
          ((list _ (and x (plus)) _ (plus)) (summing x)))))
(defun data-sum-y (data)
  (iter (for datum in data)
        (match datum
          ((list _ (plus) _ (and y (plus))) (summing y)))))

(defmacro with-forced-lex (bindings &body body)
  `(let ,(mapcar (lambda (x) (list x x)) bindings)
     ,@body))

(define-symbol-macro base (%id "base" :tag))
