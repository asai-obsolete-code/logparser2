(in-package :ros.script.plot)
(defun correct (num)
  (if (plusp num)
      num
      10e8))

(defun setup (title path &optional improved)
  (ensure-directories-exist path :verbose t)
  (gp-setup :terminal `(:pngcairo :enhanced
                        :size #+pdf(5.5 3.6) (1200 800)
                        ;; :dashed
                        :background :rgb ,(if improved "white" "gray90")
                        ;; :monochrome
                        :font "Times New Roman, 12")
            :size :square
            :view '(:equal :xy)
            :key '(:spacing 8)
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
