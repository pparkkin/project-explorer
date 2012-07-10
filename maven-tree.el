
(provide 'maven-tree)
(eval-when-compile
  (require 'cl))

(require 'tree-mode)

(define-derived-mode maven-tree-mode tree-mode "Maven tree"
  "A mode to display a tree view of source packages in a Maven project"
  (kill-all-local-variables)
  (setq mode-name "Maven tree mode")
  (setq major-mode 'maven-tree-mode)

  (maven-forest
   (list '("Project" (("main/scala") ("test/scala"))))))

(defun maven-forest (forest)
  (set (make-local-variable 'maven-tree)
       (mapcar 'tree-mode-insert
	       (maven-tree-widgets forest))))

(defun maven-tree-widgets (forest)
;  (debug)
  (mapcar 'maven-tree-widget forest))

(defun maven-tree-widget (tree)
;  (print tree) ; debug
;  (debug)
  `(tree-widget
    :node (push-button
	   :tag ,(car tree)
	   :format "%[%t%]\n"
	   :notify tree-mode-reflesh-parent)
    :expander ,(lambda (w)
		 (maven-tree-widgets (widget-get w :child-elements)))
    :open nil
    :child-elements ,(car (cdr tree))))

;; (defun maven-project-widgets (projects)
;;   (mapcar (lambda (p)
;; 	    (maven-tree-widget p 'maven-source-widgets))
;; 	  projects))

;; (defun maven-source-widgets (widg)
;;   (mapcar (lambda (s)
;; 	    (maven-tree-widget s 'maven-package-widgets))
;; 	  (maven-sources (:tag (:node widg)))))

;; (defun maven-package-widgets (widg)
;;   nil)

;; (defun maven-projects ()
;;   (list "PRoject 1" "PRoject 2"))

;; (defun maven-sources (project)
;;   (list "main/java" "test/java"))