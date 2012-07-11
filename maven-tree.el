
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
   (mapcar (lambda (prj)
	     (append prj (list (packages prj))))
	   (projects "~/devel/MrMutator"))))

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
    :child-elements ,(car (cdr (cdr tree)))))

(defun packages (project)
  (mapcar
   (lambda (sr)
     (list
      (mapconcat (lambda (s) s) (last (split-string sr "/") 2) "/")
      sr))
   (apply
    'append
    (mapcar 'list-directories
	    (list-directories (concat (car (cdr project)) "/src"))))))

(defun projects (directory)
  (mapcar (lambda (pom)
	    (list
	     (car (last (split-string pom "/") 2))
	     (substring pom 0 -8)))
	  (poms directory)))

(defun poms (directory)
  "List the pom files in DIRECTORY and in its sub-directories."
  (interactive "DDirectory name: ")
  (mapcar
   'car
   (recursively-find-files (lambda (fle)
			    (equal "pom.xml" (substring (car fle) -7)))
			  directory)))

(defun recursively-find-files (pred directory)
  "List the files in DIRECTORY and in its sub-directories matching pred."
  (let ((current-directory-list
	 (directory-files-and-attributes directory t)))
    ;; while we are in the current directory
    (reduce (lambda (fles fle)
	      (cond
	       ((eq t (car (cdr fle)))
		;; decide whether to skip or recurse
		(if (equal "." (substring (car fle) -1))
		    ;; then do nothing since filename is that of
		    ;;   current directory or parent, "." or ".."
		    fles
		  ;; else descend into the directory and repeat the process
		  (append
		   (recursively-find-files pred (car fle)) fles)))
	       ((funcall pred fle)
		(cons fle fles))
	       ;; check whether filename is that of a directory
	       (t fles)))
	    current-directory-list
	    :initial-value ())))

(defun list-directories (dir)
  (let ((current-directory-list
	 (directory-files-and-attributes dir t)))
    (reduce (lambda (fles fle)
	      (if (eq t (car (cdr fle)))
		  (if (equal "." (substring (car fle) -1))
		      fles
		    (cons (car fle) fles))
		fles))
	    current-directory-list
	    :initial-value ())))
  
(defun list-files (dir)
  (let ((current-directory-list
	 (directory-files-and-attributes dir t)))
    (reduce (lambda (fles fle)
	      (if (eq nil (car (cdr fle)))
		  (if (equal "." (substring (car fle) -1))
		      fles
		    (cons (car fle) fles))
		fles))
	    current-directory-list
	    :initial-value ())))

