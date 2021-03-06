
(provide 'project-explorer)
(eval-when-compile
  (require 'cl))

(require 'tree-mode)

(defconst *project-explorer-buffer*
  (get-buffer-create "*project-explorer*"))
(defvar *editor-window* nil)

(define-derived-mode project-tree-mode tree-mode "Maven tree"
  "A mode to display a tree view of source packages in a Maven project"
  (kill-all-local-variables)
  (setq mode-name "Project tree mode")
  (setq major-mode 'project-tree-mode))

(defun project-explorer (project-dir)
  (interactive "DProject directory: ")
  (let ((view-buf (window-buffer))
	(tree-win
	 (split-window-horizontally
	  (truncate (* (window-width) 0.75)))))
    (setq *editor-window* (selected-window))
    (set-window-buffer tree-win *project-explorer-buffer*)
    (select-window tree-win)
    (project-forest (projects project-dir))
    (select-window *editor-window*)
    (set-window-buffer *editor-window* view-buf)))

(defun project-forest (forest)
  (set (make-local-variable 'project-tree)
       (mapcar 'tree-mode-insert
	       (project-tree-widgets forest))))

(defun project-tree-widgets (forest)
;  (debug)
  (mapcar 'project-tree-widget forest))

(defun project-tree-widget (tree)
;  (print tree) ; debug
;  (debug)
  (let ((tag      (nth 0 tree))
	(path     (nth 1 tree))
	(children (nth 2 tree)))
    (cond ((file-directory-p path)
	   `(tree-widget
	     :node (push-button
		    :tag ,tag
		    :format "%[%t%]\n"
		    :notify tree-mode-reflesh-parent)
	     :expander ,(lambda (w)
			  (project-tree-widgets (widget-get w :child-elements)))
	     :open nil
	     :child-elements ,children))
	  ((file-regular-p path)
	   `(push-button
	     :tag ,tag
	     :path ,path
	     :format "%t\n"
	     :notify ,(lambda (widget &rest ignore)
			(let ((path (widget-get widget :path)))
			  (set-window-buffer *editor-window*
					     (find-file-noselect path)))))))))

(defun projects (directory)
  (mapcar (lambda (pom)
	    (let ((prj (substring pom 0 -8)))
	      (list
	       (car (last (split-string prj "/")))
	       prj
	       (sources prj))))
	  (poms directory)))

(defun sources (project)
  (mapcar
   (lambda (sr)
     (list
      (mapconcat (lambda (s) s) (last (split-string sr "/") 2) "/")
      sr
      (packages sr)))
   (apply
    'append
    (mapcar 'list-directories
	    (list-directories (concat project "/src"))))))

(defun packages (srcs)
  (mapcar (lambda (src)
	    (list
	     (replace-regexp-in-string
	      "/" "."
	      (substring src (+ (length srcs) 1)))
	     src
	     (files src)))
	  (list-directories srcs :recursive)))

(defun files (src)
  (mapcar (lambda (f)
	    (list
	     (car (last (split-string f "/")))
	     f))
	  (list-files src)))

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

(defun list-directories (dir &optional recursive)
  (let ((current-directory-list
	 (directory-files-and-attributes dir t)))
    (reduce (lambda (fles fle)
	      (if (eq t (car (cdr fle)))
		  (if (equal "." (substring (car fle) -1))
		      fles
		    (if recursive
			(append (cons (car fle)
				      (list-directories (car fle) :recursive))
				fles)
		      (cons (car fle) fles)))
		fles))
	    current-directory-list
	    :initial-value ())))
  
(defun list-files (dir)
;  (print dir) ; debug
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

