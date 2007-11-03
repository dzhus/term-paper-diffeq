(require 'semantic)
(require 'semanticdb)
(semanticdb-toggle-global-mode)

;; Return a Semantic tag table for file
(defun get-file-tags (file-name)
  (with-current-buffer 
      (find-file-noselect file-name)
    (semantic-fetch-tags)))

;; Return full tag source code (suitable for princ-ing)
(defun get-tag-body (tag)
  (let ((from (semantic-tag-start tag))
        (to (semantic-tag-end tag))
        (buffer (semantic-tag-buffer tag)))
    (with-current-buffer buffer
      (buffer-substring from to))))

;; Return a list of tags from tag-table which are also mentioned in
;; tag
(defun get-tag-deps (tag tag-table)
  (let ((from (semantic-tag-start tag))
        (to (semantic-tag-end tag))
        (buffer (semantic-tag-buffer tag))
        ;; Build associative list with tag names as keys
        (deps (mapcar 
               (lambda (tag) 
                 (cons (semantic-tag-name tag)
                       tag))
               tag-table)))
    (with-current-buffer buffer
      (let (result)
        ;; cddddr is a Lisp-oriented hack to prevent tag itself from
        ;; inclusion to dependency list
        (dolist (lexem (cddddr (semantic-lex from to 1.0e+INF)) result)
          (if (or (eq 'symbol (car lexem))
                  (eq 'NAME (car lexem)))
              (let* ((lexem-string (buffer-substring 
                                    (cadr lexem)
                                    (cddr lexem)))
                     (found-tag (assoc lexem-string
                                       deps)))
                (if found-tag
                    (add-to-list 'result (cdr found-tag) t)))))))))

;; Print body of tag with specified name from specified file
(defun print-tag-from-file (tag-name file-name)
  (interactive "sTag name: \nfFile name: ")
  (let ((tag-table (get-file-tags file-name)))
    (princ (format "%s" 
                   (get-tag-body 
                    (semantic-find-first-tag-by-name 
                     tag-name 
                     tag-table))))))

;; Get a list of all 'function tags declared in specified file
(defun get-file-functions (file-name)
  (semantic-find-tags-by-class
   'function
   (get-file-tags file-name)))

;; Get a list of all 'function tags declared in specified file and its
;; included files
(defun get-file-functions-deep (file-name)
  (with-current-buffer
      (find-file-noselect file-name)
    (semanticdb-strip-find-results
     (semanticdb-find-tags-by-class
      'function))))

;; Return a list of pairs (TAG . DEPS) where DEPS is a list of
;; functions TAG «depends» on
(defun get-file-depgraph (file-name)
  (let ((deep-tag-table (get-file-functions-deep file-name))
        (file-tag-table (get-file-functions file-name))
        (depgraph))
    (dolist (tag file-tag-table depgraph)
      (let ((deps (get-tag-deps tag deep-tag-table)))
        (add-to-list 'depgraph (cons tag deps) t)))))

;; Print depgraph for functions in specified files in DOT format
;; (suitable for processing with Graphviz programs)
(defun print-files-depgraph (&rest file-names)
  (princ "digraph D {\n")
  (princ "overlap=scale;\n")
  (dolist (file file-names)
    (let ((depgraph (get-file-depgraph file)))
      (dolist (dep-list-for-tag depgraph)
        (let ((function-name (semantic-tag-name 
                              (car dep-list-for-tag))))
          (princ (format "\"%s\";\n" function-name))
          (dolist (dependency (cdr dep-list-for-tag))
            (princ (format "\"%s\" -> \"%s\";\n"
                     (semantic-tag-name dependency)
                     function-name)))))))
  (princ "}\n"))
