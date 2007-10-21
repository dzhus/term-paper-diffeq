(require 'semantic)
(require 'semanticdb)
(semanticdb-toggle-global-mode)

;; Return a Semantic tag table for file
(defun get-file-tags (file-name)
  (with-current-buffer 
      (find-file-noselect file-name)
    (semantic-fetch-tags)))

;; Return full tag source code (suitable for 'message-ing)
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
        (deps (mapcar 
               (lambda (tag) 
                 (cons (semantic-tag-name tag)
                       tag))
               tag-table)))
    (switch-to-buffer buffer)
    (let (result)
      ;; cddddr is a Lisp-oriented hack to prevent tag itself from
      ;; inclusion to dependency list
      (dolist (lexem (cddddr (semantic-lex from to 50)) result)
        (if (eq 'symbol (car lexem))
            (let* ((lexem-string (buffer-substring 
                                  (cadr lexem)
                                  (cddr lexem)))
                   (found-tag (assoc lexem-string
                                     deps)))
              (if found-tag
                  (add-to-list 'result (cdr found-tag) t))))))))

;; Print body of tag with specified name from specified file
(defun print-tag-from-file (tag-name file-name)
  (interactive "sTag name: \nfFile name: ")
  (let ((tag-table (get-file-tags file-name)))
    (message "%%%%%s BODY" tag-name)
    (message "%s" 
             (get-tag-body 
              (semantic-find-first-tag-by-name tag-name tag-table)))))

;; Print a list of all functions declared in specified file
(defun print-file-functions (file-name)
  (let ((tag-table (semantic-find-tags-by-class
                    'function
                    (get-file-tags file-name))))
    (message "%%%% TAGS")
    (dolist (tag tag-table)
      (message "%s" (semantic-tag-name tag)))))

(defun print-depgraph (file-name)
  (interactive "fFile name: ")
  (with-current-buffer
      (find-file-noselect file-name)
    (let ((deep-tag-table (semanticdb-strip-find-results
                           (semanticdb-find-tags-by-class
                            'function)))
          (file-tag-table (semantic-find-tags-by-class
                           'function
                           (semantic-fetch-tags))))
      (message "%%%% DEPS")
      (dolist (tag file-tag-table)
        (let ((deps (get-tag-deps tag deep-tag-table)))
          (dolist (dependency deps)
            (message (semantic-tag-components-with-overlays-default tag))
            (message "%s %s"
                     (semantic-tag-name dependency)
                     (semantic-tag-name tag))))))))