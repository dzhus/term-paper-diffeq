(require 'semantic)

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
      (message (buffer-substring from to)))))

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
;    (message "%d" from)
    (let (result)
      (dolist (lexem (semantic-lex from to 50) result)
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
    (message "%%%%")
    (message (get-tag-body 
              (semantic-find-first-tag-by-name
               tag-name
               tag-table)))))

;; Return a list of tags which specified tag depends on (== mentions
;; symbolic names of them)
(defun print-tag-deep-deps-from-file (tag-name file-name)
  (interactive "sTag name: \nfFile name: ")
  (with-current-buffer 
      (find-file-noselect file-name)
    (let ((tag-table (semanticdb-strip-find-results
                      (semanticdb-find-tags-by-class
                       'function))))
      (let ((deps (get-tag-deps 
                   (semantic-find-first-tag-by-name
                    tag-name tag-table)
                   tag-table)))
        (dolist (tag deps)
          (message (semantic-tag-name tag)))))))

(defun print-depgraph (file-name)
  (interactive "fFile name: ")
  (with-current-buffer
      (find-file-noselect file-name)
    (let ((deep-tag-table (semanticdb-strip-find-results
                           (semanticdb-find-tags-by-class
                            'function)))
          (file-tag-table (semantic-fetch-tags)))
      (dolist (tag file-tag-table)
        (let ((deps (get-tag-deps tag deep-tag-table)))
          (dolist (dependency deps)
            (message "%s depends on %s"
                     (semantic-tag-name tag)
                     (semantic-tag-name dependency))))
        (message "\n")))))
              
        