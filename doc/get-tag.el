(require 'semantic)

(defun get-file-tags (file-name)
  (find-file file-name)
  (semantic-fetch-tags))

(defun print-tag-body (tag)
  (let ((from (semantic-tag-start tag))
        (to (semantic-tag-end tag))
        (buffer (semantic-tag-buffer tag)))
    (switch-to-buffer buffer)
    (message "%%%%")
    (message (buffer-substring from to))))

(defun print-tag-from-file (tag-name file-name)
  (interactive "sTag name: \nfFile name: ")
  (let ((tag-table (get-file-tags file-name)))
    (print-tag-body (semantic-find-first-tag-by-name
                     tag-name
                     tag-table))))