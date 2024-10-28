(defun adv/pretty-print-json-buffer (file-path)
  "Pretty print the entire JSON buffer of the file at FILE-PATH."
  ;; Expand the file path to handle "~/" and other relative paths
  (setq file-path (expand-file-name file-path))
  ;; Check if the file exists
  (when (file-exists-p file-path)
    ;; Open the file
    (with-current-buffer (find-file-noselect file-path)
      ;; Temporarily disable read-only mode if it's enabled
      (when buffer-read-only
        (read-only-mode -1))
      ;; Apply json-pretty-print-buffer to the entire buffer
      (json-pretty-print-buffer)
      ;; Save the buffer
      (save-buffer)
      ;; Close the buffer
      (kill-buffer))))
