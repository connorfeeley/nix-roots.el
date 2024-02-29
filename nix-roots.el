;;; nix-roots.el --- Emacs package to list stray nix roots -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Connor Feeley
;;
;; Author: Connor Feeley <git@cfeeley.org>
;; Maintainer: Connor Feeley <git@cfeeley.org>
;; Created: February 28, 2024
;; Modified: February 28, 2024
;; Version: 0.0.1
;; Keywords: convenience local nix tools unix
;; Homepage: https://github.com/connorfeeley/nix-roots
;; Package-Requires: ((emacs "25.1") (dash "2.19.1") (s) (f))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides a command to list stray nix roots.
;;
;;; Code:

(require 'cl-lib)

;; Custom groups
(defgroup nix-roots nil
  "Emacs package to list stray nix roots."
  :prefix "nix-roots"
  :group 'nix)

(defcustom nix-roots-command '("nix-store" "--gc" "--print-roots")
  "Command to list stray nix roots."
  :type 'list
  :group 'nix-roots)

(defvar nix-roots-query-queue '() "Queue holding roots yet to be processed.")
(defvar nix-roots-query-parallel-limit 8 "Maximum number of parallel nix queries.")
(defvar nix-roots-query-running-count 0 "Number of currently running nix queries.")

;;;###autoload
(defun nix-roots-list ()
  "Convert a `dts' FILE back to a `dtb' buffer."
  (interactive)
  (nix-roots-to-buffer (nix-roots-filter-matrix (nix-roots-to-matrix (nix-roots-query)))))

(defun nix-roots-query ()
  "Query nix store for stray roots."
  (shell-command-to-string "nix-store --gc --print-roots"))

(defun nix-roots-to-matrix (output)
  "Convert command output to list of lists."
  (let* ((lines (split-string output "\n" t))  ;; split the output into lines
         (roots
          (mapcar
           (lambda (line)
             (split-string line " -> " t))  ;; split each line into two parts at " -> "
           lines)))
    roots))

(defun nix-roots-filter-matrix (matrix)
  "Filter out undesired source locations in the matrix."
  (let ((filtered-matrix ()))
    (dolist (row matrix)
      (unless (or (string-match "{lsof}" (nth 0 row))
                  (string-match "/run" (nth 0 row)))
        (cl-pushnew row filtered-matrix)))
    filtered-matrix))

;; Logging function for asynchronous query results
(defun nix-roots-log-query-result (root size)
  "Log the root and its corresponding size to the *Nix Query Log* buffer."
  (with-current-buffer (get-buffer-create "*Nix Query Log*")
    ;; Make sure we're at the end of the buffer to insert new log entry
    (goto-char (point-max))
    ;; Insert formatted log entry
    (insert (format "[%s] Root: %s, Size: %s\n"
                    (format-time-string "%Y-%m-%d %H:%M:%S")
                    root size))
    ;; If the buffer is displayed, refresh its window
    (let ((win (get-buffer-window (current-buffer) 'visible)))
      (when win (with-selected-window win (recenter -1))))))



(defun nix-roots-query-process-next ()
  "Process the next entry in the nix-roots-query-queue if below the limit."
  (while (and (< nix-roots-query-running-count nix-roots-query-parallel-limit)
              nix-roots-query-queue)
    (let* ((root (pop nix-roots-query-queue))
           (store (nth 1 root))
           (callback (lambda (process output)
                       (string-match "^\\(.*?\\) +\\(.*?\\)$" output) ; Updated this line
                       (let ((size
                              (if (zerop (process-exit-status process))
                                  (match-string 2 output)
                                "!")))
                         (nix-roots-log-query-result (car root) size)
                         (with-current-buffer "*Nix Roots*"
                           (dolist (entry tabulated-list-entries)
                             (when (string= (car entry) (car root))
                               (setf (cadr entry) (vector (car root) store size))))
                           (tabulated-list-print t))))))
      (let ((process-connection-type nil)) ;; Use pipes instead of ptys
        (make-process :name "nix-store-query-size"
                      :buffer (generate-new-buffer "*")
                      :command (list "nix" "path-info" "--size" (car root))
                      :sentinel (lambda (process _signal)
                                  (when (memq (process-status process) '(exit signal))
                                    (progn
                                      (funcall callback process (with-current-buffer (process-buffer process) (buffer-string)))
                                      (cl-decf nix-roots-query-running-count)
                                      (nix-roots-query-process-next))))))
      (cl-incf nix-roots-query-running-count))))

(defun nix-roots-query-size-sort (entry1 entry2)
  "Custom sort function for the Size column in tabulated-list-entries."
  ;; Element 2 of the vector represents the Size field.
  ;; We use string-to-number to correctly sort by Size value instead of lexicographically.
  (let ((size1 (string-to-number (aref (cadr entry1) 2)))
        (size2 (string-to-number (aref (cadr entry2) 2))))
    (< size1 size2)))

(defun nix-roots-to-buffer (matrix)
  "Show the results as a `tabulated-list-mode' buffer."
  (switch-to-buffer "*Nix Roots*")
  (tabulated-list-mode)
  (setq tabulated-list-format
        [("Root" 120 nil)
         ("Store" 150 nil)
         ("Size" 50 nix-roots-query-size-sort)]) ; We replace `nil` with our custom sort function.
  (setq nix-roots-query-queue (mapcar (lambda (row)
                                        (list (car row) (nth 1 row)))
                                      matrix))
  (setq nix-roots-query-running-count 0)
  (setq tabulated-list-entries
        (mapcar (lambda (row)
                  (list (car row) (vector (car row) (nth 1 row) "Fetching...")))
                matrix))
  (nix-roots-query-process-next)
  (tabulated-list-init-header)
  (setq buffer-read-only t)
  (tabulated-list-print))

(provide 'nix-roots)
;;; nix-roots.el ends here
