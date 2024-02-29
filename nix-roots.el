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
;; Package-Requires: ((emacs "25.1") (dash) (s) (f))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides a command to list stray nix roots.
;;
;;; Code:


;; Custom groups
(defgroup nix-roots nil
  "Emacs package to list stray nix roots."
  :prefix "nix-roots"
  :group 'nix)

(defcustom nix-roots-command '("nix-store" "--gc" "--print-roots")
  "Command to list stray nix roots."
  :type 'list
  :group 'nix-roots)

;; Main
;;;###autoload
(defun nix-roots-list ()
  "Convert a `dts' FILE back to a `dtb' buffer."
  (interactive)
  ;; Invoke `dtc', ensuring all output is read
  (let* ( ;; (inhibit-read-only t)
         (nix-roots-buffer (get-buffer-create "nix-roots"))
         ;; (dtb-base-name (file-name-base (buffer-file-name buffer)))
         (stdout (get-buffer-create "nix-roots-list"))
         (process (make-process :name "nix-roots-list"
                                :command (append nix-roots-command)
                                :buffer stdout
                                ;; :stderr stderr
                                ;; :coding 'binary
                                :connection-type 'pipe
                                ))
         ;; (stderr-process (get-buffer-process stderr))
         )

    ;; Don't include the "Process <name> finished" messages
    (set-process-sentinel process (lambda (process event) (message "Process %s has terminated: %s" process event)))
    ;; (set-process-sentinel stderr-process (lambda (process event) (message "Process %s has terminated: %s" process event)))

    (message "Sending input")
    (with-current-buffer stdout (process-send-region process (point-min) (point-max)))
    (message "Sending EOF")
    (process-send-eof process)
    (message "Sent EOF")

    ;; (unless (and process stderr-process) (error "Process unexpectedly nil"))
    (message "Accepting process output")
    (while (accept-process-output process 1))
    ;; (message "Accepting process stderr output")
    ;; (while (accept-process-output stderr-process 1))

    (delete-process process)
    ;; (delete-process stderr-process)

    ;; Ensure `read-only-mode' is off, and clear `dtb-buffer' output from previous runs
    ;; (with-current-buffer dtb-buffer (read-only-mode 0) (erase-buffer))

    ;; Append `stdout' to `dtb-buffer'
    (with-current-buffer nix-roots-buffer (read-only-mode 0) (erase-buffer))
    (with-current-buffer stdout (append-to-buffer nix-roots-buffer (point-min) (point-max)))
    ;; (with-current-buffer nix-roots-buffer
    ;;   (erase-buffer)
    ;;   (insert
    ;;    (nix-roots--output-to-matrix (buffer-string))))

    ;; Show a message (unlikely) or popup buffer (likely) with the `dtc' stderr
    ;; (with-current-buffer stderr (save-excursion (when virtual-dts-show-stderr (display-message-or-buffer (buffer-string)))))

    ;; Delete the intermediate 'stdout' and `stderr' buffers
    (kill-buffer stdout)
    ;; (kill-buffer stderr)

    ;; Return the `dtb' buffer
    ;; (message "dtb-buffer: %s (%s)" stdout (buffer-size dtb-buffer))
    nix-roots-buffer)
  )

(defun nix-roots--output-to-text (matrix)
  "Convert MATRIX of strings to a text representation."
  (s-join "\n" (--map (s-join " " it) matrix))
  )

(defun nix-roots--output-to-matrix (output)
  "Convert OUTPUT to a matrix."
  (--map (s-split " " it) (s-lines (s-chomp output))))


;; (nix-roots--matrix-to-pairs matrix)
(defun nix-roots--matrix-to-pairs (matrix)
  ""
  (mapcar (lambda (arg) (list (nth 0 arg) (nth 2 arg))) matrix)
  )


;; One line:
;; (nix-roots--output-to-text (list (car matrix)))
;; Get first of each element:
;; (mapcar #'car matrix)

(provide 'nix-roots)
;;; nix-roots.el ends here

;; Run 'nix-store --gc --print-roots' to get list of stray roots.
;; Each line is in the format:
;; ROOT_LOCATION -> STORE_LOCATION
(defun nix-roots-query ()
  "Query nix store for stray roots."
  (shell-command-to-string "nix-store --gc --print-roots"))

;; Transform the output of `nix-roots-query' into a list of lists.
;; Result should be in form:
;; '((ROOT_LOCATION STORE_LOCATION) (ROOT_LOCATION STORE_LOCATION) ...)

(defun nix-roots-to-matrix (output)
  "Convert command output to list of lists."
  (let* ((lines (split-string output "\n" t))  ;; split the output into lines
         (roots
           (mapcar
             (lambda (line)
               (split-string line " -> " t))  ;; split each line into two parts at " -> "
            lines)))
    roots))

;; Convert the matrix results into a read-only tabulated-list-mode buffer
(defun nix-roots-to-buffer (matrix)
  "Show the results as a `tabulated-list-mode' buffer."
  ;; create a new buffer (or switch to it if it already exists)
  (switch-to-buffer "*Nix Roots*")
  ;; enable `tabulated-list-mode' for this buffer
  (tabulated-list-mode)
  ;; set the format of the table
  (setq tabulated-list-format [("Root" 120 nil) ("Store" 120 nil)])
  ;; convert the matrix to the format required by `tabulated-list-mode`
  (setq tabulated-list-entries
        (mapcar (lambda (row)
                  (list (car row)
                        (vconcat (mapcar 'identity row))))
                matrix))
  ;; generate the table
  (tabulated-list-init-header)
  ;; make the buffer read-only
  (setq buffer-read-only t)
  ;; update the contents of the buffer
  (tabulated-list-print))
