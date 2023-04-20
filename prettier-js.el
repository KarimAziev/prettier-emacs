;;; prettier-js.el --- Minor mode to format JS code on file save -*- lexical-binding: t; -*-

;; Copyright (c) 2014 The go-mode Authors. All rights reserved.
;; Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

;; Version: 0.1.0
;; Author: James Long and contributors
;;         Karim Aziiev <karim.aziiev@gmail.com>
;; Created: 10 January 2017
;; Url: https://github.com/prettier/prettier-emacs
;; Keywords: convenience wp edit js
;; Package-Requires: ((emacs "27.1") (transient "0.3.7"))



;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;; * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;; * Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following disclaimer
;; in the documentation and/or other materials provided with the
;; distribution.
;; * Neither the name of the copyright holder nor the names of its
;; contributors may be used to endorse or promote products derived from
;; this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.)


;; This file is not part of GNU Emacs.

;;; Commentary:
;; Formats your JavaScript code using 'prettier' on file save.

;;; Code:

(defgroup prettier-js nil
  "Minor mode to format JS code on file save."
  :group 'languages
  :prefix "prettier-js"
  :link '(url-link :tag "Repository" "https://github.com/prettier/prettier"))

(defcustom prettier-js-command "prettier"
  "The `prettier' command."
  :type 'string
  :group 'prettier-js)

(make-variable-buffer-local 'prettier-js-command)

(defcustom prettier-js-args '()
  "List of args to send to prettier command."
  :type '(repeat string)
  :group 'prettier-js)

(make-variable-buffer-local 'prettier-js-args)

(defcustom prettier-js-show-errors 'buffer
    "Where to display prettier error output.
It can either be displayed in its own buffer, in the echo area, or not at all.
Please note that Emacs outputs to the echo area when writing
files and will overwrite prettier's echo output if used from inside
a `before-save-hook'."
    :type '(choice
            (const :tag "Own buffer" buffer)
            (const :tag "Echo area" echo)
            (const :tag "None" nil))
      :group 'prettier-js)

(defcustom prettier-js-width-mode nil
  "Specify width when formatting buffer contents."
  :type '(choice
          (const :tag "Window width" window)
          (const :tag "Fill column" fill)
          (const :tag "None" nil))
  :group 'prettier-js)

(defcustom prettier-js-buffer-global-args '("--single-quote")
  "List of default global args to send to prettier command."
  :type '(repeat string)
  :group 'prettier-js-buffer-global-args)

(defcustom prettier-js-buffer-blacklist-regexp "/\\(tmp\\|node_modules\\)"
  "Regexp of directories to disable prettier setup."
  :type 'regexp
  :group 'prettier-js-buffer-global-args)


(defcustom prettier-js-buffer-parsers '(("md" . "markdown")
                                        ("mdx" . "markdown")
                                        ("js" . "babel")
                                        ("jsx" . "babel")
                                        ("ts" . "typescript")
                                        ("tsx" . "typescript")
                                        ("json" . "json")
                                        ("css" . "css")
                                        ("scss" . "scss")
                                        ("less" . "less")
                                        ("json5" . "json5")
                                        ("yaml" . "yaml")
                                        ("yml" . "yaml")
                                        ("html" . "html")
                                        ("heapprofile" . "json"))
  "Alist of allowed file extensions and corresponding prettier parsers."
  :group 'prettier-js
  :type '(alist
          :key-type (string :tag "File extension")
          :value-type (string :tag "Parser")))

(defcustom prettier-js-buffer-major-modes-parsers '((markdown-mode . "markdown")
                                                    (gfm-mode "markdown")
                                                    (js-mode . "babel")
                                                    (js2-mode . "babel")
                                                    (js-ts-mode . "babel")
                                                    (typescript-mode .
                                                                     "typescript")
                                                    (tsx-ts-mode . "typescript")
                                                    (typescript-ts-mode .
                                                                        "typescript")
                                                    (json-mode . "json")
                                                    (css-mode . "css")
                                                    (css-ts-mode . "css")
                                                    (html-ts-mode . "html")
                                                    (html-mode . "html")
                                                    (scss-mode . "scss")
                                                    (yaml-mode . "yaml"))
  "Alist of allowed major modes and corresponding prettier parsers."
  :group 'prettier-js
  :type '(alist
          :key-type (symbol :tag "Major mode")
          :value-type (string :tag "Parser")))


(defun prettier-js-buffer-find-exec ()
  "Return prettier executable, either from node_modules or globally."
  (let ((dir default-directory)
        (node-modules)
        (found))
    (while (setq node-modules
                 (unless found
                   (setq dir (locate-dominating-file
                              dir
                              "node_modules"))))
      (setq dir (let ((parent (file-name-directory
                               (directory-file-name
                                (expand-file-name dir default-directory)))))
                  (when (and
                         (file-exists-p dir)
                         (file-exists-p parent)
                         (not (equal
                               (file-truename (directory-file-name
                                               (expand-file-name dir)))
                               (file-truename (directory-file-name
                                               (expand-file-name parent))))))
                    (if (file-name-absolute-p dir)
                        (directory-file-name parent)
                      (file-relative-name parent)))))
      (let ((file (expand-file-name "node_modules/.bin/prettier" node-modules)))
        (setq found
              (when (and (file-exists-p file)
                         (file-executable-p file))
                file))))
    (or found (executable-find "prettier"))))

(defun prettier-js-buffer-node-modules-bin-files ()
  "Look up directory hierarchy for executable files in node_modules/.bin."
  (when-let* ((node-modules
               (locate-dominating-file
                default-directory
                "node_modules"))
              (exec-dir
               (expand-file-name "node_modules/.bin/" node-modules))
              (commands
               (seq-filter #'file-executable-p
                           (and (file-exists-p exec-dir)
                                (directory-files-recursively exec-dir ".")))))
    commands))

(defun prettier-js-buffer-local-command ()
  "Return local command for prettier."
  (seq-find (lambda (it)
              (string= "prettier"
                       (file-name-base it)))
            (prettier-js-buffer-node-modules-bin-files)))


(defun prettier-js-buffer-setup ()
  "Enable prettier from project directory."
  (unless (or (not prettier-js-buffer-blacklist-regexp)
              (string-match-p "\\(snippets\\|node_modules\\)"
                              default-directory))
    (let ((local-cmd (prettier-js-buffer-local-command)))
      (setq-local prettier-js-command
                  (or local-cmd
                      (executable-find "prettier")))
      (when (and prettier-js-command
                 (not local-cmd))
        (setq-local prettier-js-args prettier-js-buffer-global-args))
      (when (fboundp 'prettier-js-mode)
        (prettier-js-mode (if prettier-js-command 1 -1))))))

(defun prettier-js-buffer-string (string &rest options)
  "Apply prettier on STRING with OPTIONS.
Return list of two elements: status (t or nil) and string with result."
  (when-let ((prettier-cmd (prettier-js-buffer-find-exec)))
    (with-temp-buffer
      (insert string)
      (when (eq 0
                (apply #'call-process-region
                       (append
                        (list (point-min)
                              (point-max)
                              prettier-cmd
                              t
                              t
                              nil)
                        (delq nil
                              (flatten-list options)))))
        (buffer-string)))))

(defun prettier-js-buffer-format-region (beg end &rest args)
  "Run PRETTIER-FN with ARGS on region between BEG and END."
  (let* ((buff (current-buffer))
         (content (buffer-substring-no-properties
                   beg
                   end))
         (formatted (prettier-js-buffer-string
                     content
                     args)))
    (when (and formatted
               (not (string= content formatted)))
      (with-current-buffer buff
        (delete-region beg end)
        (insert formatted)
        formatted))))


;;;###autoload
(defun prettier-js-buffer (&optional arg)
  "Set and run prettier from project directory, if found othervise as global.
With prefix ARG ask which parser to use."
  (interactive "P")
  (if-let ((local-prettier
            (when buffer-file-name
              (prettier-js-buffer-local-command))))
      (setq-local prettier-js-command (prettier-js-buffer-find-exec))
    (let ((args
           prettier-js-buffer-global-args)
          (parser (or (and buffer-file-name
                           (cdr
                            (assoc (file-name-extension buffer-file-name)
                                   prettier-js-buffer-parsers)))
                      (cdr  (assoc major-mode
                                   prettier-js-buffer-major-modes-parsers)))))
      (setq-local prettier-js-args
                  (if parser
                      (append (list (concat "--parser=" parser)) args)
                    args))
      (setq-local prettier-js-command (or (prettier-js-buffer-local-command)
                                          (executable-find "prettier")))
      (setq-local prettier-js-show-errors 'echo)))
  (when prettier-js-command
    (setq-local prettier-js-args
                (delete nil (if arg
                                (append
                                 prettier-js-args
                                 (list (concat "--parser="
                                               (completing-read
                                                "--parser="
                                                (seq-uniq
                                                 (mapcar #'cdr
                                                         prettier-js-buffer-parsers))))))
                              prettier-js-args)))
    (condition-case nil
        (progn (if prettier-js-command
                   (prettier-js-mode 1)
                 (prettier-js-mode -1))
               (prettier-js))
      (error (prettier-js-mode -1)
             (when-let ((formatted (prettier-js-buffer-string
                                    (buffer-substring-no-properties
                                     (point-min)
                                     (point-max))
                                    prettier-js-args)))
               (if (fboundp 'replace-region-contents)
                   (replace-region-contents (point-min)
                                            (point-max)
                                            (lambda () formatted))
                 (delete-region (point-min)
                                (point-max))
                 (insert formatted)))))))

(defun prettier-js-buffer-search-text-prop (prop &optional direction)
  "Search for text property PROP in current buffer.
If DIRECTION is negative integer, search backward, othervise forward."
  (unless direction (setq direction (setq direction 1)))
  (let ((search-fn (if (> direction 0)
                       'text-property-search-forward
                     'text-property-search-backward)))
    (if-let ((ref (get-text-property (point) prop)))
        (let ((pos (if (> direction 0)
                       (next-single-char-property-change (point) prop)
                     (previous-single-char-property-change (point) prop))))
          (goto-char pos)
          (funcall search-fn prop))
      (funcall search-fn prop))))

;;;###autoload
(defun prettier-js-buffer-region ()
  "Run PRETTIER-FN with ARGS or `prettier-js'.
If value of the variable `buffer-file-name' is nil, run `prettier-js',
otherwise run prettier-fn."
  (interactive)
  (pcase-let* ((`(,beg . ,end)
                (if (and (region-active-p)
                         (use-region-p))
                    (cons (region-beginning)
                          (region-end))
                  (cons  (point-min)
                         (point-max))))
               (parser
                (or (and buffer-file-name
                         (cdr
                          (assoc (file-name-extension buffer-file-name)
                                 prettier-js-buffer-parsers)))
                    (cdr  (assoc major-mode
                                 prettier-js-buffer-major-modes-parsers)))))
    (prettier-js-buffer-format-region
     beg end
     (concat "--parser=" parser))))



(defun prettier-js-buffer-next-single-char-property-change ()
  "Jump to the position of next read-only property change.
Return the position of point if found, or nil."
  (when-let ((beg (next-single-property-change (point) 'read-only)))
    (goto-char beg)
    beg))

(defun prettier-js-format-non-readonly-regions ()
  "Run PRETTIER-FN with ARGS or `prettier-js'.
If value of the variable `buffer-file-name' is nil, run `prettier-js',
otherwise run prettier-fn."
  (save-excursion
    (let* ((parser
            (or (and buffer-file-name
                     (cdr
                      (assoc (file-name-extension buffer-file-name)
                             prettier-js-buffer-parsers)))
                (cdr  (assoc major-mode
                             prettier-js-buffer-major-modes-parsers))))
           (res)
           (fn (lambda ()
                 (let* ((start (point))
                        (end
                         (prettier-js-buffer-next-single-char-property-change))
                        (content
                         (buffer-substring-no-properties start
                                                         (or end (point-max)))))
                   (prettier-js-buffer-format-region start (or end (point-max))
                                                     "--parser=" parser)
                   (push content
                         res)))))
      (goto-char (point-min))
      (unless (get-text-property (point) 'read-only)
        (funcall fn))
      (while
          (when (get-text-property (point) 'read-only)
            (prettier-js-buffer-next-single-char-property-change))
        (funcall fn))
      res)))



(defun prettier-js--goto-line (line)
  "Move cursor to line LINE."
  (goto-char (point-min))
    (forward-line (1- line)))

(defun prettier-js--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in prettier-js--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (setq line-offset (- line-offset len))
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (prettier-js--goto-line (- from line-offset))
                (setq line-offset (+ line-offset len))
                (let ((beg (point)))
                  (forward-line len)
                  (delete-region (point) beg))))
             (t
              (error "Invalid rcs patch or internal error in prettier-js--apply-rcs-patch")))))))))

(defun prettier-js--process-errors (filename errorfile errbuf)
  "Process errors for FILENAME, using an ERRORFILE.
Display the output in ERRBUF"
  (with-current-buffer errbuf
    (if (eq prettier-js-show-errors 'echo)
        (progn
          (message "%s" (buffer-string))
          (prettier-js--kill-error-buffer errbuf))
      (insert-file-contents errorfile nil nil nil)
      ;; Convert the prettier stderr to something understood by the compilation mode.
      (goto-char (point-min))
      (insert "prettier errors:\n")
      (while (search-forward-regexp "^stdin" nil t)
        (replace-match (file-name-nondirectory filename)))
      (compilation-mode)
      (display-buffer errbuf))))

(defun prettier-js--kill-error-buffer (errbuf)
  "Kill buffer ERRBUF."
  (let ((win (get-buffer-window errbuf)))
    (if win
        (quit-window t win)
      (with-current-buffer errbuf
        (erase-buffer))
      (kill-buffer errbuf))))


(defun prettier-js-current-file ()
  "Format the current buffer according to the prettier tool."
  (let* ((ext (file-name-extension buffer-file-name t))
         (bufferfile (make-temp-file "prettier" nil ext))
         (outputfile (make-temp-file "prettier" nil ext))
         (errorfile (make-temp-file "prettier" nil ext))
         (errbuf (if prettier-js-show-errors (get-buffer-create
                                              "*prettier errors*")))
         (patchbuf (get-buffer-create "*prettier patch*"))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (width-args
          (cond ((equal prettier-js-width-mode 'window)
                 (list "--print-width" (number-to-string (window-body-width))))
                ((equal prettier-js-width-mode 'fill)
                 (list "--print-width" (number-to-string fill-column)))
                (t
                 '()))))
    (unwind-protect
        (save-restriction
          (widen)
          (write-region nil nil bufferfile)
          (if errbuf
              (with-current-buffer errbuf
                (setq buffer-read-only nil)
                (erase-buffer)))
          (with-current-buffer patchbuf
            (erase-buffer))
          (if
              (zerop
               (apply #'call-process
                      prettier-js-command bufferfile (list
                                                      (list :file
                                                            outputfile)
                                                      errorfile)
                      nil
                      (append prettier-js-args width-args
                              (list
                               "--stdin"
                               "--stdin-filepath"
                               buffer-file-name))))
              (progn
                (call-process-region (point-min)
                                     (point-max) "diff" nil patchbuf nil "-n"
                                     "--strip-trailing-cr" "-"
                                     outputfile)
                (prettier-js--apply-rcs-patch patchbuf)
                (message "Applied prettier with args `%s'" prettier-js-args)
                (if errbuf (prettier-js--kill-error-buffer errbuf)))
            (message "Could not apply prettier")
            (if errbuf
                (prettier-js--process-errors (buffer-file-name) errorfile
                                             errbuf))))
      (kill-buffer patchbuf)
      (delete-file errorfile)
      (delete-file bufferfile)
      (delete-file outputfile))))

(defun prettier-js-setup ()
  "Run PRETTIER-FN with ARGS or `prettier-js'.
If value of the variable `buffer-file-name' is nil, run `prettier-js',
otherwise run prettier-fn."
  (if-let ((local-prettier
            (when buffer-file-name
              (prettier-js-buffer-local-command))))
      (setq-local prettier-js-command local-prettier)
    (let ((args
           prettier-js-buffer-global-args)
          (parser (or (and buffer-file-name
                           (cdr
                            (assoc (file-name-extension buffer-file-name)
                                   prettier-js-buffer-parsers)))
                      (cdr  (assoc major-mode
                                   prettier-js-buffer-major-modes-parsers)))))
      (setq-local prettier-js-args
                  (if parser
                      (append (list "--parser=" parser) args)
                    args))
      (setq-local prettier-js-command (or (prettier-js-buffer-local-command)
                                          (executable-find "prettier")))
      (setq-local prettier-js-show-errors 'echo))))

(defun prettier-js-buffer-or-region ()
  "Run PRETTIER-FN with ARGS or `prettier-js'.
If value of the variable `buffer-file-name' is nil, run `prettier-js',
otherwise run prettier-fn."
  (prettier-js-setup)
  (cond ((save-excursion
           (goto-char (point-min))
           (prettier-js-buffer-next-single-char-property-change))
         (prettier-js-format-non-readonly-regions))
        ((and (region-active-p)
              (use-region-p))
         (prettier-js-buffer-format-region
          (region-beginning)
          (region-end)
          prettier-js-args))
        ((and buffer-file-name)
         (prettier-js-current-file))
        (t          (prettier-js-buffer-format-region
                     (point-min)
                     (point-max)
                     prettier-js-args))))



;;;###autoload
(defun prettier-js ()
  "Format the current buffer according to the prettier tool."
  (interactive)
  (prettier-js-buffer-or-region))

;;;###autoload
(define-minor-mode prettier-js-mode
  "Runs prettier on file save when this mode is turned on."
  :lighter " Prettier"
  :global nil
  (if prettier-js-mode
      (add-hook 'before-save-hook #'prettier-js-buffer-or-region nil 'local)
    (remove-hook 'before-save-hook #'prettier-js-buffer-or-region 'local)))


(provide 'prettier-js)
;;; prettier-js.el ends here
