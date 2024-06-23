;;; prettier-js.el --- Minor mode to format JS code on file save -*- lexical-binding: t; -*-

;; Copyright (c) 2014 The go-mode Authors. All rights reserved.
;; Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

;; Version: 0.1.0
;; Author: James Long and contributors
;;         Karim Aziiev <karim.aziiev@gmail.com>
;; Created: 10 January 2017
;; Url: https://github.com/KarimAziev/prettier-emacs
;; Keywords: convenience wp edit js
;; Package-Requires: ((emacs "29.1"))


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


(defcustom prettier-js-buffer-global-args '("--single-quote"
                                            "--bracket-same-line"
                                            "--jsx-single-quote"
                                            "--html-whitespace-sensitivity=ignore"
                                            "--ignore-unknown")
  "List of default global args to send to prettier command."
  :type '(repeat string)
  :group 'prettier-js)

(defcustom prettier-js-buffer-blacklist-regexp "/\\(tmp\\|node_modules\\)"
  "Regexp of directories to disable prettier setup."
  :type 'regexp
  :group 'prettier-js)


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
                                                    (gfm-mode . "markdown")
                                                    (js-mode . "babel")
                                                    (js2-mode . "babel")
                                                    (js-ts-mode . "babel")
                                                    (typescript-mode . "typescript")
                                                    (tsx-ts-mode . "typescript")
                                                    (typescript-ts-mode . "typescript")
                                                    (json-mode . "json")
                                                    (css-mode . "css")
                                                    (css-ts-mode . "css")
                                                    (html-ts-mode . "html")
                                                    (html-mode . "html")
                                                    (scss-mode . "scss")
                                                    (yaml-mode . "yaml"))
  "Alist mapping major modes to Prettier parsers.

An alist mapping major modes to Prettier parsers.

Each element is a cons cell `(major-mode . parser)`, where `major-mode' is a
symbol representing an Emacs major mode, and `parser' is a string specifying the
Prettier parser to use for files in that mode.

When formatting a buffer, the appropriate parser is selected based on the
buffer's major mode. If the major mode of the current buffer is present in this
alist, its corresponding parser is used as an argument to the Prettier command.

Ensure that the parsers specified are valid according to the Prettier
documentation and that the corresponding Prettier plugins are installed."
  :group 'prettier-js
  :type '(alist
          :key-type (symbol :tag "Major mode")
          :value-type (string :tag "Plugin")))

(defcustom prettier-js-buffer-plugins-alist '((sh-base-mode . "prettier-plugin-sh"))
  "Alist mapping major modes to Prettier plugins.

An association list mapping major modes to Prettier plugins used for formatting
buffers in those modes. Each element of the list is a cons cell where the car is
the major mode symbol and the cdr is a string specifying the name of the
Prettier plugin to use.

To configure a Prettier plugin for a specific major mode, add an entry to the
list with the major mode symbol and the corresponding plugin name. For example,
to use the \"prettier-plugin-sh\" for `sh-base-mode', add the following to the
list:

`(sh-base-mode . \"prettier-plugin-sh\")`

When formatting a buffer, if the buffer's major mode matches one of the modes in
the list, the specified plugin will be used by Prettier. If no entry for the
major mode is found, Prettier will use its default behavior for determining the
appropriate plugin."
  :group 'prettier-js
  :type '(alist
          :key-type (symbol :tag "Major mode")
          :value-type (string :tag "Parser")))

(defcustom prettier-js-config-names '(".prettierrc" ".prettierrc.json"
                                      ".prettierrc.yml"
                                      ".prettierrc.yaml" ".prettierrc.json5"
                                      ".prettierrc.js"
                                      "prettier.config.js" ".prettierrc.mjs"
                                      "prettier.config.mjs" ".prettierrc.cjs"
                                      "prettier.config.cjs" ".prettierrc.toml")
  "List of filenames recognized as Prettier configuration files.

A list of filenames to search for Prettier configuration files.

Each element should be a string representing a possible filename
for a Prettier configuration file. The search will check for these
files in the current directory and its parent directories."
  :group 'prettier-js
  :type '(repeat
          (string :tag "Config filename")))


(defun prettier-js-buffer-local-command ()
  "Return local command for prettier."
  (when-let ((dir (locate-dominating-file
                   default-directory
                   "node_modules/.bin/prettier")))
    (expand-file-name "node_modules/.bin/prettier" dir)))

(defun prettier-js-resolve-config-file ()
  "Locate and return the path of the nearest Prettier configuration file."
  (let ((curr-dir default-directory)
        (config-names prettier-js-config-names)
        (found))
    (while (and config-names
                (not found))
      (let* ((curr (car config-names))
             (dir (locate-dominating-file curr-dir curr)))
        (if dir
            (setq found (expand-file-name curr dir))
          (setq config-names (cdr config-names)))))
    found))

(defun prettier-js-buffer-string (string &rest options)
  "Apply prettier on STRING with OPTIONS.
Return list of two elements: status (t or nil) and string with result."
  (when-let ((prettier-cmd prettier-js-command))
    (setq options (delq nil
                        (flatten-list options)))
    (with-temp-buffer
      (insert string)
      (cond ((zerop (apply #'call-process-region
                           (append
                            (list (point-min)
                                  (point-max)
                                  prettier-cmd
                                  t
                                  t
                                  nil)
                            options)))
             (message "Applied prettier `%s' with args `%s'"
                      prettier-js-command options)
             (buffer-string))
            (t (message "prettier errors with args: `%s': %s" options
                        (buffer-string))
               nil)))))

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



(defun prettier-js-next-read-only-property-change ()
  "Jump to the position of next read-only property change.

Return the position of point if found, or nil."
  (when-let ((beg (next-single-property-change (point) 'read-only)))
    (goto-char beg)
    beg))

(defun prettier-js-format-non-readonly-regions ()
  "Format non-read-only regions with Prettier."
  (save-excursion
    (let* ((args prettier-js-args)
           (res)
           (fn (lambda ()
                 (let* ((start (point))
                        (end
                         (prettier-js-next-read-only-property-change))
                        (content
                         (buffer-substring-no-properties start
                                                         (or end (point-max)))))
                   (prettier-js-buffer-format-region start (or end (point-max))
                                                     args)
                   (push content res)))))
      (goto-char (point-min))
      (unless (get-text-property (point) 'read-only)
        (funcall fn))
      (while
          (when (get-text-property (point) 'read-only)
            (prettier-js-next-read-only-property-change))
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
            (error
             "Invalid rcs patch or internal error in prettier-js--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond ((equal action "a")
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
                   (error
                    "Invalid rcs patch or internal error in prettier-js--apply-rcs-patch")))))))))

(defun prettier-js--process-errors (filename errorfile errbuf)
  "Process errors for FILENAME, using an ERRORFILE.
Display the output in ERRBUF"
  (let ((error-output prettier-js-show-errors))
    (with-current-buffer errbuf
      (if (eq error-output 'echo)
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
        (display-buffer errbuf)))))

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
         (cmd prettier-js-command)
         (args (append prettier-js-args
                       (list
                        "--stdin-filepath"
                        buffer-file-name))))
    (unwind-protect
        (save-restriction
          (widen)
          (write-region nil nil bufferfile)
          (when errbuf
            (with-current-buffer errbuf
              (setq buffer-read-only nil)
              (erase-buffer)))
          (with-current-buffer patchbuf
            (erase-buffer))
          (cond ((zerop (apply #'call-process cmd bufferfile (list
                                                              (list :file
                                                                    outputfile)
                                                              errorfile)
                               nil args))
                 (call-process-region (point-min)
                                      (point-max) "diff" nil patchbuf nil "-n"
                                      "--strip-trailing-cr" "-"
                                      outputfile)
                 (let ((size (with-temp-buffer
                               (insert-file-contents outputfile)
                               (buffer-size))))
                   (unless (zerop size)
                     (prettier-js--apply-rcs-patch patchbuf)
                     (message (concat (format "Applied prettier `%s'" cmd)
                                      (if prettier-js-args
                                          (format " with args `%s'"
                                                  prettier-js-args)
                                        "")))
                     (when errbuf
                       (prettier-js--kill-error-buffer errbuf)))))
                (t (message "Could not apply prettier")
                   (when errbuf
                     (prettier-js--process-errors
                      (buffer-file-name)
                      errorfile
                      errbuf)))))
      (kill-buffer patchbuf)
      (delete-file errorfile)
      (delete-file bufferfile)
      (delete-file outputfile))))

(declare-function json-read "json")
(defvar json-object-type)
(defvar json-array-type)
(defvar json-null)
(defvar json-false)

(defun prettier-js-read-json-file (file &optional object-type array-type
                                        null-object false-object)
  "Parse FILE with natively compiled function or with json library.

The argument OBJECT-TYPE specifies which Lisp type is used
to represent objects; it can be `hash-table', `alist' or `plist'.  It
defaults to `alist'.

The argument ARRAY-TYPE specifies which Lisp type is used
to represent arrays; `array' or `vector' and `list'.

The argument NULL-OBJECT specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The argument FALSE-OBJECT specifies which object to use to
represent a JSON false value.  It defaults to `:false'."
  (if (and (fboundp 'json-parse-string)
           (fboundp 'json-available-p)
           (json-available-p))
      (with-temp-buffer (insert-file-contents file)
                        (goto-char (point-min))
                        (json-parse-buffer
                         :object-type (or object-type 'alist)
                         :array-type
                         (pcase array-type
                           ('list 'list)
                           ('vector 'array)
                           (_ 'array))
                         :null-object (or null-object :null)
                         :false-object (or false-object :false)))
    (with-temp-buffer
      (require 'json)
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((json-object-type (or object-type 'alist))
            (json-array-type
             (pcase array-type
               ('list 'list)
               ('array 'vector)
               (_ 'vector)))
            (json-null (or null-object :null))
            (json-false (or false-object :false)))
        (json-read)))))

(defun prettier-js-setup-plugin (plugin mode)
  "Set up Prettier PLUGIN for specified MODE.

Argument PLUGIN is a string specifying the name of the prettier plugin to set
up.

Argument MODE is a symbol representing the major mode in which the PLUGIN should
be set up."
  (when (derived-mode-p mode)
    (when-let* ((prettiery-bin (file-truename prettier-js-command))
                (file (file-truename prettiery-bin))
                (ext (file-name-extension file))
                (dir
                 (when (and ext (string= ext "cjs"))
                   (file-name-directory file)))
                (node_modules-parent-dir (locate-dominating-file dir
                                                                 "node_modules"))
                (package-json (expand-file-name
                               (concat "node_modules/" plugin "/package.json")
                               node_modules-parent-dir))
                (main
                 (when (file-exists-p package-json)
                   (alist-get 'main (prettier-js-read-json-file package-json))))
                (plugin-file (expand-file-name
                              main
                              (expand-file-name (concat "node_modules/" plugin)
                                                node_modules-parent-dir))))
      (when (file-exists-p plugin-file)
        (setq prettier-js-args (prettier-js-merge-args prettier-js-args
                                                       (list "--plugin"
                                                             plugin-file)))))))


(defun prettier-js-has-arg-p (arg args)
  "Check if ARG is present in ARGS, either directly or as a prefix.

Argument ARG is the string to search for in ARGS.

Argument ARGS is a list of strings to be searched."
  (or (member arg args)
      (seq-find
       (apply-partially #'string-prefix-p (concat arg "="))
       args)))


(defun prettier-js-remove-arg (arg args)
  "Remove specified ARG from ARGS, handling value associations.

Argument ARG is the argument to remove from ARGS.

Argument ARGS is a list of arguments from which ARG should be removed."
  (let* ((pos (seq-position args arg))
         (value
          (and pos
               (nth (1+ pos) args)))
         (arg-prefix (concat arg "="))
         (arg-prefix-val))
    (when (and value
               (string-prefix-p "-" value))
      (setq value nil))
    (cond ((and pos
                (not value))
           (remove arg args))
          ((and pos value)
           (append (seq-subseq args 0 (max 0 (1- pos)))
                   (seq-subseq args (1+ (1+ pos)))))
          ((setq arg-prefix-val (seq-find
                                 (apply-partially #'string-prefix-p arg-prefix)
                                 args))
           (remove arg-prefix-val args))
          (t args))))

(defun prettier-js-get-arg-value (arg args)
  "Extract the value of a specified argument ARG from a list ARGS.

Argument ARG is the argument name to search for in ARGS.

Argument ARGS is a list of command-line arguments."
  (let ((value (cadr (member arg args))))
    (if (and value
             (not (string-prefix-p "-" value)))
        value
      (when-let ((found (seq-find
                         (apply-partially #'string-prefix-p (concat arg "="))
                         args)))
        (substring-no-properties found (1+ (length arg)))))))

(defun prettier-js-merge-args (args override-args)
  "Merge ARGS with OVERRIDE-ARGS, removing duplicates based on `arg' names.

Argument ARGS is a list of strings representing arguments to merge.

Argument OVERRIDE-ARGS is a list of strings representing arguments that will
override ARGS."
  (let ((result))
    (while args
      (let* ((arg (pop args))
             (val-idx (and arg
                           (string-match-p "=" arg)))
             (val (if val-idx
                      (substring-no-properties arg (1+ val-idx))
                    (and (car args)
                         (not (string-prefix-p "-" (car args)))
                         (pop args)))))
        (when val-idx
          (setq arg (substring-no-properties arg 0 val-idx)))
        (unless (prettier-js-has-arg-p arg override-args)
          (setq result (append result (if val-idx
                                          (list (concat arg "=" val))
                                        (delq nil (list arg val))))))))
    (append result override-args)))


(defun prettier-js-setup ()
  "Set up the Prettier command for the current buffer.

Determine the appropriate Prettier parser based on the buffer's major mode.

Append necessary arguments to the Prettier command.

Associate Prettier plugins with corresponding major modes."
  (if-let ((local-prettier
            (when buffer-file-name
              (prettier-js-buffer-local-command))))
      (setq-local prettier-js-command local-prettier)
    (let* ((conf-file (prettier-js-resolve-config-file))
           (args
            (if conf-file
                (list "--config" conf-file)
              (prettier-js-merge-args
               prettier-js-buffer-global-args
               prettier-js-args)))
           (parser
            (unless (or (and buffer-file-name
                             (file-name-extension buffer-file-name))
                        (seq-find
                         (apply-partially #'string-prefix-p "--parser=")
                         args))
              (cdr (assq major-mode
                         prettier-js-buffer-major-modes-parsers)))))
      (setq-local prettier-js-args
                  (if parser
                      (prettier-js-merge-args
                       args
                       (list (concat "--parser=" parser)))
                    args))
      (setq-local prettier-js-command (executable-find "prettier"))
      (setq-local prettier-js-show-errors 'echo)
      (pcase-dolist (`(,mode . ,plugin) prettier-js-buffer-plugins-alist)
        (prettier-js-setup-plugin plugin mode)))))

(defun prettier-js-buffer-or-region ()
  "Format non-read-only regions or the whole buffer using Prettier."
  (cond ((save-excursion
           (goto-char (point-min))
           (prettier-js-next-read-only-property-change))
         (prettier-js-format-non-readonly-regions))
        ((and (region-active-p)
              (use-region-p))
         (prettier-js-buffer-format-region
          (region-beginning)
          (region-end)
          prettier-js-args))
        ((and buffer-file-name)
         (prettier-js-current-file))
        (t (prettier-js-buffer-format-region
            (point-min)
            (point-max)
            prettier-js-args))))



;;;###autoload
(defun prettier-js ()
  "Format code using Prettier."
  (interactive)
  (prettier-js-setup)
  (prettier-js-buffer-or-region))

;;;###autoload
(define-minor-mode prettier-js-mode
  "Runs prettier on file save when this mode is turned on."
  :lighter " Prettier"
  :global nil
  (remove-hook 'before-save-hook #'prettier-js-buffer-or-region 'local)
  (when prettier-js-mode
    (prettier-js-setup)
    (add-hook 'before-save-hook #'prettier-js-buffer-or-region nil
              'local)))


(provide 'prettier-js)
;;; prettier-js.el ends here
