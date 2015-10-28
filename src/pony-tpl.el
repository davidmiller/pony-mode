;;
;; Pony-tpl-minor-mode
;;
;; Commentary:
;;
;; This minor mode provides syntax highlighting and some useful default
;; shortcuts for editing django template (html) files.
;;

;; pony-tpl-minor-mode begins

(defgroup pony-tpl nil
  "Djangification for Templates in Emacs"
  :group 'pony
  :prefix "pony-tpl-")

;;
;; Indentation of Django tags
;;
;; Commentary:
;;
;; This implementation "borrows" (read: Steals Liberally) from Florian Mounier's Jinja2 Mode
;; https://github.com/paradoxxxzero/jinja2-mode
;;
;; All we really do here is redefine the relevant functions, alter the keywords and
;; make sure that TAB doesn't affect (point)
;;

(defvar pony-nonindenting-tags
  '("cache" "csrf_token" "cycle" "debug" "extends" "firstof" "include" "load" "now"
    "regroup" "ssi" "templatetag" "trans" "url" "widthratio")
  "List of tags that do not imply indentation (or require an end tag).")

(defvar pony-indenting-tags
  '("autoescape" "block" "blocktrans" "comment" "elif" "else" "empty"
    "filter" "for" "if" "ifchanged" "ifequal" "ifnotequal" "plural" "spaceless" "verbatim" "with")
  "List of template tags that imply indentation.")

(defvar pony-indenting-tags-regexp
  (regexp-opt pony-indenting-tags)
  "Regular expression matching a template tag that implies indentation.")

(defun sgml-indent-line-num ()
  "Indent the current line as SGML."
  (let* ((savep (point))
         (indent-col
          (save-excursion
            (back-to-indentation)
            (if (>= (point) savep) (setq savep nil))
            (sgml-calculate-indent))))
    (if (null indent-col)
        0
      (if savep
          (save-excursion indent-col)
        indent-col))))

(defun pony-calculate-indent-backward (default)
  "Return indent column based on previous lines"
  (let ((indent-width sgml-basic-offset))
    (forward-line -1)
    (if (looking-at "^[ \t]*{%-? *end") ; Don't indent after end
        (current-indentation)
      (if (looking-at (concat "^[ \t]*{%-? *.*?{%-? *end" pony-indenting-tags-regexp "\\>"))
          (current-indentation)
        (if (looking-at (concat "^[ \t]*{%-? *" pony-indenting-tags-regexp "\\>")) ; Check start tag
            (+ (current-indentation) indent-width)
          (if (looking-at "^[ \t]*<") ; Assume sgml block trust sgml
              default
            (if (bobp)
                0
              (pony-calculate-indent-backward default))))))))

(defun pony-calculate-indent ()
  "Return indent column"
  (save-excursion
    (beginning-of-line)
    (if (bobp)  ; Check begining of buffer
        0
      (let ((indent-width sgml-basic-offset) (default (sgml-indent-line-num)))
        (if (looking-at "^[ \t]*{%-? *\\(e\\(nd\\|lse\\|lif\\|mpty\\)\\|plural\\)") ; Check close tag
            (progn
              (forward-line -1)
              (if
                  (and
                   (looking-at (concat "^[ \t]*{%-? *" pony-indenting-tags-regexp "\\>"))
                   (not (looking-at (concat "^[ \t]*{%-? *.*?{% *end" pony-indenting-tags-regexp "\\>"))))
                  (current-indentation)
                (- (current-indentation) indent-width)))
          (if (looking-at "^[ \t]*</") ; Assume sgml end block trust sgml
              default
            (pony-calculate-indent-backward default)))))))

(defun pony-indent ()
  "Indent current line as Jinja code"
  (interactive)
  (let ((pos (- (point-max) (point)))
        (indent (pony-calculate-indent)))
    (if (< indent 0)
        (setq indent 0))
    (indent-line-to indent)
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (let ((moved-pos (- (point-max) pos)))
     (if (> moved-pos (point))
         (goto-char moved-pos)))))

(defvar pony-tpl-mode-hook nil)

(defconst pony-tpl-font-lock-keywords
  (append
   sgml-font-lock-keywords
   (list
    '("{%.*\\(\\bor\\b\\).*%}" . (1 font-lock-builtin-face))
    ;'("{% ?comment ?%}\\(\n?.*?\\)+?{% ?endcomment ?%}" . font-lock-comment-face)
    '("{#.*#}" . font-lock-comment-face)
    (cons (concat "{% *\\(\\(?:end\\)?" (regexp-opt pony-indenting-tags) "\\|" (regexp-opt pony-nonindenting-tags) "\\)\\>.*?%}") 1)
    '("{{ ?\\(.*?\\) ?}}" . (1 font-lock-variable-name-face))
    '("{%\\|\\%}\\|{{\\|}}" . font-lock-builtin-face)
    ))
  "Highlighting for pony-tpl-mode")

(define-minor-mode pony-tpl-minor-mode
  "Pony-templatin-riffic"
  :initial nil
  :lighter " PonyTpl"
  :keymap pony-minor-mode-map)

(defun pony-tpl-mode()
  "Minor mode for editing pony templates"
  (interactive)
  (pony-tpl-minor-mode t)
  (run-hooks 'pony-tpl-mode-hook)
  (set (make-local-variable 'font-lock-defaults)
       '(pony-tpl-font-lock-keywords))
  (if (> emacs-major-version 23)
      (font-lock-refresh-defaults))
  (set (make-local-variable 'indent-line-function) 'pony-indent)
   (pony-load-snippets))

;; pony-tpl-minor-mode ends
