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

(defun pony-indenting-keywords ()
  '("if" "for" "block" "else" "elif"))

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
  (let ((indent-width sgml-basic-offset) (default (sgml-indent-line-num)))
    (forward-line -1)
    (if (looking-at "^[ \t]*{%-? *end") ; Don't indent after end
        (current-indentation)
      (if (looking-at (concat "^[ \t]*{%-? *.*?{%-? *end" (regexp-opt (pony-indenting-keywords))))
          (current-indentation)
        (if (looking-at (concat "^[ \t]*{%-? *" (regexp-opt (pony-indenting-keywords)))) ; Check start tag
            (+ (current-indentation) indent-width)
          (if (looking-at "^[ \t]*<") ; Assume sgml block trust sgml
              default
            (if (bobp)
                0
              (pony-calculate-indent-backward default))))))))

(defun pony-calculate-indent ()
  "Return indent column"
  (if (bobp)  ; Check begining of buffer
      0
    (let ((indent-width sgml-basic-offset) (default (sgml-indent-line-num)))
      (if (looking-at "^[ \t]*{%-? *e\\(nd\\|lse\\|lif\\)") ; Check close tag
          (save-excursion
            (forward-line -1)
            (if
                (and
                 (looking-at (concat "^[ \t]*{%-? *" (regexp-opt (pony-indenting-keywords))))
                 (not (looking-at (concat "^[ \t]*{%-? *.*?{% *end" (regexp-opt (pony-indenting-keywords))))))
                (current-indentation)
              (- (current-indentation) indent-width)))
        (if (looking-at "^[ \t]*</") ; Assume sgml end block trust sgml
            default
          (save-excursion
            (pony-calculate-indent-backward default)))))))

(defun pony-indent ()
  "Indent current line as Jinja code"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((indent (pony-calculate-indent)))
      (if (< indent 0)
          (setq indent 0))
      (indent-line-to indent))))

(defvar pony-tpl-mode-hook nil)

(defconst pony-tpl-font-lock-keywords
  (append
   sgml-font-lock-keywords
   (list
    '("{%.*\\(\\bor\\b\\).*%}" . (1 font-lock-builtin-face))
    ;'("{% ?comment ?%}\\(\n?.*?\\)+?{% ?endcomment ?%}" . font-lock-comment-face)
    '("{#.*#}" . font-lock-comment-face)
    '("{% +?\\(\\(end\\)?\\(extends\\|for\\|cache\\|cycle\\|filter\\|firstof\\|debug\\|if\\(changed\\|equal\\|notequal\\|\\)\\|include\\|load\\|now\\|regroup\\|spaceless\\|ssi\\|templatetag\\|widthratio\\|block\\|trans\\)\\) ?.*? ?%}" . 1)
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
  (set (make-local-variable 'indent-line-function) 'pony-indent)
   (pony-load-snippets))

;; pony-tpl-minor-mode ends